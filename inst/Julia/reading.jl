struct Fail
   message::String
end

function Fail(msg, origex)
   Fail(msg * "\nOriginal error:\n" * sprint(showerror, origex, backtrace()))
end


struct ElementList
   positionalelements::Vector{Any}
   names::Vector{Symbol}
   namedelements::Dict{Symbol, Any}
   attributes::Dict{String, Any}
   parsingfails::Vector{Fail}
end

function ElementList(unnamed::Vector{Any} = Vector{Any}(),
      names::Vector{Symbol} = Vector{Symbol}(),
      named::Dict{Symbol, Any} = Dict{Symbol, Any}(),
      attributes = Dict{String, Any}())
   ElementList(unnamed, names, named, attributes, Vector{Fail}())
end

function ElementList(unnamed::Vector{Any},
      named::Dict{Symbol, Any} = Dict{Symbol, Any}(),
      attributes = Dict{String, Any}())

   ElementList(unnamed, collect(keys(named)), named, attributes, Vector{Fail}())
end


struct Call
   fun::Union{Function, Type, Module}
   args::ElementList
   parsingfails::Vector{Fail}

   function Call(fun::Union{Function, Type} = () -> nothing,
      args::ElementList = ElementList(),
      parsingfails::Vector{Fail} = Vector{Fail}())

      new(fun, args, parsingfails)
   end
end


function read_attributes(communicator)
   nattributes = read_nattributes(communicator)
   attributes = Dict{String, Any}()
   for i in 1:nattributes
      name = read_string(communicator)
      attributes[name] = read_element(communicator)
   end
   attributes
end


function read_complexs(communicator, n::Int)
   doublepairs = reinterpret(Float64, read_bin(communicator, 16*n))
   map(i -> Complex{Float64}(doublepairs[2*i - 1], doublepairs[2*i]), 1:n)
end

function read_complexs_with_missings(communicator, n::Int)
   doublepairs = reinterpret(Float64, read_bin(communicator, 16*n))
   ret = map(i -> Complex{Float64}(doublepairs[2*i - 1], doublepairs[2*i]), 1:n)
   if any(isequal(R_NA_COMPLEX), ret)
      return replace(ret, R_NA_COMPLEX => missing)
   else
      return ret
   end
end


function read_float64s(communicator, n::Int)
   reinterpret(Float64, read_bin(communicator, 8*n))
end


function read_float64s_with_missings(communicator, n::Int)
   ret = read_float64s(communicator, n)
   if any(isequal(R_NA_REAL), ret)
      return replace(ret, R_NA_REAL => missing)
   else
      return ret
   end
end


function read_ints(communicator, n::Int)
   map(Int, reinterpret(Int32, read_bin(communicator, 4*n)))
end

read_int(communicator) = read_ints(communicator, 1)[1]


function read_ints_with_missings(communicator, n::Int)
   ret = read_ints(communicator, n)
   if any(isequal(R_NA_INTEGER), ret)
      return replace(ret, R_NA_INTEGER => missing)
   else
      return ret
   end
end


function read_nattributes(communicator)
   ret = read_bin(communicator, 1)[1]
end


function read_string(communicator)
   nbytes = read_int(communicator)
   if nbytes > 0
      return String(read_bin(communicator, nbytes))
   else
      return ""
   end
end

function read_strings(communicator, n::Int)
   ret = Vector{String}(undef, n)
   for i = 1:n
      ret[i] = read_string(communicator)
   end
   ret
end


function read_bools(communicator, n::Int)
   reinterpret(Bool, read_bin(communicator, n))
end


function read_dimensions(communicator)
   ndims = read_int(communicator)
   if ndims == 0
      return 0
   else
      return read_ints(communicator, ndims)
   end
end


function read_element(communicator)
   typeid = read_bin(communicator, UInt8)

   if typeid == TYPE_ID_LIST
      return read_list(communicator)
   elseif typeid == TYPE_ID_NOTHING
      return nothing
   elseif typeid == TYPE_ID_EXPRESSION
      return read_expression(communicator)
   elseif typeid == TYPE_ID_OBJECT_REFERENCE
      # not needed in Julia, ignore for now
      object_class = read_bin(communicator, 1)
      ref = parseheapref(read_bin(communicator, 8))
      return ObjectReference(ref)
   elseif typeid == TYPE_ID_CALLBACK
      callbackid = read_string(communicator)
      callback = callbackfun(callbackid, communicator)
      return callback
   elseif typeid == TYPE_ID_SYMBOL
      return Symbol(read_string(communicator))
   else
      dimensions = read_dimensions(communicator)
      nelements = dimensions == 0 ? 1 : reduce(*, dimensions)

      attributes = NO_ATTRIBUTES
      if typeid == TYPE_ID_FLOAT64
         ret = read_float64s_with_missings(communicator, nelements)
         attributes = read_attributes(communicator)
         return convert_reshape_element(ret, attributes, dimensions)
      elseif typeid == TYPE_ID_INT
         ret = read_ints_with_missings(communicator, nelements)
         attributes = read_attributes(communicator)
         return convert_reshape_element(ret, attributes, dimensions)
      elseif typeid == TYPE_ID_BOOL
         ret = read_bools(communicator, nelements)
         return reshape_element(ret, dimensions)
      elseif typeid == TYPE_ID_STRING
         ret = read_strings(communicator, nelements)
         attributes = read_attributes(communicator)
         return convert_reshape_string(ret, attributes, dimensions)
      elseif typeid == TYPE_ID_COMPLEX
         ret = read_complexs_with_missings(communicator, nelements)
         attributes = read_attributes(communicator)
         return convert_reshape_element(ret, attributes, dimensions)
      elseif typeid == TYPE_ID_RAW
         ret = read_bin(communicator, nelements)
         attributes = read_attributes(communicator)
         return convert_reshape_raw(ret, attributes, dimensions)
      else
         return Fail("Invalid type id $typeid of element")
      end
   end
end


function reshape_element(element, dimensions)
   if dimensions == 0
      return element[1]
   else # dimensions > 0
      return Array(reshape(element, dimensions...))
   end
end


function convert_reshape_element(element, attributes, dimensions)
   typestr = get(attributes, "JLTYPE", "")
   if typestr != ""
      try
         type = maineval(typestr)
         element = convert.(type, element)
      catch ex
         return Fail("Conversion to type \"$typestr\" failed", ex)
      end
   end
   return reshape_element(element, dimensions)
end


function convert_reshape_raw(element, attributes, dimensions)
   typestr = get(attributes, "JLTYPE", "")
   if typestr != ""
      try
         type = maineval(typestr)
         if type <: SEND_AS_RAW_TYPES
            element = reinterpret(type, element)
            # adjust dimensions, as the original dimensions are for a UInt8 array
            if length(element) == 1 && isempty(get(attributes, "JLDIM", Int[]))
               dimensions = 0
            else
               dimensions[1] /= sizeof(type)
            end
            element = convert.(type, element)
         elseif type <: String
            # It may be the case that a Julia String can not be handled in R,
            # e.g. if it contains NUL characters. Then it will be encoded in bytes.
            return String(element)
         end
      catch ex
         return Fail("Conversion to type \"$typestr\" failed", ex)
      end
   end
   return reshape_element(element, dimensions)
end


function convert_reshape_string(ret::Array{String}, attributes, dimensions)
   if haskey(attributes, "NA")
      ret = convert(Array{Union{String, Missing}}, ret)
      try
         for i in attributes["NA"]
            ret[i] = missing
         end
      catch ex
         return Fail("Translating NAs failed", ex)
      end
   end
   return convert_reshape_element(ret, attributes, dimensions)
end


function read_expression(communicator)
   exprstr = read_string(communicator)
   try
      return maineval(exprstr)
   catch ex
      return Fail("Evaluation of \"$exprstr\" failed", ex)
   end
end


function read_list(communicator)

   npositional = read_int(communicator)
   positionalelements = Vector{Any}(undef, npositional)
   for i in 1:npositional
      positionalelements[i] = read_element(communicator)
   end

   fails = Vector{Fail}(
         positionalelements[isa.(positionalelements, Fail)])

   nnamed = read_int(communicator)
   names = Vector{Symbol}(undef, nnamed)
   namedelements = Dict{Symbol, Any}()
   for i in 1:nnamed
      name = read_string(communicator)
      namedelement = read_element(communicator)
      try
         sym = Symbol(name)
         namedelements[sym] = namedelement
         names[i] = sym
      catch ex
         push!(fails, Fail("Ignored element with name $name", ex))
      end
   end

   attributes = read_attributes(communicator)

   ElementList(positionalelements, names, namedelements, attributes, fails)
end


function findfield(name::AbstractString)
   # The use of eval here brings a potential security risk if one wants
   # to restrict the functions that can be called somehow
   maineval(name)::Union{Function, Type, Module}
end


# return a function that is the broadcasted version of f,
# corresponding to adding a dot in the call, as
# f.(args...) is actually equivalent to broadcast(f, args...)
# (see Julia documentation of dot syntax for vectorizing functions)
function	broadcasted(f::Function)
   ((args...) -> broadcast(f, args...))
end

function broadcasted(call::Call)
   Call(broadcasted(call.fun), call.args, call.parsingfails)
end


function read_call(communicator)
   name = read_string(communicator)
   if endswith(name, ".")
      funname = name[1:(end-1)]
      return broadcasted(read_call(communicator, funname))
   else
      return read_call(communicator, name)
   end
end

function read_call(communicator, name::AbstractString)
   fails = Vector{Fail}()
   fun = () -> nothing
   try
      fun = findfield(name)
   catch ex
      push!(fails, Fail("Unable to identify function", ex))
   end
   args = read_list(communicator)
   Call(fun, args, fails)
end