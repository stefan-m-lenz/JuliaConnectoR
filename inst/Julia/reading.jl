struct Fail
   message::String
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
   fun::Union{Function, DataType, Module}
   args::ElementList
   parsingfails::Vector{Fail}

   function Call(fun::Union{Function, DataType} = () -> nothing,
      args::ElementList = ElementList(),
      parsingfails::Vector{Fail} = Vector{Fail}())

      new(fun, args, parsingfails)
   end
end


""" Accepts everything, does nothing """
function emptyfun(args...; kwargs...)
end


function read_attributes(inputstream)
   nattributes = read_nattributes(inputstream)
   attributes = Dict{String, Any}()
   for i in 1:nattributes
      name = read_string(inputstream)
      attributes[name] = read_element(inputstream, Vector{Function}())
   end
   attributes
end


function read_complexs(inputstream, n::Int)
   doublepairs = reinterpret(Float64, read_bin(inputstream, 16*n))
   map(i -> Complex{Float64}(doublepairs[2*i - 1], doublepairs[2*i]), 1:n)
end


function read_float64s(inputstream, n::Int)
   reinterpret(Float64, read_bin(inputstream, 8*n))
end


function read_ints(inputstream, n::Int)
   map(Int, reinterpret(Int32, read_bin(inputstream, 4*n)))
end

read_int(inputstream) = read_ints(inputstream, 1)[1]


function read_nattributes(inputstream)
   ret = read_bin(inputstream, 1)[1]
end


function read_string(inputstream)
   nbytes = read_int(inputstream)
   if nbytes > 0
      return String(read_bin(inputstream, nbytes))
   else
      return ""
   end
end

function read_strings(inputstream, n::Int)
   ret = Vector{String}(undef, n)
   for i = 1:n
      ret[i] = read_string(inputstream)
   end
   ret
end


function read_bools(inputstream, n::Int)
   reinterpret(Bool, read_bin(inputstream, n))
end


function read_dimensions(inputstream)
   ndims = read_int(inputstream)
   if ndims == 0
      return 0
   else
      return read_ints(inputstream, ndims)
   end
end


function read_element(inputstream, callbacks::Vector{Function})
   typeid = read_bin(inputstream, UInt8)

   if typeid == TYPE_ID_LIST
      return read_list(inputstream, callbacks)
   elseif typeid == TYPE_ID_NOTHING
      return nothing
   elseif typeid == TYPE_ID_EXPRESSION
      return read_expression(inputstream)
   elseif  typeid == TYPE_ID_CALLBACK
      callbackid = read_int(inputstream)
      if callbackid == 0
         return emptyfun
      else
         callback = callbackfun(callbackid, inputstream)
         push!(callbacks, callback)
         return callback
      end
   else
      dimensions = read_dimensions(inputstream)
      nelements = dimensions == 0 ? 1 : reduce(*, dimensions)

      attributes = NO_ATTRIBUTES
      if typeid == TYPE_ID_FLOAT64
         ret = read_float64s(inputstream, nelements)
         attributes = read_attributes(inputstream)
      elseif typeid == TYPE_ID_INT
         ret = read_ints(inputstream, nelements)
         attributes = read_attributes(inputstream)
      elseif typeid == TYPE_ID_BOOL
         ret = read_bools(inputstream, nelements)
         return reshape_element(ret, dimensions)
      elseif typeid == TYPE_ID_STRING
         ret = read_strings(inputstream, nelements)
         attributes = read_attributes(inputstream)
      elseif typeid == TYPE_ID_COMPLEX
         ret = read_complexs(inputstream, nelements)
         attributes = read_attributes(inputstream)
      elseif typeid == TYPE_ID_RAW
         ret = read_bin(inputstream, nelements)
         attributes = read_attributes(inputstream)
      else
         return Fail("Invalid type id $typeid of element")
      end

      return convert_reshape_element(ret, attributes, dimensions)
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
         if type <: SEND_AS_RAW_TYPES
            element = reinterpret(type, element)
            # adjust dimensions, as the original dimensions are for a UInt8 array
            if length(element) == 1 && isempty(get(attributes, "JLDIM", Int[]))
               dimensions = 0
            else
               dimensions[1] /= sizeof(type)
            end
         end
         element = convert.(type, element)
      catch ex
         return Fail("Conversion to type \"$typestr\" failed. Original error: $ex")
      end
   end
   return reshape_element(element, dimensions)
end


function read_expression(inputstream)
   exprstr = read_string(inputstream)
   try
      return maineval(exprstr)
   catch ex
      return Fail("Evaluation of \"$exprstr\" failed. Original error $ex")
   end
end


function read_list(inputstream, callbacks::Vector{Function})

   npositional = read_int(inputstream)
   positionalelements = Vector{Any}(undef, npositional)
   for i in 1:npositional
      positionalelements[i] = read_element(inputstream, callbacks)
   end

   fails = Vector{Fail}(
         positionalelements[isa.(positionalelements, Fail)])

   nnamed = read_int(inputstream)
   names = Vector{Symbol}(undef, nnamed)
   namedelements = Dict{Symbol, Any}()
   for i in 1:nnamed
      name = read_string(inputstream)
      namedelement = read_element(inputstream, callbacks)
      try
         sym = Symbol(name)
         namedelements[sym] = namedelement
         names[i] = sym
      catch ex
         push!(fails,
               Fail("Ignored element with name $name because of: $ex"))
      end
   end

   attributes = read_attributes(inputstream)

   ElementList(positionalelements, names, namedelements, attributes, fails)
end


function findfield(name::AbstractString)
   # The use of eval here brings a potential security risk if one wants
   # to restrict the functions that can be called somehow
   maineval(name)::Union{Function, DataType, Module}
end


function read_call(inputstream, callbacks::Vector{Function})
   name = read_string(inputstream)
   fails = Vector{Fail}()
   fun = () -> nothing
   try
      fun = findfield(name)
   catch ex
      push!(fails, Fail("Unable to identify function: $ex"))
   end
   args = read_list(inputstream, callbacks)
   Call(fun, args, fails)
end