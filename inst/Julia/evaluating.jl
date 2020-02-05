function collectfails(call::Call)
   vcat(call.parsingfails, collectfails(call.args))
end

function collectfails(ellist::ElementList)
   ellist.parsingfails
end

function collectfails(el)
   Fail[]
end

function collectfails(fail::Fail)
   fail
end

"""
Converts a vector to a vector of the most specific type that all
elements share as common supertype.
"""
function converttomostspecifictype(v::Vector)
   if isempty(v)
      return v
   else
      Vector{mostspecifictype(v)}(v)
   end
end


"""
    mostspecifictype(v)
Returns the most specific supertype for all elements in the vector `v`.
"""
function mostspecifictype(v::Vector)
   mapreduce(typeof, typejoin, v)
end


function evaluate!(call::Call)
   # check parsing first
   fails = collectfails(call)
   if !isempty(fails)
      return Fail("Parsing failed. Reason: " * string(fails))
   end

   # evaluate arguments
   evaluate!(call.args)

   # the actual function call
   result = Fail("")
   try
      result = Base.invokelatest(call.fun,
            call.args.positionalelements...; call.args.namedelements...)
   catch ex
      result = Fail("Evaluation failed. Original error: $ex")
   end
   result
end

function evaluate!(list::ElementList)
   # recursively evaluate sub-elements first
   for i in eachindex(list.positionalelements)
      list.positionalelements[i] = evaluate!(list.positionalelements[i])
   end
   for name in list.names
      list.namedelements[name] = evaluate!(list.namedelements[name])
   end

   jltype = get(list.attributes, "JLTYPE", "")
   if !isempty(jltype)
      try
         constructor = findfield(jltype)
         if length(list.names) > 0
            if constructor <: AbstractDict
               return Base.invokelatest(constructor, zip(
                     list.namedelements[:keys],
                     list.namedelements[:values]))
            elseif constructor <: NamedTuple
               return NamedTuple{Tuple(list.names)}(
                     [list.namedelements[name] for name in list.names])
            elseif constructor <: Symbol
               return Symbol(list.namedelements[:name])
            elseif constructor <: Module
               return Module(Symbol(list.namedelements[:name]))
            elseif constructor <: CircularReference
               error("Circular references cannot be reconstructed")
            else
               return reconstruct_struct(constructor, list)
            end
         else
            if constructor <: Tuple
               return Base.invokelatest(constructor, list.positionalelements)
            else
               # array type (or AbstractSet)
               return reconstruct_array(constructor, list.positionalelements,
                     list.attributes)
            end
         end
      catch ex
         return Fail("Construction of type $jltype failed. Original error: $ex")
      end
   elseif isempty(list.namedelements)
      return converttomostspecifictype(list.positionalelements)
   else
      return list
   end
end


function evaluate!(objref::ImmutableObjectReferece)
   objref.obj
end


function evaluate!(item)
   item
end


""" Evaluates Julia code, passed as String, in the Main module """
function maineval(str::AbstractString)
   Main.eval(Meta.parse(str))
end


function mainevalcmd(str::String)
   strippedstr = strip(str)
   ret = include_string(Main, str)
   if strippedstr[end] == ';'
      return nothing
   else
      return ret
   end
end


"""
    mainevallet(str; ...)
Evalutes the string as the body of a let expression with the given
keyword arguments as variables in the let expression.
"""
function mainevallet(str::String; kwargs...)
   # Enclose str with a let block containing the kwargs

   # prevent the creation of a toplevel-Expression in Meta.parse
   str = "begin " * str * " end"

   if !isempty(kwargs)
      e = Expr(:let,
            Expr(:block,
                  [Expr(:(=), key, value) for (key, value) in kwargs]...),
            Expr(:block, Meta.parse(str)))
   else
      e = Expr(:let, Expr(:block), Expr(:block, Meta.parse(str)))
   end
   Main.eval(e)
end


function reconstruct_array(constructor, arr::Vector, attributes)
   if haskey(attributes, "JLDIM")
      return reconstruct_multidimensional_array(constructor,
            arr, attributes["JLDIM"])
   elseif any(isequal(JLUNDEF()), arr)
      ret = Base.invokelatest(constructor, undef, size(arr))
      copy_defined!(ret, arr)
      return ret
   else
      return Base.invokelatest(constructor, arr)
   end
end


function reconstruct_multidimensional_array(constructor, arr::Vector,
         dims::Vector{Int})

   if prod(dims) != length(arr)
      error("Incorrect dimensions of array. Dimensions are" *
            " $dims but number of elements is $(length(arr)).\n" *
            " If the array has been modified, the attribute \"JLDIM\"" *
            " (and perhaps the attribute \"JLTYPE\") must be set accordingly.")
   else
      ret = Base.invokelatest(constructor, undef, dims...)
      copy_defined!(ret, arr)
      return ret
   end
end


function reconstruct_struct(constructor, list::ElementList)
   # normal composite type:
   # forge call to inner constructor (which may be private)
   constructorargs = [list.namedelements[name] for name in list.names]
   obj = Main.eval(Expr(:new, constructor, [:($arg) for arg in constructorargs]...))

   # The reconstructed object may depend on a pre-existing copy and its resources.
   # This is referenced by the "JLREF" attribute.
   jlref::UInt64 = get(list.attributes, "JLREF", UInt64(0))
   if jlref != 0
      sharedfinalizer(obj, jlref)
   end

   return obj
end


function getprop(value, name::String)
   getproperty(value, Symbol(name))
end

function setprop!(value, name::String, x)
   setproperty!(value, Symbol(name), x)
end