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
   evaluate!(call.args)

   # the actual function call
   result = Fail("")
   #println(call)
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
            # composite type
            return Base.invokelatest(constructor,
                  [list.namedelements[name] for name in list.names]...)
         else
            if constructor <: Union{Pair, Tuple}
               return Base.invokelatest(constructor, list.positionalelements...)
            else
               # array type
               return Base.invokelatest(constructor, 
                     collect(list.positionalelements))
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

function evaluate!(item)
   item
end


""" Evaluates Julia code, passed as String, in the Main module """
function maineval(str::String)
   Main.eval(Meta.parse(str))
end

