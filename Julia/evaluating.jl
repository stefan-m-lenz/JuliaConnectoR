function evaluate!(call::FunctionCall)
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
            # array type
            return Base.invokelatest(constructor, collect(list.positionalelements))
         end
      catch ex
         return Fail("Construction of type $jltype failed. Original error: $ex")
      end
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

