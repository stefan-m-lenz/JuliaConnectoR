""" Representing an #undef value """
struct JLUNDEF
end


""" 
Creates a new array that has `JLUNDEF()` objects in places of undefined values.
"""
function undefs_replaced(arr::AbstractArray)
   ret = Array{Any}(undef, size(arr))
   @inbounds for i in eachindex(arr)
      if isassigned(arr, i)
         ret[i] = arr[i]
      else
         ret[i] = JLUNDEF()
      end
   end
   ret
end


""" 
Copies all elements that are not `JLUNDEF()` from `src`
to the corresponding index in `dst` 
"""
function copy_defined!(dst::Array, src::Array)
   @inbounds for i in eachindex(src)
      if src[i] != JLUNDEF()
         dst[i] = src[i]
      end
   end
   dst
end
