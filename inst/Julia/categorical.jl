# This file is included at server startup, but CategoricalArrays is optional.
# Keep package-dependent signatures and top-level imports out of this file so
# all existing connector functionality works without CategoricalArrays installed.
function categorical_from_factor(codes::AbstractVector,
                                 factor_levels::AbstractVector{String},
                                 ordered::Bool)
   if !isdefined(@__MODULE__, :CategoricalArrays)
      try
         @eval import CategoricalArrays
      catch ex
         # Preserve the original diagnostic (e.g. a dependency or precompile
         # failure), while explaining how to install the optional dependency.
         error("juliaCategorical() requires CategoricalArrays.jl in the active " *
               "Julia environment. Install it with " *
               "juliaEval('import Pkg; Pkg.add(\"CategoricalArrays\")').\n" *
               sprint(showerror, ex))
      end
   end

   # Importing during a running call introduces methods in a newer world age.
   # Enter the builder through invokelatest so the very first conversion can use
   # those methods, including on the older Julia versions supported by RConnector.
   Base.invokelatest(build_categorical_from_factor, codes, factor_levels, ordered)
end

function build_categorical_from_factor(codes, factor_levels, ordered)
   # An explicit union element type also covers empty and all-missing factors.
   # Missing codes were already translated from R NA by the wire protocol.
   values = Vector{Union{Missing, String}}(undef, length(codes))
   for i in eachindex(codes)
      code = codes[i]
      values[i] = ismissing(code) ? missing : factor_levels[code]
   end
   result = CategoricalArrays.CategoricalArray{Union{Missing, String}, 1}(values)
   # Use public APIs rather than accessing the package's internal reference pool.
   # Setting levels explicitly retains unused levels and the R factor's order.
   CategoricalArrays.levels!(result, collect(factor_levels))
   CategoricalArrays.ordered!(result, ordered)
   result
end
