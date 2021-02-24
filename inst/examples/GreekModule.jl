module GreekModule
   σ(x) = 1 ./ (1 + exp.(-x))
   logσ(x) = - log1p.(exp.(-x))
end
