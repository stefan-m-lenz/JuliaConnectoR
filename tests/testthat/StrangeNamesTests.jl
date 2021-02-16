module StrangeNamesTest1
   export σ, ∇a
   σ(x) = 2*x
   *ₛ(x,y) = x*y
   const ∇a = Vector{Vector{Float64}}
   🚗(x) = "car" * x
end


module StrangeNamesTest2
   export σ
   σ(x) = 2*x
end


module StrangeNamesTest3
   σ(x) = 2*x
end


module StrangeNamesTest4
   export harmless1, harmless2, 🚗, 🚗🚗
   σ(x) = 2*x
   harmless1(x) = 17
   harmless2(x) = 17
   harmless3(x) = 17
   🚗(x) = "car" * x
   🚗🚗(x) = "carcar" * x
end