module Tables
   const JuliaConnectoR_DummyTables = true
   istable(x) = false
   columnaccess(x) = false
   columns(x) = nothing
   getcolumn(x) = nothing
   columnnames(x) = nothing
   abstract type AbstractColumns end
end