import Pkg
if VERSION < v"1.4"
   if get(Pkg.installed(), "Tables", v"0.0") < v"1.0"
      include("./dummy_tables.jl")
      @eval import .Tables
   else
      @eval import Tables
   end
else
   # no check for correct version of Tables in Julia 1.4 since
   # Pkg.dependencies() is "experimental" and Pkg.installed is deprecated
   try
      @eval import Tables
   catch ex
      include("./dummy_tables.jl")
      @eval import .Tables
   end
end

using InteractiveUtils # for convenience: this is loaded in the standard REPL as well

import Random

include("./RConnector.jl")

# Clients must authenticate with this token, so that only processes that can
# read the port file (i.e., processes of the same user) can use this Julia.
token = bytes2hex(rand(Random.RandomDevice(), UInt8, 32))

RConnector.serve(parse(Int, ARGS[1]), portfile = ARGS[2],
   multiclient = (ARGS[3] == "t"), token = token)
