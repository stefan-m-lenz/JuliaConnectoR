import Pkg
if get(Pkg.installed(), "Tables", v"0.0") < v"1.0"
   include("./dummy_tables.jl")
end

import Tables

include("./RConnector.jl")
RConnector.serve(parse(Int, ARGS[1]), portfile = ARGS[2])
