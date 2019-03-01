include("./TcpCallR.jl")
using BoltzmannMachines
TcpCallR.serve(parse(Int, ARGS[1]))