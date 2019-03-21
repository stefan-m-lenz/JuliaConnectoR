# JuliaConnector

This package provides a functionally oriented interface between R and Julia.
The goal is to use functions from Julia packages directly in R.
Functions imported with the `JuliaConnector` can accept and return R variables.
It is also possible to pass function arguments to enable callbacks from Julia to R.

The data structures passed to and returned from Julia are serialized to a custom streaming format, sent via TCP and translated to Julia data structures. Returned results are likewise translated back to R. R functions can be passed as arguments and will be invoked by Julia in place of Julia functions.

The package requires that Julia (Version &ge; 0.7) is installed and that the Julia executable is in the system search `PATH` or that the `JULIA_BINDIR` environment variable is set to the `bin` directory of the Julia installation.

Sine Julia is more type-sensitive than R, it is important to know the translations of the data structures that are shown in the following table:

| R                                  | Julia |
|------------------------------------|-------|
| `vector` of length 1 of type (`typeof`)<br />&bull; `integer`<br />&bull; `double` <br />&bull; `logical` <br />&bull; `character` <br />&bull; `complex` <br />&bull; `raw`| <br />&bull; `Int` <br />&bull; `Float64` <br />&bull; `Bool` <br />&bull; `String` <br />&bull; `Complex{Float64}`<br />&bull; `UInt8` |
| `vector` of length > 1 (N = 1)  or <br /> `array` with N dimensions (`dim`) of type <br />&bull; `integer`<br />&bull; `double` <br />&bull;  `logical` <br />&bull; `character` <br />&bull; `complex` <br />&bull; `raw`| <br /><br />&bull; `Array{Int, N}` <br />&bull; `Array{Float64, N}` <br />&bull; `Array{Bool, N}`<br />&bull; `Array{String, N}` <br />&bull; `Array{Complex{Float64}, N}`<br />&bull; `Array{UInt8, N}` |
| R function (type `closure`) | Julia function that will call the given R function |
| `list` with attribute `"JLDATATYPE"` | Julia object of the data type specified in the attribute. The constructor is called with the elements of the list in the given order. |
| `list` without attribute `"JLDATATYPE"` | `Vector{T}` where `T` is the most specific supertype of the list elements after translation to Julia |
| Julia code as one-element character vector with attribute `"JLEXPR"` | Evaluation of the expression |
