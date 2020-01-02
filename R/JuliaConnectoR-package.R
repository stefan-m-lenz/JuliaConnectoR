#' A Functionally Oriented Interface for Integrating Julia with R
#'
#' This package provides a functionally oriented interface between R and Julia.
#' The goal is to call functions from Julia packages directly as R functions.
#'
#' Julia functions imported via the \emph{JuliaConnectoR} can accept and return R variables.
#' The data structures passed to and returned from Julia are serialized,
#' sent via TCP and translated to Julia data structures.
#' Returned results are translated back to R.
#' Even complex Julia data structures are translated to R in a way that
#' they can be translated back and passed to Julia again.
#'
#' It is also possible to pass function arguments to enable \emph{callbacks} from Julia to R.
#' R functions can be passed as arguments and will be invoked by
#' Julia in place of Julia functions.
#'
#'
#' @section Setup:
#' The package requires that
#' \href{https://julialang.org/downloads/}{Julia (Version \eqn{\geq}{>=} 0.7) is installed}
#' and that the Julia executable is in the system search \code{PATH} or that the
#' \code{JULIA_BINDIR} environment variable is set to the \code{bin} directory of
#' the Julia installation.
#'
#
#' @section Translation from Julia to R and vice versa:
#' From a technical perspective, R data structures are serialized with an
#' optimized custom streaming format,
#' sent to a (local) Julia TCP server, and translated to Julia data structures by Julia.
#' The results of function calls are likewise translated back to R.
#'
#' Since Julia is more type-sensitive than R,
#' it is important to know the translations of the data structures.
#'
#' The translations from Julia to R are shown in the following table:
#'
#' \tabular{ll}{
#' \strong{bla} \tab \strong{bla}\cr
#' \itemize{ \item blup \item z} \tab blup \cr}
#'
#' @md
#' @docType package
#' @name JuliaConnectoR-package
NULL

#'  `vector` of length 1 of type (`typeof`)
#'  \itemize{
#'   \item `integer`
#'   \item `double`
#' }
#' \tab test \cr


#'   } <br />&bull; `logical` <br />&bull; `character` <br />&bull; `complex` <br />&bull; `raw`| <br />&bull; `Int` <br />&bull; `Float64` <br />&bull; `Bool` <br />&bull; `String` <br />&bull; `Complex{Float64}`<br />&bull; `UInt8` |
#'   | `vector` of length > 1 (N = 1)  or <br /> `array` with N dimensions (`dim`) of type <br />&bull; `integer`<br />&bull; `double` <br />&bull;  `logical` <br />&bull; `character` <br />&bull; `complex` <br />&bull; `raw`| <br /><br />&bull; `Array{Int, N}` <br />&bull; `Array{Float64, N}` <br />&bull; `Array{Bool, N}`<br />&bull; `Array{String, N}` <br />&bull; `Array{Complex{Float64}, N}`<br />&bull; `Array{UInt8, N}` |
#'   | R function (type `closure`) | Julia function that will call the given R function |
#'   | `list` with attribute `"JLTYPE"` | Julia object of the data type specified in the attribute. The constructor is called with the elements of the list in the given order. |
#'   | `list` without attribute `"JLTYPE"` | `Vector{T}` where `T` is the most specific supertype of the list elements after translation to Julia |
#'   | Julia code as one-element character vector with attribute `"JLEXPR"` | Evaluation of the expression |
