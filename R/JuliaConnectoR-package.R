#' A Functionally Oriented Interface for Integrating Julia with R
#'
#' This package provides a functionally oriented interface between R and Julia.
#' The goal is to call functions from Julia packages directly as R functions.
#'
#' Julia functions imported via the \pkg{JuliaConnectoR} can accept and return R variables.
#' The data structures passed to and returned from Julia are serialized,
#' sent via TCP and translated to Julia data structures.
#' Returned results are translated back to R.
#' Even complex Julia data structures are translated to R in a way that
#' they can be translated back and passed to Julia again.
#'
#' It is also possible to pass function arguments to enable
#' \emph{callbacks} from Julia to R:
#' R functions can be passed as arguments and will be invoked by
#' Julia in place of Julia functions.
#'
#'
#' @section Setup:
#' The package requires that
#' \href{https://julialang.org/downloads/}{Julia (Version \eqn{\geq}{>=} 1.0) is installed}
#' and that the Julia executable is in the system search \env{PATH} or that the
#' \env{JULIA_BINDIR} environment variable is set to the \code{bin} directory of
#' the Julia installation.
#'
#'
#' @section Function overview:
#' The functions \code{\link{juliaImport}} and \code{\link{juliaUsing}}
#' can be used to make functions and types of entire packages directly
#' available in R.
#'
#' If only a single Julia function needs to be importedR, \code{\link{juliaFun}}
#' can do this. The simplest way to call a Julia function without any importing
#' is to use \code{\link{juliaCall}} with the function name given
#' as character string.
#'
#' For evaluating expressions in Julia, \code{\link{juliaEval}} and
#' \code{\link{juliaLet}} can be used. With \code{\link{juliaLet}} one can use
#' R variables in a expression.
#'
#' \code{\link{juliaExpr}} makes it possible to refer to a Julia object
#' with a string in R.
#
#' @section Translation:
#' From a technical perspective, R data structures are serialized with an
#' optimized, custom streaming format,
#' sent to a (local) Julia TCP server, and translated to Julia data structures by Julia.
#' The results of function calls are likewise translated back to R.
#'
#' Since Julia is more type-sensitive than R,
#' it is important to know the translations of the data structures.
#'
#' \subsection{Translation from R to Julia}{
#' The type correspondences of the basic R data types in Julia are the following:
#'
#' \tabular{lcl}{
#' \strong{R} \tab  \tab \strong{Julia}\cr
#' \code{integer} \tab \eqn{\rightarrow}{-->} \tab \code{Int} \cr
#' \code{double}  \tab \eqn{\rightarrow}{-->} \tab \code{Float64} \cr
#' \code{logical}   \tab \eqn{\rightarrow}{-->} \tab \code{Bool} \cr
#' \code{character} \tab \eqn{\rightarrow}{-->} \tab \code{String} \cr
#' \code{complex} \tab \eqn{\rightarrow}{-->} \tab \code{Complex{Float64}} \cr
#' \code{raw}  \tab \eqn{\rightarrow}{-->} \tab \code{UInt8} \cr
#' }
#'
#' R vectors of length 1 of the types in the table above will be translated to the types shown.
#'
#' R vectors or arrays with more than one element will be translated to Julia \code{Array}s
#' of the corresponding types. The dimensions of an R array, as returned by \code{dim()},
#' will also be respected.
#' For example, the R integer vector \code{c(1L, 2L)} will be of type \code{Vector{Int}},
#' or \code{Array{Int,1}}, in Julia.
#' A double matrix such as \code{matrix(c(1,2,3,4), nrow = 2)}
#' will be of type \code{Array{Float64,2}}.
#'
#'
#' R lists are translated as \code{Vector{T}} in Julia, with \code{T} being
#' the most specific supertype of the list elements after translation to Julia.
#'
#' An R function (type \code{closure}) that is handed to Julia as argument in a function
#' call is translated to a Julia callback function that will call the given R function.
#'
#' Strings with attribute \code{"JLEXPR"}
#' will be evaluated as Julia expressions,
#' and the value is used in their place (see \code{\link{juliaExpr}}).
#'
#' }
#'
#' \subsection{Translation from Julia to R}{
#' The type system of Julia is richer than that of R. Therefore, to be able to turn
#' the Julia data structures that have been translated to R back to the original Julia
#' data structures, the original Julia types are added to the translated Julia objects
#' in R via the attribute \code{"JLTYPE"}.
#' When passed to Julia, R variables with this
#' attribute will be coerced to the respective type.
#' This allows the reconstruction of the objects
#' with their original type.
#'
#' It should not be necessary to worry too much
#' about the translations from Julia to R because the resulting R objects should be
#' intuitive to handle.
#'
#' The following table shows how the primitive types
#' of Julia are translated to R:
#' \tabular{lcl}{
#' \strong{Julia} \tab  \tab \strong{R} \cr
#'  \code{Float64}\tab \eqn{\rightarrow}{-->} \tab\code{double} \cr
#'  \code{Float16}, \code{Float32}, \code{UInt32} \tab \eqn{\rightarrow}{-->} \tab\code{double} with type attribute \cr
#'  \code{Int64} that fits in 32 bits \tab \eqn{\rightarrow}{-->} \tab \code{integer} \cr
#'  \code{Int64} not fitting in 32 bits \tab \eqn{\rightarrow}{-->} \tab \code{double} with type attribute \cr
#'  \code{Int8}, \code{Int16}, \code{UInt16}, \code{Int32}, \code{Char} \tab \eqn{\rightarrow}{-->} \tab\code{integer} with type attribute \cr
#'  \code{UInt8}\tab \eqn{\rightarrow}{-->} \tab\code{raw} \cr
#'  \code{UInt64}, \code{Int128}, \code{UInt128}, \code{Ptr} \tab \eqn{\rightarrow}{-->} \tab\code{raw} with type attribute \cr
#'  \code{Complex{Float64}}\tab \eqn{\rightarrow}{-->} \tab\code{complex} \cr
#'  \code{Complex{Int\var{X}}} with \var{X} \eqn{\leq}{<=} 64 \tab \eqn{\rightarrow}{-->} \tab\code{complex} with type attribute \cr
#'  \code{Complex{Float\var{X}}} with \var{X} \eqn{\leq}{<=} 32 \tab \eqn{\rightarrow}{-->} \tab\code{complex} with type attribute \cr
#' }
#'
#' Non-primitive objects are translated as follows:
#' \tabular{lcl}{
#' \strong{Julia} \tab  \tab \strong{R} \cr
#'  \code{Array} of primitive type \tab \eqn{\rightarrow}{-->} \tab \code{vector} or \code{array} of corresponding type \cr
#'  \code{struct} \tab \eqn{\rightarrow}{-->} \tab \code{list} with the named struct elements \cr
#'  \code{Array} of \code{struct} type \tab \eqn{\rightarrow}{-->} \tab \code{list} (of \code{list}s) \cr
#'  \code{AbstractDict} \tab \eqn{\rightarrow}{-->} \tab \code{list} with two sub-lists: "\code{keys}" and "\code{values}" \cr
#'  \code{AbstractSet} \tab \eqn{\rightarrow}{-->} \tab \code{list} \cr
#'  \code{Function} \tab \eqn{\rightarrow}{-->} \tab function that calls the Julia function \cr
#' }
#'
#' Julia functions that have been translated to R can also be translated back to the
#' corresponding Julia functions when used as argument of another function
#' (see \code{\link{juliaFun}}).
#'
#' It is safe to translate objects that contain external references from Julia to R.
#' The pointers will be copied as values and the finalization of the translated
#' Julia objects is prevented.
#' The original objects are garbage collected after all direct or
#' indirect copies are garbage collected.
#'
#' }
#'
#' @section Limitations:
#'
#' The current version does not translate data frames into a useful
#' format in Julia. Also, \code{NA}s in R are not translated to \code{missing}
#' values in Julia. The behaviour regarding data frames and missing values
#' can change in future versions of the package.
#'
#' Objects containing cicular references cannot be translated back to Julia.
#'
#' If Julia objects contain external references such as pointers,
#' they cannot be translated back to Julia after the Julia process
#' has been stopped and restarted.
#'
#' Numbers of type \code{Int64} that are too big to be expressed as 32-bit
#' \code{integer} values in R will be translated to \code{double} numbers.
#' This may lead to a inaccurate results for very large numbers,
#' when they are translated back to Julia, since, e. g.,
#' \code{(2^53 + 1) - 2^53 == 0} holds for double-precision
#' floating point numbers.
#'
#'
#' @docType package
#' @name JuliaConnectoR-package
NULL
