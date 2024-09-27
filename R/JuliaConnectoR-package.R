#' A Functionally Oriented Interface for Integrating Julia with R
#'
#' This package provides a functionally oriented interface between R and Julia.
#' The goal is to call functions from Julia packages directly as R functions.
#'
#' This R-package provides a functionally oriented interface between R and Julia.
#' The goal is to call functions from Julia packages directly as R functions.
#' Julia functions imported via the \pkg{JuliaConnectoR} can accept and return R variables.
#' It is also possible to pass R functions as arguments in place of Julia functions,
#' which allows \emph{callbacks} from Julia to R.
#'
#
#' From a technical perspective, R data structures are serialized with an optimized custom streaming format,
#' sent to a (local) Julia TCP server, and translated to Julia data structures by Julia.
#' The results are returned back to R.
#' Simple objects, which correspond to vectors in R, are directly translated.
#' Complex Julia structures are by default transferred to R by reference via proxy objects.
#' This enables an effective and intuitive handling of the Julia objects via R.
#' It is also possible to fully translate Julia objects to R objects.
#' These translated objects are annotated with information
#' about the original Julia objects, such that they can be translated back to Julia.
#' This makes it also possible to serialize them as R objects.
#'
#'
#' @section Setup:
#' The package requires that
#' \href{https://julialang.org/downloads/}{Julia (Version \eqn{\geq}{>=} 1.0) is installed}
#' and that the Julia executable is in the system search \env{PATH} or that the
#' \env{JULIA_BINDIR} environment variable is set to the \code{bin} directory of
#' the Julia installation. For more details about the setup,
#' see \code{\link{Setup-JuliaConnectoR}}.
#'
#'
#' @section Function overview:
#' The function \code{\link{juliaImport}} makes
#' functions and data types from Julia packages or modules available as R functions.
#'
#' If only a single Julia function needs to be imported in R, \code{\link{juliaFun}}
#' can do this. The simplest way to call a Julia function without any importing
#' is to use \code{\link{juliaCall}} with the function name given
#' as character string.
#'
#' For evaluating expressions in Julia, \code{\link{juliaEval}} and
#' \code{\link{juliaLet}} can be used. With \code{\link{juliaLet}} one can use
#' R variables in a expression.
#'
#' \code{\link{juliaExpr}} makes it possible use complex Julia syntax in R via R strings
#' that contain Julia expressions.
#'
#' With \code{\link{juliaGet}}, a full translation of a Julia proxy object into an R object
#' is performed.
#'
#' \code{as.data.frame} is overloaded (\code{\link{as.data.frame.JuliaProxy}})
#' for translating Julia objects that implement the
#' \href{https://github.com/JuliaData/Tables.jl}{\code{Tables}} interface
#' to R data frames.
#
#' @section Translation:
#'
#' Since Julia is more type-sensitive than R, and many Julia functions expect to be called
#' using specific types, it is important to know the translations of the R data structures
#' to Julia.
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
#' \code{symbol} \tab \eqn{\rightarrow}{-->} \tab \code{Symbol} \cr
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
#' Missing values (\code{NA}) in R are translated to \code{missing} values in Julia.
#' R vectors and arrays with missing values are converted to Julia arrays
#' of type \code{Array{Union{Missing, T}}}, where \code{T} stands for the translated
#' type in the table above.
#'
#' R lists are translated as \code{Vector{T}} in Julia, with \code{T} being
#' the most specific supertype of the list elements after translation to Julia.
#'
#' An R function that is handed to Julia as argument in a function
#' call is translated to a Julia callback function that will call the given R function.
#'
#' Strings with attribute \code{"JLEXPR"}
#' will be evaluated as Julia expressions,
#' and the value is used in their place (see \code{\link{juliaExpr}}).
#'
#' R data frames are translated to objects that implement the Julia
#' \href{https://github.com/JuliaData/Tables.jl}{\code{Tables}} interface.
#' Such objects can be used by functions of many different
#' Julia packages that deal with table-like data structures.
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
#' The following table shows how basic R-compatible types of Julia are translated to R:
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
#' Julia \code{Array}s of these types are translated to \code{vector}s or \code{array}s of the corresponding types in R.
#'
#'
#' Julia functions are translated to R functions that call the Julia function.
#' These functions can also be translated back to the
#' corresponding Julia functions when used as argument of another function
#' (see \code{\link{juliaFun}}).
#'
#'
#' Julia object of other types, in particular \code{struct}s, \code{Tuple}s, \code{NamedTuple}s,
#' and \code{AbstractArray}s of other types are transferred by reference in the form of proxy objects.
#' Elements and properties of these proxy objects can be accessed and mutated via the operators \code{`[[`},
#' \code{`[`}, and \code{`$`} (see \link{AccessMutate.JuliaProxy}).
#'
#' A full translation of the proxy objects into R objects, which also allows saving these objects in R,
#' is possible via \code{\link{juliaGet}}.
#'
#'
#' }
#'
#' @section Limitations:
#' \subsection{Possible inexactness when dealing with large 64 bit integers}{
#' Numbers of type \code{Int64} that are too big to be expressed as 32-bit
#' \code{integer} values in R will be translated to \code{double} numbers.
#' This may lead to a inaccurate results for very large numbers,
#' when they are translated back to Julia, since, e. g.,
#' \code{(2^53 + 1) - 2^53 == 0} holds for double-precision
#' floating point numbers.
#' }
#'
#' \subsection{Non-ASCII characters in variable names}{
#' Julia uses UTF-8 as default string encoding everywhere.
#' In particular, Julia permits characters that are not
#' expressible in encodings such as "Latin-1" in variable and function names.
#' In R, the encoding of names in lists of environments depends on the platform.
#' On locales without UTF-8 as native encoding, (i.e., mostly Windows),
#' unexpected translations may happen when using UTF-8 characters in strings.
#'
#' When using \code{\link{juliaImport}} for importing packages/modules,
#' alternative names for variables using non-ASCII characters are added,
#' which are compatible across different encodings.
#' (For more information, see \code{\link{juliaImport}}.)
#'
#' In other places, such as when evaluating code via \code{\link{juliaEval}} and
#' \code{\link{juliaLet}}, the problem cannot be addressed.
#' It should therefore be avoided to use non-ASCII characters
#' if code should be portable across different platforms.
#' }
#'
#' @name JuliaConnectoR-package
"_PACKAGE"


#' Environment variables used by the \pkg{JuliaConnectoR}
#'
#' There are some environment variables which can be used to deviate from the
#' default behavior of the package.
#' To have an effect, these environment variables must be set before a Julia
#' connection is established, i.e., before the first call to Julia or before a
#' call to  \code{\link{startJuliaServer}}.
#' All the variables are optional.
#'
#' The environment variables that are used in the package are listed below:
#' \describe{
#' \item{\env{JULIA_BINDIR}:}{The directory where the \code{bin} directory of
#'   the Julia can be specified explicitly. This allows, e.g., to easily work
#'   with different Julia versions without having to modify the system
#'   \env{PATH}.}
#' \item{\env{JULIACONNECTOR_JULIAENV}:}{Specify environment variables only for
#'    Julia.
#'    (This does not work on Windows and the variable is ignored there.)
#'    This allows, e.g., to set
#'    the LD_LIBRARY_PATH variable to a different value for Julia than for R.
#'    The value can be any R code that defines variables, e.g.,
#'    \code{"LD_LIBRARY_PATH=''"} is a valid value.
#'    On Linux, Julia is started with an empty \code{LD_LIBRARY_PATH}
#'    by default as the \code{LD_LIBRARY_PATH} required by R may be incompatible
#'    with Julia. If the \code{LD_LIBRARY_PATH} needs to be set to a different
#'    value, this can be done via the \code{JULIACONNECTOR_JULIAENV} variable.}
#' \item{\env{JULIACONNECTOR_JULIAOPTS}:}{Set start-up options for Julia.
#'    As an example, consider specifying the project environment and enabling
#'    code coverage when starting Julia. This can be achieved by setting
#'    the environment variable to
#'    \code{"--project=/path/to/project --code-coverage"}.}
#' \item{\env{JULIACONNECTOR_SERVER}:}{Specifies the server address of a
#'   (running) Julia server that the R process can connect to.
#'   A possible example value is "localhost:11980", specifying host and port.
#'   The function \code{\link{startJuliaServer}} sets this variable and
#'   communicates the location of the server to child processes with it.
#'   Due to security concerns, the Julia server accepts only connections from
#'   the same machine and connecting to remote machines is currently not
#'   possible.}
#' }
#'
#' @aliases JULIA_BINDIR JULIACONNECTOR_JULIAENV JULIACONNECTOR_SERVER
#' @name EnvVars-JuliaConnectoR
NULL


#' Julia setup
#'
#' Julia must be installed separately from the \pkg{JuliaConnectoR} package.
#' You can download and install Julia from \href{https://julialang.org/downloads/}{https://julialang.org/downloads/}.
#'
#' \subsection{Setup via the Juliaup installation manager}{
#' If you have installed Julia via Juliaup,
#' the Julia installation should be discovered by the \pkg{JuliaConnectoR}.
#' }
#'
#' \subsection{Juliaup on Windows}{
#' On Windows, if you have freshly installed Juliaup, start Julia once on the command line.
#' This will do the actual installation of the current Julia version.
#' Juliaup puts the Julia executable on the system \env{PATH}.
#' This way, the Julia installation can be detected by the \pkg{JuliaConnectoR}
#' }
#'
#' \subsection{Juliaup on Mac}{
#' After the installation of Juliaup, it might not be on the system \env{PATH}
#' but it should be discovered automatically
#' if it is installed in the default location, i.e., the \code{.juliaup}
#' folder in your home directory.
#' }
#'
#' \subsection{Setup via Julia binaries}{
#' If you have installed Julia via a binary package or any other method,
#' the simplest way to make Julia discoverable is by adding the directory
#' containing the Julia executable to the \env{PATH} environment variable.
#'
#' Alternatively, you can set the \env{JULIA_BINDIR} environment variable
#' to specify the exact directory containing the Julia binary.
#' (You can find the correct path to this directory by evaluating the expression
#' \code{Sys.BINDIR} within Julia.)
#'
#' If the \env{JULIA_BINDIR} variable is set, it takes precedence over
#' looking up the system \env{PATH}.
#' This makes it easy to use a different Julia version
#' than the one in your system \env{PATH}.
#' }
#'
#' @name Setup-JuliaConnectoR
NULL
