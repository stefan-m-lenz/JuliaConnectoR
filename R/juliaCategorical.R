#' Convert an R factor to a Julia categorical array
#'
#' Create a categorical vector using the Julia package
#' \href{https://github.com/JuliaData/CategoricalArrays.jl}{CategoricalArrays.jl}.
#' Factor levels, including unused levels and their order, and the ordered
#' status are preserved. Missing factor values become Julia \code{missing}.
#'
#' @param x an R factor, including an ordered factor.
#'
#' @details
#' CategoricalArrays.jl must be installed in the active Julia environment
#' (for example, with \code{juliaEval('import Pkg; Pkg.add("CategoricalArrays")')}).
#' It is loaded only when this function is first called; this function never
#' installs Julia packages automatically.
#'
#' The result is always a one-dimensional array, even for factors of length
#' zero or one, and allows missing values. Names and other R attributes are
#' not transferred. An actual \code{NA} in \code{levels(x)} is rejected because
#' Julia cannot represent a missing value as a category distinct from missing
#' observations. The character level \code{"NA"} is supported.
#'
#' @return A \code{JuliaArrayProxy} referencing a Julia
#'   \code{CategoricalArray{Union{Missing, String}, 1}}.
#' @seealso \code{\link{juliaPut}}, \code{\link{juliaCall}}
#' @export
#' @examples
#' if (Sys.getenv("NOT_CRAN") == "true" && juliaSetupOk()) {
#'    # Install CategoricalArrays.jl first if necessary:
#'    # juliaEval('import Pkg; Pkg.add("CategoricalArrays")')
#'    severity <- ordered(c("high", NA, "low"),
#'                        levels = c("low", "medium", "high"))
#'    categorical <- juliaCategorical(severity)
#'    stopifnot(length(categorical) == 3L)
#'    stopifnot(juliaCall("RConnector.CategoricalArrays.isordered", categorical))
#'    stopifnot(identical(as.vector(juliaCall(
#'       "RConnector.CategoricalArrays.levels", categorical)), levels(severity)))
#'    stopifnot(identical(as.vector(juliaCall("ismissing.", categorical)),
#'                        c(FALSE, TRUE, FALSE)))
#'    stopifnot(length(juliaCategorical(factor(character(0)))) == 0L)
#'    stopifnot(length(juliaCategorical(factor("only"))) == 1L)
#' }
#' \dontshow{
#' if (Sys.getenv("NOT_CRAN") == "true" && juliaSetupOk()) {
#'    stopJulia()
#' }
#' }
juliaCategorical <- function(x) {
   # Validate before opening a connection: invalid R inputs should not require
   # either a running Julia process or the optional CategoricalArrays package.
   if (!is.factor(x)) {
      stop("x must be an R factor", call. = FALSE)
   }
   factorLevels <- levels(x)
   if (anyNA(factorLevels)) {
      stop("NA factor levels are not supported; use missing factor values instead",
           call. = FALSE)
   }

   # Transfer codes rather than labels to avoid repeating long strings. Explicit
   # dimensions prevent the connector from turning a length-one vector into a
   # scalar; they also make the zero-length case unambiguous. as.integer strips
   # the factor class so ordinary factor serialization remains unchanged.
   codes <- array(as.integer(x), dim = length(x))
   factorLevels <- array(factorLevels, dim = length(factorLevels))
   juliaCall("RConnector.categorical_from_factor", codes, factorLevels,
             is.ordered(x))
}
