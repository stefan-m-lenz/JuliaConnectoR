#' Coerce a Julia Table to a Data Frame
#'
#' Get the data from a Julia proxy object that implements the Julia
#' \href{https://github.com/JuliaData/Tables.jl}{\code{Tables}} interface,
#' and create an R data frame from it.
#'
#' Strings are not converted to factors.
#'
#' @param x a proxy object pointing to a Julia object that implements the interface
#'   of the package Julia package \code{Tables}
#' @param ... (not used)
#'
#' @examples
#' \dontshow{
#'    if (juliaSetupOk() && Sys.getenv("NOT_CRAN") == "true") {
#'       Pkg <- juliaImport("Pkg")
#'       if (juliaEval('VERSION < v"1.6"')) {
#'          subproject <- "1_0"
#'       } else if (juliaEval('VERSION < v"1.10"')) {
#'          subproject <- "1_6"
#'       } else {
#'          subproject <- "1_10"
#'       }
#'       Pkg$activate(system.file("examples", "IndexedTables-Project", subproject,
#'                    package = "JuliaConnectoR"))
#'       Pkg$instantiate()
#'       # ignore warnings on Julia 1.0:
#'       capture.output(juliaImport("IndexedTables"), type = "message")
#'    }
#' }
#' if (juliaSetupOk() && Sys.getenv("NOT_CRAN") == "true") {
#'    # (This example is not run on CRAN as it takes a little too long.)
#'
#'    # Demonstrate the usage with the Julia package "IndexedTables" (v1.0)
#'
#'    # Install the package first if it is not installed:
#'    # juliaEval('import Pkg; Pkg.add("IndexedTables")')
#'
#'    # Import "IndexedTables" package
#'    IndexedTables <- juliaImport("IndexedTables")
#'
#'    mydf <- data.frame(x = c(1, 2, 3),
#'                       y = c("a", "b", "c"),
#'                       z = c(TRUE, FALSE, NA),
#'                       stringsAsFactors = FALSE)
#'
#'    # Create a table in Julia, e. g. via IndexedTables
#'    mytbl <- IndexedTables$table(mydf)
#'
#'    # This table can, e g. be queried and
#'    # the result can be translated to an R data frame.
#'    seltbl <- IndexedTables$select(mytbl, juliaExpr("(:x, :y)"))[1:2]
#'
#'    # Translate selection of Julia table into R data frame
#'    as.data.frame(seltbl)
#'
#' }
#'
#' \dontshow{
#'    if (juliaSetupOk()){
#'       stopJulia()
#'       if (Sys.getenv("NOT_CRAN") == "true") {
#'          rm(mytbl, seltbl)
#'       }
#'    }
#' }
as.data.frame.JuliaProxy <- function(x, ...) {
   juliaCall("RConnector.full_translation!", pkgLocal$communicator, TRUE)
   tryCatch({ret <- juliaCall("RConnector.get_df", x)},
            finally = { juliaCall("RConnector.full_translation!",
                                  pkgLocal$communicator, FALSE)})
   ret <- data.frame(ret, stringsAsFactors = FALSE)
   ret
}
