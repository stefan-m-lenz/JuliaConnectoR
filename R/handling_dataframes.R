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
#'    if (juliaSetupOk()) {
#'       Pkg <- juliaImport("Pkg")
#'       Pkg$activate(system.file("examples", "IndexedTables-Project",
#'                    package = "JuliaConnectoR"))
#'       Pkg$instantiate()
#'       # ignore warnings on Julia 1.0:
#'       capture.output(juliaImport("IndexedTables"), type = "message")
#'    }
#' }
#' if (juliaSetupOk()) {
#'
#'    # Demonstrate the usage with the Julia package "IndexedTables" (v1.0)
#'    juliaEval('import Pkg; Pkg.add("IndexedTables", preserve = Pkg.PRESERVE_ALL)')
#'    IndexedTables <- juliaImport("IndexedTables")
#'
#'    mydf <- data.frame(x = c(1, 2, 3),
#'                       y = c("a", "b", "c"),
#'                       z = c(TRUE, FALSE, NA),
#'                       stringsAsFactors = FALSE)
#'
#'    # create a table in Julia, e. g. via IndexedTables
#'    mytbl <- IndexedTables$table(mydf)
#'
#'    # this table can, e g. be queried and
#'    # the result can be translated to an R data frame
#'    seltbl <- IndexedTables$select(mytbl, juliaExpr("(:x, :y)"))[1:2]
#'
#'    # translate selection of Julia table into R data frame
#'    as.data.frame(seltbl)
#'
#' }
#'
#' \dontshow{
#'    if (juliaSetupOk()) {
#'       rm(mytbl, seltbl)
#'       JuliaConnectoR:::stopJulia()
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
