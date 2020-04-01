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
#' # Demonstrate the usage with the Julia package "JuliaDB"
#' juliaEval('import Pkg; Pkg.add("JuliaDB")')
#' juliaImport("JuliaDB")
#'
#' mydf <- data.frame(x = c(1, 2, 3),
#'                    y = c("a", "b", "c"),
#'                    z = c(TRUE, FALSE, NA),
#'                    stringsAsFactors = FALSE)
#'
#' # create a table in Julia, e. g. via JuliaDB
#' mytbl <- JuliaDB.table(mydf)
#'
#' # this table can, e g. be queried and
#' # the result can be translated to an R data frame
#' seltbl <- JuliaDB.select(mytbl, juliaExpr("(:x, :y)"))[1:2]
#'
#' # translate selection of Julia table into R data frame
#' as.data.frame(seltbl)
#'
#' \dontshow{
#' rm(mytbl, seltbl)
#' JuliaConnectoR:::stopJulia()
#' }
as.data.frame.JuliaProxy <- function(x, ...) {
   juliaCall("RConnector.full_translation!", TRUE)
   tryCatch({ret <- juliaCall("RConnector.get_df", x)},
            finally = {juliaCall("RConnector.full_translation!", FALSE)})
   ret <- data.frame(ret, stringsAsFactors = FALSE)
   ret
}
