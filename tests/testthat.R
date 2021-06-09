library(testthat)
library(JuliaConnectoR)
library(utils)

if (juliaSetupOk()) {
   test_check("JuliaConnectoR")
} else if (identical(Sys.getenv("TRAVIS"), "true")) {
   stop("Julia setup on Travis is not OK")
}
