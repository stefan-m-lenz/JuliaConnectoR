library(testthat)
library(JuliaConnectoR)
library(utils)

if (juliaSetupOk()) {
   test_check("JuliaConnectoR")
} else if (Sys.getenv("GITHUB_ACTIONS") == "true" ||
           Sys.getenv("NOT_CRAN") == "true") {
   stop("Julia setup is not OK for this non-CRAN test run")
}
