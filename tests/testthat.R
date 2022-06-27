library(testthat)
library(JuliaConnectoR)
library(utils)

if (juliaSetupOk()) {
   test_check("JuliaConnectoR")
   stop("Julia setup cannot be ok")
} else if (Sys.getenv("GITHUB_ACTIONS") == "true") {
   stop("Julia setup on GitHub Actions is not OK")
}
