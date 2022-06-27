library(testthat)
library(JuliaConnectoR)
library(utils)


if (juliaSetupOk()) {
   test_check("JuliaConnectoR")
} else if (identical(Sys.getenv("GITHUB_ACTIONS"), "true")) {
   stop("Julia setup on GitHub Actions is not OK")
}
