library(testthat)
library(JuliaConnectoR)
library(utils)

stop(paste0("schluss: ", Sys.getenv("GITHUB_ACTIONS")))

if (juliaSetupOk()) {
   test_check("JuliaConnectoR")
} else if (identical(Sys.getenv("GITHUB_ACTIONS"), "true")) {
   stop("Julia setup on GitHub Actions is not OK")
}
