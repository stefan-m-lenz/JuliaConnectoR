library(testthat)
library(JuliaConnectoR)
library(utils)

if (juliaSetupOk()) {
   cat(paste("Julia setup is OK, using Julia Version", JuliaConnectoR:::getJuliaVersionViaCmd(), "\n"))
   test_check("JuliaConnectoR")
} else if (Sys.getenv("GITHUB_ACTIONS") == "true") {
   stop("Julia setup on GitHub Actions is not OK")
}
