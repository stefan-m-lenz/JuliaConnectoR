library(testthat)
library(JuliaConnectoR)
library(utils)

if (juliaSetupOk()) {
   test_check("JuliaConnectoR")
   stop(paste("Julia setup cannot be ok", "executablepath", JuliaConnectoR:::getJuliaExecutablePath(), "version", JuliaConnectoR:::getJuliaVersion()))
} else if (Sys.getenv("GITHUB_ACTIONS") == "true") {
   stop("Julia setup on GitHub Actions is not OK")
}
