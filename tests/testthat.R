library(testthat)
library(JuliaConnectoR)
library(utils)

print("LD_LIBRARY_PATH")
print(Sys.getenv("LD_LIBRARY_PATH"))

if (juliaSetupOk()) {
   test_check("JuliaConnectoR")
} else if (identical(Sys.getenv("TRAVIS"), "true")) {
   stop("Julia setup on Travis is not OK")
}
