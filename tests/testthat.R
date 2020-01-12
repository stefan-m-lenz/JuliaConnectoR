library(testthat)
library(JuliaConnectoR)
library(utils)

juliaFound <- FALSE
try({
      JuliaConnectoR:::getJuliaExecutablePath()
      juliaFound <- TRUE
   })

if (juliaFound) {
   test_check("JuliaConnectoR")
} else {
   warning("Tests not performed because Julia was not found.")
}

