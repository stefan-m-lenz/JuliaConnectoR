library(testthat)
library(JuliaConnectoR)
library(utils)

juliaWorks <- FALSE
try({
      juliaEval("1")
      juliaWorks <- TRUE
   })

if (juliaWorks) {
   test_check("JuliaConnectoR")
} else {
   warning("Tests not performed because Julia was not found.")
}

