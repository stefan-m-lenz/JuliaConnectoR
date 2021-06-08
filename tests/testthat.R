library(testthat)
library(JuliaConnectoR)
library(utils)

if (identical(Sys.getenv("TRAVIS"), "true")) {
   # prevent issue with glibc that happens on Travis
   # https://discourse.julialang.org/t/unable-to-launch-julia-1-6-ssl-and-glibc-issues/60488/5
   Sys.unsetenv("LD_LIBRARY_PATH")
}


if (juliaSetupOk()) {
   test_check("JuliaConnectoR")
}
