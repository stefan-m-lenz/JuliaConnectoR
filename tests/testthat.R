library(testthat)
library(JuliaConnectoR)
library(utils)

if (juliaSetupOk()) {
   test_check("JuliaConnectoR")
}

test_that("Examples from README work", {
   skip_on_cran()
   skip_if(Sys.info()["login"] %in% c("lenz", "selectstern"))
   cat("\nExecuting README examples...\n")

   Pkg <- juliaImport("Pkg")
   Pkg$activate(system.file("examples", "iris-example",
                            package = "JuliaConnectoR", mustWork = TRUE))
   Pkg$instantiate()
   Pkg$build()

   irisExampleJl <- system.file("examples", "iris-example", "iris-example.jl",
                                package = "JuliaConnectoR", mustWork = TRUE)
   irisExampleJuliaCode <- readLines(irisExampleJl)
   irisExampleJuliaCode <- sub("epochs <-.*", "epochs <- 5", irisExampleJuliaCode)
   juliaEval(paste(irisExampleJuliaCode, collapse = "\n"))
   irisExampleR <- system.file("examples",  "iris-example", "iris-example.R",
                               package = "JuliaConnectoR", mustWork = TRUE)
   irisExampleRCode <- readLines(irisExampleR)
   irisExampleRCode <- sub("epochs <-.*", "epochs <- 5", irisExampleRCode)
   scriptEnv <- new.env(emptyenv())
   eval(parse(text = paste(irisExampleRCode, collapse = "\n")),
        envir = scriptEnv)
   # just test something
   expect_s3_class(scriptEnv$model, "JuliaProxy")

   Pkg$activate() # use default environment again
})
