juliaConnection <- function() {
   juliaexe <- Sys.getenv("JULIA_BINDIR")
   if (juliaexe == "") {
      juliaexe <- "julia"
   } else {
      juliaexe <- file.path(juliaexe, "julia")
   }

   mainJuliaFile <- system.file("Julia", "main.jl",
                                package = "JuliaConnectoR", mustWork = TRUE)

   sleepTime <- 2 # rough upper bound of startup time

   for (port in JULIA_PORTS) {

      # workaround for https://github.com/rstudio/rstudio/issues/2446
      stdoutfile = tempfile('stdout'); stderrfile = tempfile('stderr')
      on.exit(unlink(c(stdoutfile, stderrfile)), add = TRUE)

      # start Julia server in background
      system2(juliaexe, c(mainJuliaFile, port), wait = FALSE,
              stdout = stdoutfile, stderr = stderrfile)


      # Give Julia server a head start before connecting.
      # The head start increases with the number of tries,
      # since the failure might not only be due to the port being in use
      # but also due to a slow startup/precompilation of Julia.
      sleepTime <- sleepTime + 1.5
      Sys.sleep(sleepTime)

      # try to connect to the Julia server that is hopefully already listening at the port
      try(return(socketConnection(host = "localhost",
                                  port = port,
                                  blocking = TRUE,
                                  server = FALSE,
                                  open="r+b", timeout = 2)))
   }
}


startJulia <- function() {
   pkgLocal$con <- juliaConnection()
}


stopJulia <- function() {
   if (!is.null(pkgLocal$con)) {
      tryCatch({
         writeBin(BYEBYE, pkgLocal$con)
         close(pkgLocal$con)
      }, error = function(e) {})
      pkgLocal$con <- NULL
   }
}


ensureJuliaConnection <- function() {
   if (is.null(pkgLocal$con)) {
      startJulia()
   }
}
