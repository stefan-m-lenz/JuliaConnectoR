juliaConnection <- function() {

   juliaSocketAdress <- Sys.getenv("JULIASERVER_SOCKET_ADDRESS")
   if (juliaSocketAdress != "") {
      host_port <- strsplit(juliaSocketAdress, split = ":", fixed = TRUE)[[1]]
      juliaPort <- as.integer(host_port[2])

      if (length(host_port) != 2 || is.na(juliaPort)) {
         error("Environment variable JULIASERVER_SOCKET_ADDRESS must be of form <host>:<port>")
      }
      message(paste("Connecting to Julia TCP server at", juliaSocketAdress, "..."))
      return(socketConnection(host = host_port[1],
                              port = juliaPort,
                              blocking = TRUE,
                              server = FALSE,
                              open="r+b", timeout = 10))
   }

   # If there is no Julia server specified, start a new one:
   mainJuliaFile <- system.file("Julia", "main.jl",
                                package = "JuliaConnectoR", mustWork = TRUE)

   portfilename <- tempfile(paste0("juliaPort", Sys.getpid()))
   port <- 11980 # default port

   # workaround for https://github.com/rstudio/rstudio/issues/2446
   stdoutfile <- tempfile('stdout'); stderrfile <- tempfile('stderr')
   on.exit(unlink(c(stdoutfile, stderrfile)), add = TRUE)

   # start Julia server in background
   juliaexe <- getJuliaExecutablePath()
   system2(juliaexe, c(mainJuliaFile, port, portfilename), wait = FALSE,
           stdout = stdoutfile, stderr = stderrfile)

   # get information about the real port from the temporary file
   while (file.access(portfilename, mode = 4) < 0) {
      Sys.sleep(0.2)
   }
   portfile <- file(portfilename, open = "r")
   realJuliaPort <- as.integer(readLines(con = portfile, n = 1L, ok = FALSE, encoding = "UTF-8"))
   close(portfile)
   file.remove(portfilename)

   return(socketConnection(host = "localhost",
                           port = realJuliaPort,
                           blocking = TRUE,
                           server = FALSE,
                           open="r+b", timeout = 2))
}


startJulia <- function() {
   pkgLocal$con <- juliaConnection()
}


getJuliaExecutablePath <- function() {
   juliaexe <- Sys.getenv("JULIA_EXE")
   if (juliaexe == "") {
      juliaBindir <- Sys.getenv("JULIA_BINDIR")
      if (juliaBindir == "") {
         juliaexe <- "julia" # assume julia is in the path
      } else {
         juliaexe <- file.path(juliaBindir, "julia")
      }
   }
   return(juliaexe)
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
