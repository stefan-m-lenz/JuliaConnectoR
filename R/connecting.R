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
   juliaexe <- Sys.getenv("JULIA_BINDIR")
   if (juliaexe == "") {
      juliaexe <- "julia"
   } else {
      juliaexe <- file.path(juliaexe, "julia")
   }

   mainJuliaFile <- system.file("Julia", "main.jl",
                                package = "JuliaConnectoR", mustWork = TRUE)

   # workaround for https://github.com/rstudio/rstudio/issues/2446
   stdoutfile = tempfile('stdout'); stderrfile = tempfile('stderr')
   on.exit(unlink(c(stdoutfile, stderrfile)), add = TRUE)

   portfilename = tempfile(paste0("juliaPort", Sys.getpid()))

   port <- 11980 # default port

   # start Julia server in background
   system2(juliaexe, c(mainJuliaFile, port, portfilename), wait = FALSE,
           stdout = stdoutfile, stderr = stderrfile)

   # get information about the real port from the temporary file
   while (!file.exists(portfilename)) {
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
