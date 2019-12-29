juliaConnection <- function() {

   juliaSocketAdress <- Sys.getenv("JULIACONNECTOR_SERVER")
   if (juliaSocketAdress != "") {
      host_port <- strsplit(juliaSocketAdress, split = ":", fixed = TRUE)[[1]]
      juliaPort <- as.integer(host_port[2])

      if (length(host_port) != 2 || is.na(juliaPort)) {
         stop("Environment variable JULIACONNECTOR_SERVER must be of form <host>:<port>")
      }
      message(paste("Connecting to Julia TCP server at", juliaSocketAdress, "..."))
      return(list(port = juliaPort,
                  con = socketConnection(host = host_port[1],
                                         port = juliaPort,
                                         blocking = TRUE,
                                         server = FALSE,
                                         open="r+b", timeout = 10)))
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

   return(list(port = realJuliaPort,
               con = socketConnection(host = "localhost",
                                      port = realJuliaPort,
                                      blocking = TRUE,
                                      server = FALSE,
                                      open="r+b", timeout = 2)))
}


startJulia <- function() {
   jlc <- juliaConnection()
   pkgLocal$con <- jlc$con
   pkgLocal$port <- jlc$port
}


getJuliaExecutablePath <- function() {
   juliaBindir <- Sys.getenv("JULIA_BINDIR")
   if (juliaBindir == "") {
      juliaCmd <- "julia" # assume julia is in the path
   } else {
      juliaCmd <- file.path(juliaBindir, "julia")
   }
   return(juliaCmd)
}


stopJulia <- function() {
   if (!is.null(pkgLocal$con)) {
      tryCatch({
         writeBin(BYEBYE, pkgLocal$con)
         close(pkgLocal$con)
      }, error = function(e) {})
      pkgLocal$con <- NULL
      pkgLocal$port <- NULL
   }
}


ensureJuliaConnection <- function() {
   if (is.null(pkgLocal$con)) {
      startJulia()
   }
}


killJulia <- function() {
   os <- Sys.info()['sysname']
   juliaPort <- pkgLocal$port
   stopJulia()
   if (os == "Windows") {
      juliaPid <- killJuliaWindows(juliaPort)
   } else {
      juliaPid <- killJuliaUnix(juliaPort)
   }
}


killJuliaWindows <- function(juliaPort) {
   netstatOut <- system2(command = "netstat", args = "-on", stdout = TRUE)
   juliaLineRegex <- paste0("^\\s*TCP\\s+\\S*:", juliaPort)
   juliaLine <- netstatOut[grep(juliaLineRegex, netstatOut)]
   juliaPid <- substring(juliaLine,
                         regexpr("[0-9]+$", juliaLine))
   juliaPid <- as.integer(juliaPid)
   if (length(juliaPid) == 1) {
      system2(command = "taskkill", args = paste("/F /PID", juliaPid),
              stdout = FALSE)
   }
}

# should work on Linux, MacOS and FreeBSD
killJuliaUnix <- function(juliaPort) {
   lsofArgs <- paste0("-t -iTCP:", juliaPort, " -sTCP:LISTEN")
   juliaPid <- system2(command = "lsof", args = lsofArgs, stdout = TRUE)
   if (length(juliaPid) == 1) {
      system2(command = "kill", c("-9", juliaPid))
   }
}
