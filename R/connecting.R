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

   port <- runJuliaServer()

   return(list(port = port,
               con = socketConnection(host = "127.0.0.1",
                                      port = port,
                                      blocking = TRUE,
                                      server = FALSE,
                                      open="r+b", timeout = 2)))
}


# Starts a Julia process in the background that listens on a port.
# A port hint is given by the argument "port".
# The return value is the port where Julia is actually listening.
# This port might be different than the port hint, if the given "port"
# is e. g. already in use.
runJuliaServer <- function(port = 11980) {
   message("Starting Julia ...")

   # If there is no Julia server specified, start a new one:
   mainJuliaFile <- system.file("Julia", "main.jl",
                                package = "JuliaConnectoR", mustWork = TRUE)

   portfilename <- tempfile(paste0("juliaPort", Sys.getpid()))

   # workaround for https://github.com/rstudio/rstudio/issues/2446
   stdoutfile <- tempfile('stdout'); stderrfile <- tempfile('stderr')
   on.exit(unlink(c(stdoutfile, stderrfile)), add = TRUE)

   # start Julia server in background
   juliaexe <- getJuliaExecutablePath()
   system2(juliaexe, c(mainJuliaFile, port, portfilename), wait = FALSE,
           stdout = stdoutfile, stderr = stderrfile)

   # get information about the real port from the temporary file
   sleepTime <- 0.2
   timeSlept <- 0
   while (file.access(portfilename, mode = 4) < 0) {
      Sys.sleep(sleepTime)
      timeSlept <- timeSlept + sleepTime
      if (timeSlept >= 50) {
         stop("Timeout while waiting for response from Julia server")
      }
   }

   portfile <- file(portfilename, open = "r")
   realJuliaPort <- as.integer(readLines(con = portfile, n = 1L, ok = FALSE, encoding = "UTF-8"))
   close(portfile)
   file.remove(portfilename)
   return(realJuliaPort)
}

#' Check Julia setup
#'
#' Checks that Julia can be started and that the Julia version is at least 1.0.
#'
#' @return \code{TRUE} if the Julia setup is OK; otherwise \code{FALSE}
juliaSetupOk <- function() {

   juliaCmd <- NULL
   try({
      juliaCmd <- getJuliaExecutablePath()
   })
   if (is.null(juliaCmd)) {
      message("Julia not found")
      return(FALSE)
   }

   juliaVersion <- NULL
   try({
      juliaVersion <- system2(juliaCmd, "--version", stdout = TRUE)
   })
   if (is.null(juliaVersion)) {
      message("Julia could not be started")
      return(FALSE)
   }

   juliaVersion <- regmatches(juliaVersion,
                              regexpr("[0-9]+\\.[0-9]+\\.[0-9]+",
                                      juliaVersion))
   juliaVersion <- as.integer(unlist(strsplit(juliaVersion, ".", fixed = TRUE)))
   if (juliaVersion[1] < 1) {
      message("Julia version is less than 1.0")
      return(FALSE)
   } else {
      return(TRUE)
   }
}


startJulia <- function() {
   jlc <- juliaConnection()
   pkgLocal$con <- jlc$con
   pkgLocal$port <- jlc$port
}


showUpdateTablesMsg <- function() {
   message(paste0("Installation of the package \"Tables\" failed, ",
                  "probably due to unsatisfiable dependencies. \n",
                  "\"Tables\" is required to translate data frames properly.\n",
                  "Probable fix: Run juliaEval('import Pkg; Pkg.update()') to update ",
                  "all Julia packages and restart the R session."))
}


getJuliaExecutablePath <- function() {
   juliaBindir <- Sys.getenv("JULIA_BINDIR")
   if (juliaBindir == "") {
      if (nchar(Sys.which("julia")) == 0) {
         stop("Julia not found in path. Please check your Julia setup.")
      }
      juliaCmd <- "julia"
   } else {
      juliaExe <- list.files(path = juliaBindir, pattern = "^julia.*")
      if (length(juliaExe) == 0) {
         stop(paste0("No Julia executable file found in supposed bin directory \"" ,
                     juliaBindir, "\""))
      }
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

      # clean up references
      invisible(gc(verbose = FALSE))
      pkgLocal$finalizedRefs <- NULL
   }
}


ensureJuliaConnection <- function() {
   if (is.null(pkgLocal$con)) {
      startJulia()

      # make sure that Tables.jl is available
      if (juliaEval("isdefined(Tables, :JuliaConnectoR_DummyTables)")) {
         message("Package \"Tables.jl\" (version >= 1.0) is required. Installing ...")
         # Add Tables package and trigger precompilation:
         # For Importing/precompilation use the Temp module 
         # because Tables is already defined in the Main module.
         tryCatch({juliaEval('import Pkg; Pkg.add("Tables");
            module Temp
               import Tables
            end;')}, error = function(e) {showUpdateTablesMsg()})
         stopJulia()
         startJulia()
      }
   }
}


killJulia <- function() {
   os <- Sys.info()['sysname']
   juliaPort <- pkgLocal$port
   message("Stopping Julia ...")
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
