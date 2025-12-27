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

   port <- runJuliaServer(multiclient = FALSE)

   return(list(port = port,
               con = socketConnection(host = "localhost",
                                      port = port,
                                      blocking = TRUE,
                                      server = FALSE,
                                      open="r+b", timeout = 2)))
}


getJuliaEnv <- function() {
   jlenv <- character()
   if (Sys.getenv("JULIACONNECTOR_JULIAENV") != "") {
      if (Sys.info()['sysname'] == "Windows") {
         warning("Setting \"JULIACONNECTOR_JULIAENV\" not supported on Windows")
      } else {
         envdef <- Sys.getenv("JULIACONNECTOR_JULIAENV")
         evalenv <- new.env(emptyenv())
         eval(expr = parse(text = envdef), envir = evalenv)
         if (Sys.info()['sysname'] == "Linux" && is.null(evalenv$LD_LIBRARY_PATH)) {
            evalenv$LD_LIBRARY_PATH <- "''"
         }
         # system2 expects a character vector of name=value strings
         jlenv <- unlist(lapply(names(evalenv),
                         function(x) { paste0(x, "=", shQuote(evalenv[[x]])) }))
      }
   } else { # no environment variables for Julia specified by user
      if (Sys.info()['sysname'] == "Linux") {
         jlenv <- "LD_LIBRARY_PATH=''"
      }
   }
   return(jlenv)
}


#' Start a Julia server that may serve multiple clients (R processes)
#'
#' Starting a Julia server allows that different R processes may connect to the
#' the same Julia server and share a single session.
#' This can be useful for saving start-up/precompilation time when starting
#' additional processes or when sharing global variables between processes.
#' \emph{For the standard way of starting Julia, this function is not needed.
#' It is also not needed if child processes should use separate Julia sessions.}
#'
#' The functions communicates the server address via setting the
#' \env{JULIACONNECTOR_SERVER} environment variable.
#' A possible value for the variable is "localhost:11980".
#' The \env{JULIACONNECTOR_SERVER} variable is communicated automatically via
#' the system environment to child processes that are started after this
#' function has been called.
#' The child processes will then connect to the same Julia server if the
#' variable is set.
#' The variable can also be set explicitly in child processes before connecting
#' to Julia to connect to a running server.
#' Unsetting the variable will result in a normal Julia start-up in the first
#' call to Julia, using a single-client Julia session.
#'
#' For security reasons, the Julia server accepts only connections
#' from localhost.
#'
#' For using Julia with multiple clients, it can be good to advise Julia to
#' use multiple threads via setting the \env{JULIA_NUM_THREADS} environment
#' variable before starting Julia.
#'
#' @note The standard (error) output from Julia (printing and warnings)
#' can currently only be forwarded to one client.
#' This is currently the last client that has connected but this may be subject
#' to change.
#'
#' @param port a hint for the port that is used by the server.
#'    If it is not available, a different port is used.
#'    The final port is returned (invisibly).
#'
#' @return the port number (invisibly)
#'
#' @seealso \link{JULIACONNECTOR_SERVER}
#'
#' @examples
#' if (juliaSetupOk()) {
#'    print("dont do this")
#'    #Sys.setenv("JULIA_NUM_THREADS" = parallel::detectCores())
#'    #startJuliaServer()
#'
#'    #library(future)
#'    #plan(multisession) # use background R processes on the same machine
#'
#'    #juliaEval("global x = 1")
#'
#'    # Child processes now use the same Julia session:
#'    #f1 <- future({juliaEval("x")})
#'    #value(f1)
#'
#'    #plan(sequential) # close background workers
#'
#' }
#' \dontshow{
#' JuliaConnectoR:::stopJulia()
#' }
startJuliaServer <- function(port = 11980) {
   if (!is.null(pkgLocal$con)) {
      warning(paste0("There is already a connection to Julia established.\n",
                     "Run \"stopJulia()\" to stop this connection ",
                     "before running \"startJuliaServer()\"."))
      return(invisible(NULL))
   }

   port <- runJuliaServer(port, multiclient = TRUE)
   Sys.setenv("JULIACONNECTOR_SERVER" = paste0("localhost:", port))

   # It is noted that Julia has been started to serve multiple clients.
   # This can later be used for knowing that the environment variable
   # JULIACONNECTOR_SERVER has become invalid and needs to be unset.
   pkgLocal$startedAsMultiClientServer <- TRUE

   # Establish and test the connection. This also sets the pkgLocal$communicator
   juliaEval("0")
   return(invisible(port))
}


# Starts a Julia process in the background that listens on a port.
# A port hint is given by the argument "port".
# The return value is the port where Julia is actually listening.
# This port might be different than the port hint, if the given "port"
# is e. g. already in use.
runJuliaServer <- function(port = 11980, multiclient = TRUE) {
   startupOpts <- Sys.getenv('JULIACONNECTOR_JULIAOPTS')
   optsMessage <- ifelse(startupOpts != '', paste0(' (with opts: ', startupOpts, ')'), '')

   message("Starting Julia ...", optsMessage)

   # If there is no Julia server specified, start a new one:
   mainJuliaFile <- system.file("Julia", "main.jl",
                                package = "JuliaConnectoR", mustWork = TRUE)

   portfilename <- tempfile(paste0("juliaPort", Sys.getpid()))

   # workaround for https://github.com/rstudio/rstudio/issues/2446
   startupOutputFile <- tempfile('startupOutput');
   on.exit(unlink(startupOutputFile), add = TRUE)


   if (multiclient == TRUE) {
      multiclient <- "t"
   } else {
      multiclient <- "f"
   }

   # start Julia server in background
   juliaexe <- getJuliaExecutablePath()

   if (startupOpts != '') {
      # if startup options are specified, separate them from the arguments via "--"
      startupOpts <- paste(startupOpts, '-- ')
   }

   system2(command = juliaexe,
           args = c(startupOpts, shQuote(mainJuliaFile), port, shQuote(portfilename),
                    multiclient),
           wait = FALSE,
           stdout = startupOutputFile, stderr = startupOutputFile,
           env = getJuliaEnv())

   readJuliaOutputSafely <- function() {
      tryCatch({
         return(paste(suppressWarnings(readLines(startupOutputFile)), collapse = "\n"))
      }, error = function(e) {
         return(paste("[Could not read Julia output log: file is locked by the process.",
                      "This is expected on Windows if the Julia process has not stopped.]"))
      })
   }

   # get information about the real port from the temporary file
   sleepTime <- 0.2
   timeSlept <- 0

   timeSinceLastOutput <- 0
   lastFileSize <- 0

   # timeout happens if there is no output from Julia in this duration
   silenceLimit <- as.integer(Sys.getenv("JULIACONNECTOR_SILENCE_TIMEOUT", 60))

   # timeout happens after this time elapsed since starting up Julia
   maxTimeout <- as.integer(Sys.getenv("JULIACONNECTOR_MAX_TIMEOUT", 300))

   while (file.access(portfilename, mode = 4) < 0) {
      Sys.sleep(sleepTime)
      timeSlept <- timeSlept + sleepTime
      timeSinceLastOutput <- timeSinceLastOutput + sleepTime

      # Check if output file has grown (Julia is alive and talking/precompiling)
      finfo <- file.info(startupOutputFile)
      currentSize <- finfo$size

      if (!is.na(currentSize) && currentSize > lastFileSize) {
         # Output detected: Reset the silence timer
         timeSinceLastOutput <- 0
         lastFileSize <- currentSize
      }

      # Check for timeouts
      if (timeSinceLastOutput >= silenceLimit) {
         cat(c("Julia startup (timed out due to lack of output):\n"))
         cat(readJuliaOutputSafely(), "\n")
         stop(paste("Timeout: No output from Julia server for", silenceLimit, "seconds."))
      }

      if (timeSlept >= maxTimeout) {
         cat(c("Julia startup (timed out due to total duration):\n"))
         cat(readJuliaOutputSafely(), "\n")
         stop(paste("Timeout: Julia server failed to start within", maxTimeout, "seconds."))
      }
   }

   portfile <- file(portfilename, open = "r")
   realJuliaPort <- as.integer(readLines(con = portfile, n = 1L, ok = FALSE, encoding = "UTF-8"))
   close(portfile)
   file.remove(portfilename)
   return(realJuliaPort)
}


getJuliaVersionViaCmd <- function(juliaCmd = getJuliaExecutablePath()) {
   juliaVersion <- NULL
   try({
      juliaVersion <- system2(juliaCmd, "--version", stdout = TRUE,
                              env = getJuliaEnv())
      juliaVersion <- regmatches(juliaVersion,
                                 regexpr("[0-9]+\\.[0-9]+\\.[0-9]+",
                                         juliaVersion))
   })
   juliaVersion
}


#' Check Julia setup
#'
#' Checks that Julia can be started and that the Julia version is at least 1.0.
#' For more information about the setup and discovery of Julia,
#' see \link{JuliaConnectoR-package}, section "Setup".
#'
#' @return \code{TRUE} if the Julia setup is OK; otherwise \code{FALSE}
juliaSetupOk <- function() {

   juliaCmd <- NULL
   try({
      juliaCmd <- getJuliaExecutablePath()
   })
   if (is.null(juliaCmd)) {
      message("Julia setup check failed: Julia command not found")
      return(FALSE)
   }

   juliaVersion <- getJuliaVersionViaCmd(juliaCmd)
   if (is.null(juliaVersion)) {
      message("Julia setup check failed: Julia version could not be determined")
      return(FALSE)
   }

   juliaVersion <- as.integer(unlist(strsplit(juliaVersion, ".", fixed = TRUE)))
   if (juliaVersion[1] < 1) {
      message("Julia setup check failed: Julia version is less than 1.0")
      return(FALSE)
   }

   # try to actually start the connection
   setupOk <- tryCatch({
      ensureJuliaConnection()
      TRUE
   }, error = function(e) {
      message("Julia setup check failed: ", e$message)
      return(FALSE)
   })
   return(setupOk)
}


startJulia <- function() {
   jlc <- juliaConnection()
   pkgLocal$con <- jlc$con
   pkgLocal$port <- jlc$port
   pkgLocal$communicator <- juliaEval("RConnector.GetCommunicatoR()")
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
      if (Sys.which("julia") == "") {
         juliaCmd <- fallbackOnDefaultJuliaupPath()
      } else { # Julia is on the PATH, simply use the command "julia"
         juliaCmd <- "julia"
      }
   } else { # use the JULIA_BINDIR variable, as it is specified
      juliaExe <- list.files(path = juliaBindir, pattern = "^julia.*")
      if (length(juliaExe) == 0) {
         stop(paste0("No Julia executable file found in supposed bin directory \"" ,
                     juliaBindir, "\""))
      }
      juliaCmd <- file.path(juliaBindir, "julia")
   }
   return(juliaCmd)
}


fallbackOnDefaultJuliaupPath <- function() {
   # If Julia is not found on the PATH, check the default Juliaup installation location
   # on Linux and Mac and use the Julia command there if it exists.
   # (On Mac, Julia might not be on the PATH in the R session even though
   # Julia has been installed in the default way via Juliaup.)
   juliaCmd <- file.path(Sys.getenv("HOME"), ".juliaup", "bin", "julia")
   if (!file.exists(juliaCmd) || Sys.info()['sysname'] == "Windows") {
      stop('Julia could not be found.
Julia needs to be installed and findable for the "JuliaConnectoR" package to work.
After installing Julia, the best way make Julia findable is to put the folder containing the Julia executable into the PATH environment variable.
For more information, see the help topic ?`Julia-Setup`.
')
   } else {
      return(juliaCmd)
   }
}


#
#' Stop the connection to Julia
#'
#' This ends the connection to Julia. Julia terminates if no R process is
#' connected any more.
stopJulia <- function() {
   if (!is.null(pkgLocal$startedAsMultiClientServer)) {
      Sys.unsetenv("JULIACONNECTOR_SERVER")
      pkgLocal$startedAsMultiClientServer <- NULL
   }
   if (!is.null(pkgLocal$con)) {
      tryCatch({writeBin(BYEBYE, pkgLocal$con)}, error = function(e) {})
      tryCatch({close(pkgLocal$con)}, error = function(e) {})
      pkgLocal$con <- NULL
      pkgLocal$port <- NULL
      pkgLocal$communicator <- NULL

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
   if (length(juliaPid) == 1 && juliaPid != 0) {
      system2(command = "taskkill", args = paste("/F /PID", juliaPid),
              stdout = FALSE)
   }
}

# should work on Linux, MacOS and FreeBSD
killJuliaUnix <- function(juliaPort) {
   lsofArgs <- paste0("-t -iTCP:", juliaPort, " -sTCP:LISTEN")
   suppressWarnings({
      # there may be a warning if Julia has already been stopped
      juliaPid <- system2(command = "lsof", args = lsofArgs, stdout = TRUE
   )})
   if (length(juliaPid) == 1) {
      system2(command = "kill", c("-9", juliaPid))
   }
}
