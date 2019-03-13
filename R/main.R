JULIA_PORTS <- 11980:11989

# Constants
TYPE_ID_NULL <- as.raw(0x00)
TYPE_ID_DOUBLE <- as.raw(0x01)
TYPE_ID_INTEGER <- as.raw(0x02)
TYPE_ID_LOGICAL <- as.raw(0x03)
TYPE_ID_STRING <- as.raw(0x04)
TYPE_ID_LIST <- as.raw(0x05)
TYPE_ID_FAIL <- as.raw(0xff)

CALL_INDICATOR <- as.raw(0x01)
RESULT_INDICATOR <- as.raw(0x00)
BYEBYE <- as.raw(0xbb)

LOAD_MODE_USING <- 0L
LOAD_MODE_IMPORT <- 1L

TYPE_IDS <- list(
   "double" = TYPE_ID_DOUBLE,
   "integer" = TYPE_ID_INTEGER,
   "logical" = TYPE_ID_LOGICAL,
   "character" = TYPE_ID_STRING,
   "list" = TYPE_ID_LIST)


# for holding local package variables (the Julia connection object)
pkgLocal <- new.env(parent = emptyenv())


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

      # start Julia server in background
      system2(juliaexe, c(mainJuliaFile, port),
              wait = FALSE) # for testing on Windows: invisible = FALSE

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


finalize <- function(env) {
   stopJulia()
}

.onLoad <- function(libname, pkgname) {
   # register finalizer to stop Julia connection
   parent <- parent.env(environment())
   reg.finalizer(parent, finalize, onexit = TRUE)
}


juliaCall <- function(name, ...) {
   ensureJuliaConnection()

   jlargs <- list(...)
   writeBin(CALL_INDICATOR, pkgLocal$con)
   writeString(name)
   writeList(jlargs)
   messageType <- c()
   while (length(messageType) == 0) {
      messageType <- readBin(pkgLocal$con, "raw", 1)
   }
   if (messageType == RESULT_INDICATOR) {
      return(readElement())
   } else {
      print(messageType)
      print("Message type not supported (yet)")
      stopJulia()
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
      })
      pkgLocal$con <- NULL
   }
}

ensureJuliaConnection <- function() {
   if (is.null(pkgLocal$con)) {
      startJulia()
   }
}

juliaObj <- function(type, x) {
   if (length(x) > 26) stop("So many members need a more sophisticated implementation.")
   attributes(x) <- list(JLTYPE = type, names = letters[length(x)])
   x
}

juliaEval <- function(str) {
   juliaCall("RConnector.maineval", str)
}


attachFunctionList <- function(funnames, name, rPrefix, juliaPrefix) {
   funlist <- lapply(funnames, function(funname) {
      function(...) {
         juliaCall(paste0(juliaPrefix, funname), ...)
      }
   })
   names(funlist) <- paste0(rPrefix, funnames)
   funenv <- list2env(funlist)

   # support versions lower than 3.3.0
   if (exists("endsWith")) {
      endsWithChar <- endsWith
   } else {
      endsWithChar <- function(str, suffix) {
         substring(str, nchar(str), nchar(str)) == suffix
      }
   }

   # create a function without "bang" for each "bang-function"
   # if there is not already one without bang
   for (funname in ls(funenv)) {
      if (endsWithChar(funname, "!")) {
         funnameNoBang <- substr(funname, 1, nchar(funname)-1)
         if (is.null(funenv[[funnameNoBang]])) {
            funenv[[funnameNoBang]] <- funenv[[funname]]
         }
      }
   }

   attach(funenv, name = name)
}


attachJuliaPackage <- function(pkgName, alias, mode, importInternal = FALSE) {
   ensureJuliaConnection()

   if (length(pkgName) != 1) {
      stop("Expected exactly one package name")
   }

   if (mode == LOAD_MODE_USING) {
      loadMode <- "using"
      juliaPrefixExported <- ""
      rPrefixExported <- ""
   } else if (mode == LOAD_MODE_IMPORT) {
      loadMode <- "import"
      juliaPrefixExported <- paste0(pkgName, ".")
      rPrefixExported <- paste0(alias, ".")
   } else {
      stop(paste("Unknown mode:", mode))
   }

   juliaEval(paste(loadMode, pkgName))

   pkgContent <- juliaCall("RConnector.pkgContentList", pkgName, all = importInternal)
   if (!is.list(pkgContent)) {
      # must be an error
      stop(paste0("Could not load Julia package \"",  pkgName,
                  "\" (is it installed?): ", pkgContent))
   }

   attachFunctionList(pkgContent$exportedFunctions, pkgName,
                      rPrefixExported, juliaPrefixExported)

   juliaPrefixInternal <- paste0(pkgName, ".")
   rPrefixInternal <- paste0(alias, ".")

   if (mode == LOAD_MODE_USING) {
      attachFunctionList(pkgContent$exportedFunctions, pkgName,
                         rPrefixInternal, juliaPrefixInternal)
   }

   if (importInternal) {
      attachFunctionList(pkgContent$internalFunctions, pkgName,
                         rPrefixInternal, juliaPrefixInternal)
   }
}


juliaUsing <- function(pkgName, alias = pkgName, importInternal = FALSE) {
   attachJuliaPackage(pkgName, alias,
                      mode = LOAD_MODE_USING,
                      importInternal = importInternal)
}

juliaImport <- function(pkgName, alias = pkgName, importInternal = FALSE) {
   attachJuliaPackage(pkgName, alias,
                      mode = LOAD_MODE_IMPORT,
                      importInternal = importInternal)
}

