JULIA_PORT <- 11983

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

TYPE_IDS <- list(
   "double" = TYPE_ID_DOUBLE,
   "integer" = TYPE_ID_INTEGER,
   "logical" = TYPE_ID_LOGICAL,
   "character" = TYPE_ID_STRING,
   "list" = TYPE_ID_LIST)


juliaConnection <- function() {
   juliaexe <- Sys.getenv("JULIA_BINDIR")
   if (juliaexe == "") {
      juliaexe <- "julia"
   } else {
      juliaexe <- file.path(juliaexe, "julia")
   }

   system2(juliaexe, c("./Julia/main.jl", JULIA_PORT),
           wait = FALSE, invisible = FALSE)

   Sys.sleep(2.5) # wait for julia to start
   socketConnection(host = "localhost",
                    port = JULIA_PORT,
                    blocking = TRUE,
                    server = FALSE,
                    open="r+b")
}


finalize <- function(env) {
   stopJulia()
}

.onLoad <- function(libname, pkgname) {
   con <<- NULL
   # register finalizer to stop Julia connection
   parent <- parent.env(environment())
   reg.finalizer(parent, finalize, onexit = TRUE)
}


juliafun <- function(name, ...) {
   if (is.null(con)) {
      startJulia()
   }
   jlargs <- list(...)
   writeBin(CALL_INDICATOR, con)
   writeString(name)
   writeList(jlargs)
   message_type <- readBin(con, "raw", 1)
   if (message_type == RESULT_INDICATOR) {
      return(readElement())
   } else {
      print(message_type)
      print("Message type not supported (yet)")
      stopJulia()
   }
}

startJulia <- function() {
   con <<- juliaConnection()
}

stopJulia <- function() {
   if (!is.null(con)) {
      tryCatch({
         writeBin(BYEBYE, con)
         close(con)
      })
      con <<- NULL
   }
}

juliaObj <- function(type, x) {
   if (length(x) > 26) stop("So many members need a more sophisticated implementation.")
   attributes(x) <- list(JLTYPE = type, names = letters[length(x)])
   x
}

usingJuliaPkg <- function(pkgName) {
   if (is.null(con)) {
      startJulia()
   }
   if (length(pkgName) != 1) {
      stop("Expected exactly one package name")
   }
   juliafun("TcpCallR.maineval", paste("using", pkgName))
   juliafun("TcpCallR.pkgContentList", pkgName)
}

