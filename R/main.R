JULIA_HOST = "localhost"
JULIA_PORT = 11987

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
BYBYE <- as.raw(0xbb)

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
   print("julia started")
   system2(juliaexe, "./Julia/main.jl", wait = FALSE, invisible = FALSE)
   print("julia command success")
   Sys.sleep(10)
   print("trying socket connection")
   socketConnection(host = JULIA_HOST,
                    port = JULIA_PORT,
                    blocking = TRUE,
                    server = FALSE,
                    open="r+b")
}


finalize <- function(env) {
   stopJulia()
}


.onLoad <- function(libname, pkgname) {
   # start Julia
   con <<- juliaConnection()

   # register finalizer to stop Julia connection
   parent <- parent.env(environment())
   reg.finalizer(parent, finalize, onexit = TRUE)
}


juliafun <- function(name, ...) {
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

stopJulia <- function() {
   writeBin(BYBYE, con)
}

useJuliaPkg <- function(pkgnames) {
   juliafun("TcpCallR.execute", paste("using", (paste(pkgnames, collapse = ", "))))
}

