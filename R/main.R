JULIA_PORTS <- 11980:11989

# Constants
TYPE_ID_NULL <- as.raw(0x00)
TYPE_ID_DOUBLE <- as.raw(0x01)
TYPE_ID_INTEGER <- as.raw(0x02)
TYPE_ID_LOGICAL <- as.raw(0x03)
TYPE_ID_STRING <- as.raw(0x04)
TYPE_ID_LIST <- as.raw(0x05)
TYPE_ID_CALLBACK <- as.raw(0xcb)
TYPE_ID_EXPRESSION <- as.raw(0xee)
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


# support versions lower than 3.3.0
endsWithChar <- (function() {
   if (exists("endsWith")) {
      endsWithChar <- endsWith
   } else {
      endsWithChar <- function(str, suffix) {
         substring(str, nchar(str), nchar(str)) == suffix
      }
   }
})()


emptyfun <- function(...) {}


# for holding local package variables (the Julia connection object)
pkgLocal <- new.env(parent = emptyenv())


.onLoad <- function(libname, pkgname) {
   # register finalizer to stop Julia connection
   parent <- parent.env(environment())
   reg.finalizer(parent, finalize, onexit = TRUE)
}


finalize <- function(env) {
   stopJulia()
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
      result <- readElement()
      if (inherits(result, "error")) {
         stop(result)
      } else {
         return(result)
      }
   } else {
      print(messageType)
      print("Message type not supported (yet)")
      stopJulia()
   }
}


juliaEval <- function(str) {
   ensureJuliaConnection()
   juliaCall("RConnector.maineval", str)
}



# juliaObj <- function(type, x) {
#    if (length(x) > 26) stop("So many members need a more sophisticated implementation.")
#    attributes(x) <- list(JLTYPE = type, names = letters[length(x)])
#    x
# }
