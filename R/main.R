JULIA_PORTS <- 11980:11989

# Constants
TYPE_ID_NULL <- as.raw(0x00)
TYPE_ID_DOUBLE <- as.raw(0x01)
TYPE_ID_COMPLEX <- as.raw(0x02)
TYPE_ID_RAW <- as.raw(0x03)
TYPE_ID_INTEGER <- as.raw(0x04)
TYPE_ID_LOGICAL <- as.raw(0x05)
TYPE_ID_STRING <- as.raw(0x06)
TYPE_ID_LIST <- as.raw(0x07)
TYPE_ID_CALLBACK <- as.raw(0xcb)
TYPE_ID_EXPRESSION <- as.raw(0xee)

CALL_INDICATOR <- as.raw(0x01)
RESULT_INDICATOR <- as.raw(0x00)
FAIL_INDICATOR <- as.raw(0xff)
BYEBYE <- as.raw(0xbb)

LOAD_MODE_USING <- 0L
LOAD_MODE_IMPORT <- 1L

TYPE_IDS <- list(
   "double" = TYPE_ID_DOUBLE,
   "complex" = TYPE_ID_COMPLEX,
   "raw" = TYPE_ID_RAW,
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

#' Calls a Julia function.
#'
#' @param name name of the Julia function
#' @param ... parameters handed to the function. Will be translated
#' to Julia data structures
#'
#' @return the value returned from Julia,
#' translated to an R data structure
juliaCall <- function(name, ...) {
   ensureJuliaConnection()

   jlargs <- list(...)
   writeBin(CALL_INDICATOR, pkgLocal$con)
   writeString(name)
   callbacks <- writeList(jlargs)
   messageType <- handleCallbacks(callbacks)
   if (messageType == RESULT_INDICATOR) {
      return(readElement(callbacks))
   } else if (messageType == FAIL_INDICATOR) {
      stop(readString())
   } else {
      print(messageType)
      print("Message type not supported (yet)")
      stopJulia()
   }
}


#' Evaluates a Julia expression and translates the result to R.
#'
#' @param expr Julia expression as a one-element character vector
#'
#' @return the value of the expression, translated to R
#'
#' @examples juliaEval("1 + 2")
juliaEval <- function(expr) {
   ensureJuliaConnection()
   juliaCall("RConnector.maineval", expr)
}


handleCallbacks <- function(callbacks) {
   repeat {
      messageType <- readMessageType()
      if (messageType == CALL_INDICATOR) {
         call <- readCall()
         callbackIdx <- strtoi(call$name, base = 10)
         callbackfun <- callbacks[[callbackIdx]]
         tryCatch(answerCallback(callbackfun, call$args),
                  error = function(e) {
                     warning(e)
                     writeFailMessage(as.character(e))
                  })
      } else {
         return(messageType)
      }
   }
}


answerCallback <- function(fun, args) {
   ret <- do.call(fun, args)
   writeResultMessage(ret)
}



# juliaObj <- function(type, x) {
#    if (length(x) > 26) stop("So many members need a more sophisticated implementation.")
#    attributes(x) <- list(JLTYPE = type, names = letters[length(x)])
#    x
# }
