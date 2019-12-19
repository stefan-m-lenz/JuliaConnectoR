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
TYPE_ID_FUNCTION <- as.raw(0xfc)
TYPE_ID_EXPRESSION <- as.raw(0xee)

CALL_INDICATOR <- as.raw(0x01)
RESULT_INDICATOR <- as.raw(0x00)
STDOUT_INDICATOR <- as.raw(0x50)
STDERR_INDICATOR <- as.raw(0x5e)
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
      endsWithChar <- function(str, suffix) {
         endsWith(str, suffix)
      }
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
#' @return The value returned from Julia, translated to an R data structure.
#' If Julia returns \code{nothing}, an invisible \code{NULL} is returned.
juliaCall <- function(name, ...) {
   ensureJuliaConnection()

   if (is.null(name) || !is.character(name) || length(name) != 1) {
      stop("Name of Julia function must be specified as one-element character vector")
   }

   jlargs <- list(...)
   writeBin(CALL_INDICATOR, pkgLocal$con)
   writeString(name)
   callbacks <- writeList(jlargs)
   messageType <- handleCallbacksAndOutput(callbacks)
   if (messageType == RESULT_INDICATOR) {
      ret <- readElement(callbacks)
      if (is.null(ret)) {
         return(invisible(NULL))
      } else {
         return(ret)
      }
   } else if (messageType == FAIL_INDICATOR) {
      stop(readString())
   } else {
      print(paste(c("Message type not supported (yet): ", messageType)))
      stopJulia()
   }
}


#' Evaluate a Julia expression
#'
#' This function evaluates a Julia expression in Julia
#' and translates the result back to R.
#'
#' If the expression needs arguments, consider using \code{juliaLet}
#' instead.
#'
#' @param expr Julia expression as a one-element character vector
#'
#' @return The value returned from Julia, translated to an R data structure.
#' If Julia returns \code{nothing}, an invisible \code{NULL} is returned.
#'
#' @examples
#' juliaEval("1 + 2")
#' juliaEval('using Pkg; Pkg.add("BoltzmannMachines")')
#' juliaEval('using Random; Random.seed!(5);')
#' \dontshow{
#' JuliaConnectoR:::stopJulia()
#' }
juliaEval <- function(expr) {
   ensureJuliaConnection()
   juliaCall("RConnector.mainevalcmd", expr)
}

#' Wrap a Julia function in an R function
#'
#' Creates an R function that will call the Julia function with the given name
#' when it is called. The returned function can also be passed as function argument
#' to Julia functions.
#'
#' @param name the name of the Julia function
#'
#' @examples
#' # Wrap a Julia function and use it
#' juliaSqrt <- juliaFun("sqrt")
#' juliaSqrt(2)
#' juliaCall("map", juliaSqrt, c(1,4,9))
#' \dontshow{
#' JuliaConnectoR:::stopJulia()
#' }
juliaFun <- function(name) {
   f <- function(...) {
      juliaCall(name, ...)
   }
   attr(f, "JLFUN") <- name
   return(f)
}

#' Mark a string as Julia expression
#'
#' A given R character vector is marked as a Julia expression.
#' It will be executed and evaluated when passed to Julia.
#' This allows to pass a Julia object that is defined by complex Julia syntax
#' as an argument without needing the round-trip to R via \code{\link{juliaEval}}
#' or \code{\link{juliaLet}}.
#'
#' @param expr a character vector which should contain one string
#'
#' @examples
#' # Create complicated objects like version strings in Julia and compare them
#' v1 <- juliaExpr('v"1.0.1"')
#' v2 <- juliaExpr('v"1.2.0"')
#' juliaCall("<", v1, v2)
#' \dontshow{
#' JuliaConnectoR:::stopJulia()
#' }
juliaExpr <- function(expr) {
   attr(expr, "JLEXPR") <- TRUE
   return(expr)
}


#' Evaluate Julia expressions in a `let` block, using R variables
#'
#' R variables can be passed as named arguments, which are inserted
#' for those variables in the Julia expression that have the same name
#' as the named arguments.
#' Returns the value of the `let` block, translated back to R.
#'
#' Note that, as the evaluation is done in a `let` block, changes to
#' global variables in the Julia session are only possible by using
#' the keyword `global` in front of the Julia variables.
#'
#' @param expr a Julia expression which may contain
#' @param ... the arguments to use in the let block.
#'   The names of the arguments are used in place of the variables in
#'   the Julia expression with the same name.
#'
#' @return The value returned from Julia, translated to an R data structure.
#' If Julia returns \code{nothing}, an invisible \code{NULL} is returned.
#'
#' @examples
#' # Assign a global variable (although not recommended for a functional style)
#' juliaLet("global x = xval", xval = rnorm(10))
#' juliaEval("x")
#'
#' juliaLet('[1 => x, 17 => y]', x = rnorm(1), y = rnorm(2))
#' \dontshow{
#' JuliaConnectoR:::stopJulia()
#' }
juliaLet <- function(expr, ...) {
   args <- list(...)
   if(length(which(names(args) == "")) > 0) {
      stop("Arguments must have names")
   } else {
      args <- c("RConnector.mainevallet", expr, args)
      do.call(juliaCall, args)
   }
}


handleCallbacksAndOutput <- function(callbacks) {
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
      } else if (messageType == STDOUT_INDICATOR) {
         readOutput(writeTo = stdout())
      } else if (messageType == STDERR_INDICATOR) {
         readOutput(writeTo = stderr())
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
