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
TYPE_ID_ANONYMOUS_FUNCTION <- as.raw(0xaf)
TYPE_ID_NAMED_FUNCTION <- as.raw(0xfc)
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


# for holding local package variables:
# - the Julia connection object ("con")
# - the port where Julia is listening ("port")
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
   # problem: tryCatch has different environment and can't access pkgLocal
   con <- pkgLocal$con

   ret <- NULL

   tryCatch(
      {
         # the function call
         ret <- doCallJulia(name, jlargs)

         releaseFinalizedRefs()

      }, interrupt = function(e) {killJulia()})

   if (is.null(ret)) {
      return(invisible(NULL))
   } else {
      return(ret)
   }
}


doCallJulia <- function(name, jlargs) {
   writeBin(CALL_INDICATOR, pkgLocal$con)
   writeString(name)
   callbacks <- writeList(jlargs)
   messageType <- handleCallbacksAndOutput(callbacks)
   if (messageType == RESULT_INDICATOR) {
      return(readElement(callbacks))
   } else if (messageType == FAIL_INDICATOR) {
      stop(readString())
   } else {
      print(paste(c("Message type not supported (yet): ", messageType)))
      stopJulia()
   }
}


releaseFinalizedRefs <- function() {
   if (!is.null(pkgLocal$finalizedRefs)) {
      try({
         doCallJulia("RConnector.decrefcounts",
                     list(pkgLocal$finalizedRefs))
      })

      pkgLocal$finalizedRefs <- NULL
   }
}


#' Evaluate a Julia expression
#'
#' This function evaluates a Julia expression, given as a string, in Julia,
#' and translates the result back to R.
#'
#' If the expression needs arguments, consider using \code{juliaLet}
#' instead.
#'
#' If the Julia expression consists in fact of multiple expressions separated
#' by semicolons or newlines and a parsing error is shown,
#' consider wrapping it with \code{begin} ... \code{end}.
#'
#' @param expr Julia expression as a one-element character vector
#'
#' @return The value returned from Julia, translated to an R data structure.
#' If Julia returns \code{nothing}, an invisible \code{NULL} is returned.
#'
#' @examples
#' juliaEval("1 + 2")
#' \dontrun{
#' juliaEval('using Pkg; Pkg.add("BoltzmannMachines")')
#' }
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
#' when it is called. Like any R function, the returned function can
#' also be passed as a function argument to Julia functions.
#'
#' @param name the name of the Julia function
#' @param ... optional arguments for currying:
#'    The resulting function will be called using these arguments.
#'
#' @examples
#' # Wrap a Julia function and use it
#' juliaSqrt <- juliaFun("sqrt")
#' juliaSqrt(2)
#' # In the following call, the sqrt function is called without
#' # a callback to R because the linked function object is used.
#' juliaCall("map", juliaSqrt, c(1,4,9))
#'
#' # may also be used with arguments
#' plus1 <- juliaFun("+", 1)
#' plus1(2)
#' # Results in an R callback (calling Julia again)
#' # because there is no linked function object in Julia.
#' juliaCall("map", plus1, c(1,2,3))
#'
#'
#' \dontshow{
#' JuliaConnectoR:::stopJulia()
#' }
juliaFun <- function(name, ...) {
   args <- list(...)
   if (length(args) == 0) {
      f <- function(...) {
         juliaCall(name, ...)
      }
      # If passed to Julia, this function
      # can directly be translated to a Julia function.
      attr(f, "JLFUN") <- name
   } else {
      # This will be treated as a callback function,
      # if it is passed to Julia.
      f <- function(...) {
         do.call(juliaCall, c(name, args, list(...)))
      }
   }

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
#' # Create complicated objects like version strings in Julia, and compare them
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


#' Evaluate Julia code in a \code{let} block using values of R variables
#'
#' R variables can be passed as named arguments, which are inserted
#' for those variables in the Julia expression that have the same name
#' as the named arguments. The given Julia code is executed in Julia
#' inside a \code{let} block and the result is translated back to R.
#'
#' A simple, nonsensical example for explaining the principle:
#'
#' \code{juliaLet('println(x)', x = 1)}
#'
#' This is the same as
#'
#' \code{juliaEval('let x = 1.0; println(x) end')}
#'
#' More complex objects cannot be simply represented in a string like in
#' this simple example any more.
#' That is the problem that \code{juliaLet} solves.
#'
#' Note that the evaluation is done in a \code{let} block. Therefore,
#' changes to global variables in the Julia session are only possible by
#' using the keyword \code{global} in front of the Julia variables
#' (see examples).
#'
#' @param expr Julia code
#' @param ...  arguments that will be introduced as variables in the
#'   \code{let} block. The values are transferred to Julia and
#'   assigned to the variables introduced in the \code{let} block.
#'
#' @return The value returned from Julia, translated to an R data structure.
#' If Julia returns \code{nothing}, an invisible \code{NULL} is returned.
#'
#' @examples
#' # Intended use: Create a complex Julia object
#' # using Julia syntax and data from the R workspace
#' juliaLet('[1 => x, 17 => y]', x = rnorm(1), y = rnorm(2))
#'
#' # Assign a global variable
#' # (although not recommended for a functional style)
#' juliaLet("global x = xval", xval = rnorm(10))
#' juliaEval("x")
#'
#' \dontshow{
#' JuliaConnectoR:::stopJulia()
#' }
juliaLet <- function(expr, ...) {
   args <- list(...)
   if ((length(args) != 0 && is.null(names(args))) ||
       length(which(names(args) == "")) > 0) {
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


juliaHeapReference <- function(ref) {
   ret <- new.env(emptyenv())
   ret$ref <- ref
   reg.finalizer(ret, function(e) {
      # remove reference (see releaseFinalizedRefs)
      pkgLocal$finalizedRefs <- c(pkgLocal$finalizedRefs, e$ref)
   })
   ret
}

