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
TYPE_ID_OBJECT_REFERENCE <- as.raw(0xce)
TYPE_ID_EXPRESSION <- as.raw(0xee)
TYPE_ID_SYMBOL <- as.raw(0x5b)

OBJECT_CLASS_ID_ARRAY <- as.raw(0xaa)
OBJECT_CLASS_ID_SIMPLE_ARRAY <- as.raw(0x5a)
OBJECT_CLASS_ID_ANONYMOUS_FUNCTION <- as.raw(0xaf)
OBJECT_CLASS_ID_STRUCT <- as.raw(0x5c)
OBJECT_CLASS_ID_NO_INFO <- as.raw(0x00)


CALL_INDICATOR <- as.raw(0x01)
RESULT_INDICATOR <- as.raw(0x00)
STDOUT_INDICATOR <- as.raw(0x50)
STDERR_INDICATOR <- as.raw(0x5e)
FAIL_INDICATOR <- as.raw(0xff)
BYEBYE <- as.raw(0xbb)


TYPE_IDS <- list(
   "double" = TYPE_ID_DOUBLE,
   "complex" = TYPE_ID_COMPLEX,
   "raw" = TYPE_ID_RAW,
   "integer" = TYPE_ID_INTEGER,
   "logical" = TYPE_ID_LOGICAL,
   "character" = TYPE_ID_STRING,
   "list" = TYPE_ID_LIST,
   "symbol" = TYPE_ID_SYMBOL)


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
# - the last callback id ("lastCallbackId")
# - references to callback functions passed to Julia ("callbacks")
pkgLocal <- new.env(parent = emptyenv())
pkgLocal$callbacks <- new.env(parent = emptyenv())
pkgLocal$lastCallbackId <- -1L


registerCallback <- function(callback) {
   while (TRUE) {
      pkgLocal$lastCallbackId <- (pkgLocal$lastCallbackId + 1L) %% 2147483647L
      callbackId <- as.character(pkgLocal$lastCallbackId)
      if (is.null(pkgLocal$callbacks[[callbackId]])) {
         break
      }
   }
   assign(envir = pkgLocal$callbacks, callbackId, callback)
   callbackId
}


.onLoad <- function(libname, pkgname) {
   # register finalizer to stop Julia connection
   parent <- parent.env(environment())
   reg.finalizer(parent, finalize, onexit = TRUE)
}


finalize <- function(env) {
   stopJulia()
}


#' Call a Julia function by name
#'
#' Call a Julia function via specifying the name as string and get the translated result.
#' It is also possible to use a dot at the end of the function name
#' for applying the function in a vectorized manner via "broadcasting" in Julia.
#'
#' @param ... the name of the Julia function as first argument, followed by the
#' parameters handed to the function.
#' All arguments to the Julia function are translated to Julia data structures.
#'
#' @examples
#' if (juliaSetupOk()) {
#'
#'    juliaCall("/", 4, 2)
#'    juliaCall("Base.div", 4, 2)
#'    juliaCall("sin.", c(1,2,3))
#'    juliaCall("Base.cos.", c(1,2,3))
#'
#' }
#' \dontshow{
#' JuliaConnectoR:::stopJulia()
#' }
#'
#' @return The value returned from Julia, translated to an R data structure.
#' If Julia returns \code{nothing}, an invisible \code{NULL} is returned.
juliaCall <- function(...) {
   ensureJuliaConnection()

   args <- list(...)
   funName <- unlist(args[1])
   if (is.null(funName) || !is.character(funName) || length(funName) != 1) {
      stop("Name of Julia function must be specified as one-element character vector")
   }

   jlargs <- args[-1]

   # problem: tryCatch has different environment and can't access pkgLocal
   # therefore, use con object in this environment
   con <- pkgLocal$con

   ret <- NULL

   tryCatch(
      {
         # the function call
         ret <- doCallJulia(funName, jlargs)

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
   writeList(jlargs)
   messageType <- handleCallbacksAndOutput()
   if (messageType == RESULT_INDICATOR) {
      return(readElement())
   } else if (messageType == FAIL_INDICATOR) {
      errorMsg <- readString()
      stop(errorMsg, call. = FALSE, domain = NA)
   } else {
      print(paste(c("Message type not supported (yet): ", messageType)))
      stopJulia()
   }
}


releaseFinalizedRefs <- function() {
   if (!is.null(pkgLocal$finalizedRefs)) {
      try({
         callbackrefs <- doCallJulia("RConnector.decrefcounts",
                                     list(pkgLocal$communicator,
                                          pkgLocal$finalizedRefs))
         rm(envir = pkgLocal$callbacks, list = callbackrefs)
      })

      pkgLocal$finalizedRefs <- NULL
   }
}


#' Evaluate a Julia expression
#'
#' This function evaluates Julia code, given as a string, in Julia,
#' and translates the result back to R.
#'
#' If the code needs to use R variables, consider using \code{juliaLet}
#' instead.
#'
#' @param expr Julia code, given as a one-element character vector
#'
#' @return The value returned from Julia, translated to an R data structure.
#' If Julia returns \code{nothing}, an invisible \code{NULL} is returned.
#' This is also the case if the last non-whitespace character of \code{expr}
#' is a semicolon.
#'
#' @examples
#' if (juliaSetupOk()) {
#'
#'    juliaEval("1 + 2")
#'    juliaEval('using Pkg; Pkg.add("BoltzmannMachines")')
#'    juliaEval('using Random; Random.seed!(5);')
#'
#' }
#'
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
#' if (juliaSetupOk()) {
#'
#'    # Wrap a Julia function and use it
#'    juliaSqrt <- juliaFun("sqrt")
#'    juliaSqrt(2)
#'    # In the following call, the sqrt function is called without
#'    # a callback to R because the linked function object is used.
#'    juliaCall("map", juliaSqrt, c(1,4,9))
#'
#'    # may also be used with arguments
#'    plus1 <- juliaFun("+", 1)
#'    plus1(2)
#'    # Results in an R callback (calling Julia again)
#'    # because there is no linked function object in Julia.
#'    juliaCall("map", plus1, c(1,2,3))
#'
#' }
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
         do.call(juliaCall, quote = TRUE, c(name, args, list(...)))
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
#' if (juliaSetupOk()) {
#'
#'    # Create complicated objects like version strings in Julia, and compare them
#'    v1 <- juliaExpr('v"1.0.1"')
#'    v2 <- juliaExpr('v"1.2.0"')
#'    juliaCall("<", v1, v2)
#'
#' }
#'
#' \dontshow{
#' JuliaConnectoR:::stopJulia()
#' }
juliaExpr <- function(expr) {
   attr(expr, "JLEXPR") <- TRUE
   return(expr)
}


#' Translate a Julia proxy object to an R object
#'
#' R objects of class \code{JuliaProxy} are references to Julia objects in the Julia session.
#' These R objects are also called "proxy objects".
#' With this function it is possible to translate these objects into R objects.
#'
#' If the corresponding Julia objects do not contain external references,
#' translated objects can also saved in R and safely be restored in Julia.
#'
#' Modifying objects is possible and changes in R will be translated back to Julia.
#'
#' The following table shows the translation of Julia objects into R objects.
#'
#' \tabular{lcl}{
#' \strong{Julia} \tab  \tab \strong{R} \cr
#'  \code{struct} \tab \eqn{\rightarrow}{-->} \tab \code{list} with the named struct elements \cr
#'  \code{Array} of \code{struct} type \tab \eqn{\rightarrow}{-->} \tab \code{list} (of \code{list}s) \cr
#'  \code{Tuple} \tab \eqn{\rightarrow}{-->} \tab \code{list} \cr
#'  \code{NamedTuple} \tab \eqn{\rightarrow}{-->} \tab \code{list} with the named elements \cr
#'  \code{AbstractDict} \tab \eqn{\rightarrow}{-->} \tab \code{list} with two sub-lists: "\code{keys}" and "\code{values}" \cr
#'  \code{AbstractSet} \tab \eqn{\rightarrow}{-->} \tab \code{list} \cr
#' }
#'
#' @note
#'
#' Objects containing cicular references cannot be translated back to Julia.
#'
#' It is safe to translate objects that contain external references from Julia to R.
#' The pointers will be copied as values and the finalization of the translated
#' Julia objects is prevented.
#' The original objects are garbage collected after all direct or
#' indirect copies are garbage collected.
#' Note, however, that these translated objects cannot be translated back to Julia
#' after the Julia process has been stopped and restarted.
#'
#' @param x a reference to a Julia object
juliaGet <- function(x) {
   UseMethod("juliaGet", x)
}

juliaGet.JuliaProxy <- function(x) {
   juliaCall("RConnector.full_translation!", pkgLocal$communicator, TRUE)
   ret <- NULL
   tryCatch({ret <- juliaCall("identity", x)},
       finally = {juliaCall("RConnector.full_translation!",
                            pkgLocal$communicator, FALSE)})
   ret
}

#' Create a Julia proxy object from an R object
#'
#' This function can be used to copy R vectors and matrices to Julia and keep
#' them there.
#' The returned proxy object can be used in place of the original vector or
#' matrix.
#' This is useful to prevent that large R vectors / matrices
#' are repeatedly translated when using an object in multiple calls to Julia.
#'
#' @param x an R object (can also be a translated Julia object)
#'
#' @examples
#' if (juliaSetupOk()) {
#'
#'    # Transfer a large vector to Julia and use it in multiple calls
#'    x <- juliaPut(rnorm(100))
#'    # x is just a reference to a Julia vector now
#'    juliaEval("using Statistics")
#'    juliaCall("mean", x)
#'    juliaCall("var", x)
#'
#' }
#'
#' \dontshow{
#' JuliaConnectoR:::stopJulia()
#' }
juliaPut <- function(x) {
   if (inherits(x, "JuliaProxy")) {
      stop("Argument is already a Julia object")
   } else if (is.list(x) && !is.null(attr(x, "JLTYPE"))) {
      # a translated julia object
      juliaCall("identity", x)
   } else {
      juliaCall("RConnector.EnforcedProxy", x)
   }
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
#' @param expr Julia code, given as one-element character vector
#' @param ...  arguments that will be introduced as variables in the
#'   \code{let} block. The values are transferred to Julia and
#'   assigned to the variables introduced in the \code{let} block.
#'
#' @return The value returned from Julia, translated to an R data structure.
#' If Julia returns \code{nothing}, an invisible \code{NULL} is returned.
#'
#' @examples
#' if (juliaSetupOk()) {
#'
#'    # Intended use: Create a complex Julia object
#'    # using Julia syntax and data from the R workspace
#'    juliaLet('[1 => x, 17 => y]', x = rnorm(1), y = rnorm(2))
#'
#'    # Assign a global variable
#'    # (although not recommended for a functional style)
#'    juliaLet("global x = xval", xval = rnorm(10))
#'    juliaEval("x")
#'
#' }
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
      do.call(juliaCall, args, quote = TRUE)
   }
}


handleCallbacksAndOutput <- function() {
   repeat {
      messageType <- readMessageType()
      if (messageType == CALL_INDICATOR) {
         call <- readCall()
         callbackfun <- get(call$name, pkgLocal$callbacks)
         callbackSuccess <- FALSE
         tryCatch({
            answerCallback(callbackfun, call$args)
            callbackSuccess <- TRUE
            }, error = function(e) {
               writeFailMessage(as.character(e))
            })
         if (!callbackSuccess) {
            return(readMessageType())
         }
      } else if (messageType == STDOUT_INDICATOR) {
         readOutput(writeTo = stdout())
      } else if (messageType == STDERR_INDICATOR) {
         readOutput(writeTo = stderr())
      } else if (messageType == FAIL_INDICATOR) {
         errorMsg <- readString()
         stop(errorMsg, call. = FALSE, domain = NA)
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
      pkgLocal$finalizedRefs <- c(pkgLocal$finalizedRefs, get("ref", e))
   })
   ret
}

