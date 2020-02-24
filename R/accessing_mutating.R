#' Access or mutate Julia objects via proxy objects
#'
#' @description
#' Apply the R operators \code{$} and \code{$<-}, \code{[} and \code{[<-}, \code{[[}
#' and \code{[[<-} to access or modify parts of Julia objects via their proxy objects.
#' For an intuitive understanding, best see the examples below.
#'
#' @details
#' The \code{$} operator allows to access properties of Julia \code{struct}s
#' and \code{NamedTuple}s via their proxy objects.
#' For dictionaries (Julia type \code{AbstractDict}), the \code{$} operator
#' can also be used to look up string keys.
#' Fields of \code{mutable struct}s and dictionary elements with string keys
#' can be set via the \code{$<-} operator.
#'
#' The \code{[}, \code{[<-}, \code{[[}, and \code{[[<-}
#' operators relay to the \code{getindex} and \code{setindex!} Julia functions for
#' collections.
#' The \code{[[} and \code{[[<-} operators are used to access or mutate a single element.
#' With \code{[} and \code{[<-}, a range of objects is accessed or mutated.
#'
#' The dimensions of proxy objects for Julia \code{AbstractArray}s and \code{Tuple}s
#' can be queried via \code{length} and \code{dim}.
#'
#'
#' @param x a Julia proxy object
#' @param i,j,k,... index(es) for specifying the elements to extract or replace
#' @param name the field of a struct type, the name of a member in a \code{NamedTuple},
#'  or a key in a Julia dictionary (type \code{AbstractDict})
#' @param value a suitable replacement value.
#'    When replacing a range of elements in an array type, it is possible to
#'    replace multiple elements with single elements. In all other cases,
#'    the length of the replacement must match the number of elements to replace.
#' @name AccessMutate.JuliaProxy
#' @examples
#' # (Mutable) struct
#' juliaEval("mutable struct MyStruct
#'              x::Int
#'           end")
#'
#' MyStruct <- juliaFun("MyStruct")
#' s <- MyStruct(1L)
#' s$x
#' s$x <- 2
#' s$x
#'
#' # Array
#' x <- juliaCall("map", MyStruct, c(1L, 2L, 3L))
#' x
#' length(x)
#' x[[1]]
#' x[[1]]$x
#' x[[1]] <- MyStruct(2L)
#' x[2:3]
#' x[2:3] <- MyStruct(2L)
#' x
#'
#' # Tuple
#' x <- juliaEval("(1, 2, 3)")
#' x[[1]]
#' x[1:2]
#' length(x)
#'
#' # NamedTuple
#' x <- juliaEval("(a=1, b=2)")
#' x$a
#'
#' # Dictionary
#' strDict <- juliaEval('Dict("hi" => 1, "hello" => 2)')
#' strDict
#' strDict$hi
#' strDict$hi <- 0
#' strDict[["hi"]] <- 2
#' strDict["howdy", "greetings"] <- c(2, 3)
#' strDict["hi", "howdy"]
#' \dontshow{
#' JuliaConnectoR:::stopJulia()
#' }
NULL


#' @rdname AccessMutate.JuliaProxy
`$.JuliaStructProxy` <- function(x, name) {
   juliaCall("RConnector.getprop", x, name)
}

#' @rdname AccessMutate.JuliaProxy
`$<-.JuliaStructProxy` <- function(x, name, value) {
   juliaCall("RConnector.setprop!", x, name, value)
   x
}


#' @rdname AccessMutate.JuliaProxy
`[.JuliaProxy` <- function(x, ...) {
   ret <- do.call(juliaCall, quote = TRUE, c("RConnector.getidxs", x, list(...)))
   if (!is.list(ret) && !inherits(ret, "JuliaProxy")) {
      return(as.list(ret)) # compatibility with translated behaviour of translated objects
   } else {
      return(ret)
   }
}

#' @rdname AccessMutate.JuliaProxy
`[<-.JuliaProxy` <- function(x, i, j, k, value) {
   if (missing(k)) {
      if (missing(j)) {
         juliaCall("RConnector.setidxs!", x, value, i)
      } else {
         juliaCall("RConnector.setidxs!", x, value, i, j)
      }
   } else {
      juliaCall("RConnector.setidxs!", x, value, i, j, k)
   }
   x
}


#' @rdname AccessMutate.JuliaProxy
`[[.JuliaArrayProxy` <- function(x, ...) {
   do.call(juliaCall, quote = TRUE, c("RConnector.getidx", x, list(...)))
}

#' @rdname AccessMutate.JuliaProxy
`[[<-.JuliaArrayProxy` <- function(x, i, j, k, value) {
   if (missing(k)) {
      if (missing(j)) {
         juliaCall("RConnector.setidx!", x, value, i)
      } else {
         juliaCall("RConnector.setidx!", x, value, i, j)
      }
   } else {
      juliaCall("RConnector.setidx!", x, value, i, j, k)
   }
   x
}


#' @rdname AccessMutate.JuliaProxy
`[[.JuliaStructProxy` <- function(x, name) {
   juliaCall("RConnector.getprop", x, name)
}

#' @rdname AccessMutate.JuliaProxy
`[[<-.JuliaStructProxy` <- function(x, name, value) {
   juliaCall("RConnector.setprop!", x, name, value)
   x
}


#' @rdname AccessMutate.JuliaProxy
length.JuliaArrayProxy <- function(x) {
   juliaCall("length", x)
}


#' @rdname AccessMutate.JuliaProxy
dim.JuliaArrayProxy <- function(x) {
   juliaCall("RConnector.getdim", x)
}


print.JuliaProxy <- function(x, ...) {
   cat(paste0("<Julia object of type ", juliaCall("typeof", x), ">\n",
              juliaCall("repr", x)))
}

