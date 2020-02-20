#' Access or mutate Julia objects
#'
#' @description
#' Extract or replace parts of Julia proxy objects
#'
#' @param x a Julia proxy object
#' @param i,j,k,... indexes for specifying the elements to extract or replace
#' @param name a character string (TODO symbol) giving the name of a property
#'   of a struct type or a key in a Julia dictionary (type \code{AbstractDict})
#' @param value a suitable replacement value.
#'    When replacing a range of elements in an array type, it is possible to
#'    replace multiple elements with single elements. In all other cases,
#'    the length of the replacement must match the length of elements to replace.
#' @name AccessMutate.JuliaProxy
#' @examples
#' #TODO
#' #juliaEval("bla")
NULL

# TODO docu operators
# index: conversion to int

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
   ret <- do.call(juliaCall, c("RConnector.getidxs", x, list(...)))
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
   do.call(juliaCall, c("RConnector.getidx", x, list(...)))
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

