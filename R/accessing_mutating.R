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
#' @name AccessOrMutate.JuliaObject
#' @examples
#' #TODO
#' #juliaEval("bla")
NULL


# TODO docu operators
# index: conversion to int
# doku item indexing. ?`$.data.frame` -> Extract.data.frame

#' @rdname AccessOrMutate.JuliaObject
`$.JuliaStruct` <- function(x, name) {
   juliaCall("RConnector.getprop", x, name)
}

#' @rdname AccessOrMutate.JuliaObject
`$<-.JuliaStruct` <- function(x, name, value) {
   juliaCall("RConnector.setprop!", x, name, value)
   x
}


#' @rdname AccessOrMutate.JuliaObject
`[.JuliaObject` <- function(x, ...) {
   ret <- do.call(juliaCall, c("RConnector.getidxs", x, list(...)))
   if (!is.list(ret) && !inherits(ret, "JuliaObject")) {
      return(list(ret)) # compatibility with translated behaviour of translated objects
   } else {
      return(ret)
   }
}

#' @rdname AccessOrMutate.JuliaObject
`[<-.JuliaObject` <- function(x, i, j, k, value) {
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

# TODO [] auf arrays: so wie liste?

#' @rdname AccessOrMutate.JuliaObject
`[[.JuliaArray` <- function(x, ...) {
   do.call(juliaCall, c("RConnector.getidx", x, list(...)))
}

#' @rdname AccessOrMutate.JuliaObject
`[[<-.JuliaArray` <- function(x, i, j, k, value) {
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


#' @rdname AccessOrMutate.JuliaObject
`[[.JuliaStruct` <- function(x, name) {
   juliaCall("RConnector.getprop", x, name)
}

#' @rdname AccessOrMutate.JuliaObject
`[[<-.JuliaStruct` <- function(x, name, value) {
   juliaCall("RConnector.setprop!", x, name, value)
   x
}


length.JuliaArray <- function(x) {
   juliaCall("length", x)
}


dim.JuliaArray <- function(x) {
   unlist(juliaCall("size", x))
}


print.JuliaObject <- function(x, ...) {
   cat(paste0("<Julia object of type ", juliaCall("typeof", x), ">\n",
              juliaCall("repr", x)))
}

