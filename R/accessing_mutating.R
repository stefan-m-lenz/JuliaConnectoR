#' Access or mutate Julia objects
#'
#' @description
#' Extract or replace parts of Julia proxy objects
#'
#' @name AccessOrMutate.JuliaObject
#' @examples
#' #TODO
#' #juliaEval("bla")
NULL


# TODO docu operators
# index: conversion to int
# doku item indexing. ?`$.data.frame` -> Extract.data.frame

#' @rdname AccessOrMutate.JuliaObject
`$.JuliaStruct` <- function(jlref, name) {
   juliaCall("RConnector.getprop", jlref, name)
}

#' @rdname AccessOrMutate.JuliaObject
`$<-.JuliaStruct` <- function(jlref, name, value) {
   juliaCall("RConnector.setprop!", jlref, name, value)
   jlref
}


#' @rdname AccessOrMutate.JuliaObject
`[.JuliaObject` <- function(jlref, ...) {
   ret <- do.call(juliaCall, c("RConnector.getidxs", jlref, list(...)))
   if (!is.list(ret) && !inherits(ret, "JuliaObject")) {
      return(list(ret)) # compatibility with translated behaviour of translated objects
   } else {
      return(ret)
   }
}

#' @rdname AccessOrMutate.JuliaObject
`[<-.JuliaObject` <- function(jlref, i, j, k, value) {
   if (missing(k)) {
      if (missing(j)) {
         juliaCall("RConnector.setidxs!", jlref, value, i)
      } else {
         juliaCall("RConnector.setidxs!", jlref, value, i, j)
      }
   } else {
      juliaCall("RConnector.setidxs!", jlref, value, i, j, k)
   }
   jlref
}

# TODO [] auf arrays: so wie liste?

#' @rdname AccessOrMutate.JuliaObject
`[[.JuliaArray` <- function(jlref, ...) {
   do.call(juliaCall, c("RConnector.getidx", jlref, list(...)))
}

#' @rdname AccessOrMutate.JuliaObject
`[[<-.JuliaArray` <- function(jlref, i, j, k, value) {
   if (missing(k)) {
      if (missing(j)) {
         juliaCall("RConnector.setidx!", jlref, value, i)
      } else {
         juliaCall("RConnector.setidx!", jlref, value, i, j)
      }
   } else {
      juliaCall("RConnector.setidx!", jlref, value, i, j, k)
   }
   jlref
}


#' @rdname AccessOrMutate.JuliaObject
`[[.JuliaStruct` <- function(jlref, name) {
   juliaCall("RConnector.getprop", jlref, name)
}

#' @rdname AccessOrMutate.JuliaObject
`[[<-.JuliaStruct` <- function(jlref, name, value) {
   juliaCall("RConnector.setprop!", jlref, name, value)
   jlref
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

