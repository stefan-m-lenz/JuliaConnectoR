# TODO docu operators
# index: conversion to int
# doku item indexing. ?`$.data.frame` -> Extract.data.frame

`$.JuliaReference` <- function(jlref, name) {
   juliaCall("RConnector.getprop", jlref, name)
}

`$<-.JuliaReference` <- function(jlref, name, value) {
   juliaCall("RConnector.setprop!", jlref, name, value)
   jlref
}


`[.JuliaReference` <- function(jlref, ...) {
   do.call(juliaCall, c("RConnector.getidx", jlref, list(...)))
}

`[<-.JuliaReference` <- function(jlref, i, j, k, value) {
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

# TODO why does this not work instead?
# `[<-.JuliaReference` <- function(jlref, ..., value) {
#    do.call(juliaCall, c("RConnector.setidx!", jlref, value, list(...)))
#    jlref
# }


# TODO handle differently for arrays: numbered indexing
`[[.JuliaReference` <- function(jlref, name) {
   juliaCall("RConnector.getprop", jlref, name)
}

`[[<-.JuliaReference` <- function(jlref, name, value) {
   juliaCall("RConnector.setprop!", jlref, name, value)
   jlref
}


length.JuliaReference <- function(x) {
   juliaCall("length", x)
}


dim.JuliaReference <- function(x) {
   unlist(juliaCall("size", x))
}


print.JuliaReference <- function(x, ...) {
   cat(paste0("<Julia object of type ", juliaCall("typeof", x), ">\n",
              juliaCall("repr", x)))
}
