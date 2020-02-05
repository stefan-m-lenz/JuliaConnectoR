`$.JuliaReference` <- function(jlref, name) {
   juliaCall("RConnector.getprop", jlref, name)
}

`$<-.JuliaReference` <- function(jlref, name, value) {
   juliaCall("RConnector.setprop!", jlref, name, value)
   jlref
}

asJuliaIndexes <- function(indexes) {
   if (all(unlist(lapply(indexes, is.double)))) {
      indexes <- lapply(indexes, as.integer)
   }
   indexes
}

`[[.JuliaReference` <- function(jlref, ...) {
   indexes <- asJuliaIndexes(list(...))
   do.call(juliaCall, c("getindex", jlref, indexes))
}

`[[<-.JuliaReference` <- function(jlref, value, ...) {
   indexes <- asJuliaIndexes(list(...))
   do.call(juliaCall, c("setindex!", jlref, value, indexes))
   jlref
}


length.JuliaReference <- function(x) {
   juliaCall("length", x)
}

print.JuliaReference <- function(x) {
   cat(paste0("<Julia object>\n", juliaCall("repr", x)))
}
