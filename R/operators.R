`$.JuliaReference` <- function(jlref, name) {
   juliaCall("RConnector.getprop", jlref, name)
}

`$<-.JuliaReference` <- function(jlref, name, value) {
   juliaCall("RConnector.setprop!", jlref, name, value)
   jlref
}
