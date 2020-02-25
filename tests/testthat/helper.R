library(testthat)

juliaEcho <- function(x) juliaCall("identity", x)

jlRefsRemoved <- function(x) {
   if (is.list(x)) {
      if (!is.null(attr(x, "JLREF"))) {
         attr(x, "JLREF") <- NULL
      }
      for (i in seq_along(x)) {
         if (!is.null(x[[i]])) {
            x[[i]] <- jlRefsRemoved(x[[i]])
         }
      }
      return(x)
   } else if (is.environment(x) && inherits(x, "JuliaProxy")) {
      return(jlRefsRemoved(juliaGet(x)))
   } else {
      return(x)
   }
}


testEcho <- function(x, comparableInJulia = TRUE) {
   if (is.list(x)) {
      expect_identical(jlRefsRemoved(x), jlRefsRemoved(juliaEcho(x)))
   } else if (is.environment(x) && inherits(x, "JuliaProxy")) {
      if (comparableInJulia) {
         expect_true(juliaCall("==", x, juliaEcho(x)))
      }
      testEcho(juliaGet(x))
   } else {
      expect_equivalent(x, juliaEcho(x))
   }
}
