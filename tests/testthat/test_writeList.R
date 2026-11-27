test_that("writeList() treats NA element names as unnamed positional elements", {
   ns <- asNamespace("JuliaConnectoR")
   pl <- ns$pkgLocal

   roundTripList <- function(x) {
      con <- rawConnection(raw(0), open = "r+b")
      oldCon <- pl$con
      on.exit({
         pl$con <- oldCon
         close(con)
      }, add = TRUE)
      pl$con <- con
      ns$writeList(x)
      seek(con, 0)
      ns$readList()
   }

   x <- list(1, 2)
   names(x)[1] <- "a"
   expect_identical(roundTripList(x), list(2, a = 1))

   y <- list(1, 2)
   names(y) <- c(NA_character_, NA_character_)
   expect_identical(roundTripList(y), list(1, 2))

   z <- list(1, 2, 3)
   names(z) <- c("", NA_character_, "c")
   expect_identical(roundTripList(z), list(1, 2, c = 3))
})
