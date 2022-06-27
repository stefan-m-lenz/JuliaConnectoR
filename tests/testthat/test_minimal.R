Sys.unsetenv("R_TESTS")


test_that("Some smoke tests", {
   expect_equal(juliaCall("prod", c(1,2,3)), 6)
   juliaEval("")
   juliaCall("string", list())
   juliaCall("string", list(as.integer(1), "bla" = 23L))
   juliaEval("String[]")
   expect(!is.null(juliaEval("using Random; Random.seed!(5)")),
          "Must be able to set random seed")
   expect(is.null(juliaEval("Random.seed!(5);")),
          "Eval with semicolon at end returns NULL")
})