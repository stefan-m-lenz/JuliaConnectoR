test_that("Example for AccessMutate.JuliaProxy works", {
   # (Mutable) struct
   juliaEval("mutable struct MyStruct
                x::Int
             end")

   MyStruct <- juliaFun("MyStruct")
   s <- MyStruct(1L)
   s$x
   s$x <- 2
   expect_equal(s[["x"]], 2)


   # Array
   x <- juliaCall("map", MyStruct, c(1L, 2L, 3L))
   x
   expect_equal(length(x), 3)
   x[[1]]
   x[[1]]$x
   x[[1]] <- MyStruct(2L)
   x[2:3]
   x[2:3] <- MyStruct(2L)
   expect_equal(juliaLet("[myx.x for myx in x]", x = x), c(2,2,2))

   # Tuple
   x <- juliaEval("(1, 2, 3)")
   expect_equal(x[[1]], 1)
   expect_s3_class(x[1:2], "JuliaProxy")
   expect_equal(length(x), 3)

   # NamedTuple
   x <- juliaEval("(a=1, b=2)")
   expect_equal(x$a, 1)

   # Dictionary
   strDict <- juliaEval('Dict("hi" => 1, "hello" => 2)')
   strDict
   expect_equal(strDict$hi, 1)
   strDict$hi <- 0
   strDict[["hi"]] <- 2
   strDict["howdy", "greetings"] <- c(2, 3)
   expect_equal(strDict["hi", "howdy"], list(2,2))

})
