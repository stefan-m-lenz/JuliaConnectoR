library(JuliaConnectoR)
library(testthat)

juliaEcho <- function(x) juliaCall("identity", x) # TODO more tests with all kinds of datatypes
testEcho <- function(x) {
   if(is.list(x)) {
      expect_identical(x, juliaEcho(x))
   } else {
      e = juliaEcho(x)
      expect_equivalent(x, juliaEcho(x))
   }
}


# Should work
test_that("Some smoke tests", {
   expect((juliaCall("prod", c(1,2,3)) == 6), "Product")
   juliaCall("show", NULL)
   juliaCall("string", list())
   juliaCall("println", "hello world")
   juliaCall("string", list(as.integer(1), "bla" = 23L))
   juliaEval("String[]")
   juliaCall("eval", "println(22)")
   expect(!is.null(juliaEval("using Random; Random.seed!(5)")),
          "Must be able to set random seed")
   expect(is.null(juliaEval("Random.seed!(5);")),
          "Eval with semicolon at end returns NULL")
})


test_that("Test loading and importing a complex package", {
   juliaLet('using Pkg;
             if !haskey(Pkg.installed(), "StatsBase")
               Pkg.add("StatsBase")
             end
             ') # TODO does not work with eval
   juliaImport("StatsBase")
   expectedMeanAndVar <- list(2,1)
   attr(expectedMeanAndVar, "JLTYPE") <- "Tuple{Float64, Float64}"
   expect(all(as.numeric(StatsBase.mean_and_var(c(1,2,3))) == c(2,1)),
          "Trivial mean and var calculation")
   StatsBase.renyientropy(rnorm(100), 1)
})



# Nested callback functions
juliaEval('struct TestStruct f::Function end')
juliaEval('function testNestedFun(ts::Vector{TestStruct}) map(t -> t.f(), ts) end')
t <- juliaCall("testNestedFun",
               list(juliaCall("TestStruct", function() {print("bla")}),
                    juliaCall("TestStruct", function() {print("blup")}),
                    juliaCall("TestStruct", function() {print("blip")})))

juliaEval('function testNestedFunNamed(;ts::Vector{TestStruct}) map(t -> t.f(), ts) end')
t <- juliaCall("testNestedFunNamed", ts =
               list(juliaCall("TestStruct", function() {print("bla")}),
                    juliaCall("TestStruct", function() {print("blup")})))


# test repeated call of nested functions
juliaEval('function testNestedFun2(ts::Vector{TestStruct}) for i = 1:2 map(t -> t.f(), ts) end end')
t <- juliaCall("testNestedFun2",
               list(juliaCall("TestStruct", function() {print("bla")}),
                    juliaCall("TestStruct", function() {print("blup")})))


juliaEval('function testNestedFunArgs(ts::Vector{TestStruct}, args...) map(t -> t.f(args...), ts) end')
t <- juliaCall("testNestedFunArgs",
               list(juliaCall("TestStruct", function(x) {print(x)}),
                    juliaCall("TestStruct", function(x, y) {print(x); return(c(5,6,7))})),
               c(1, 2, 3))

juliaEval('function testNestedAndUnnested(f::Function, ts::Vector{TestStruct}, args...)
            testNestedFunArgs(ts, args...)
            f(args...)
          end')
t <- juliaCall("testNestedAndUnnested",
               function(x) {print("1"); print(x)},
               list(juliaCall("TestStruct", function(x) {print("2"); print(x)}),
                    juliaCall("TestStruct", function(x, y) {print("3") ;print(x); return(c(5,6,7))})),
               list(x = 1, 2, 3)) # TODO try with function with error



test_that("Echo: empty R vector", {testEcho(c())})

test_that("Echo: double", {
   expect(is.integer(juliaEcho(1L)), "Must be integer")
   testEcho(1L)
})
test_that("Echo: Single Int", {
   testEcho(1L)
   expect(is.integer(juliaEval("1")), "1 is not integer")
   testEcho(juliaEval("1"))
   expect(is.double(juliaEval("2^52")), "2^52 is not double") # TODO inexactness?
   testEcho(juliaEval("2^52"))
})
test_that("Echo: 1-element vector of Int in Julia", {testEcho(juliaEval("[1]"))})
test_that("Echo: Matrix of Int", {testEcho(matrix(1:6, nrow = 2))})

test_that("Echo: Single Float64", {
   testEcho(juliaEval("1.0"))
   testEcho(1)
})

test_that("Echo: 0-element vector of Float64 in Julia", {testEcho(juliaEval("Float64[]"))})
test_that("Echo: 1-element vector of Float64 in Julia", {testEcho(juliaEval("[1.0]"))})
test_that("Echo: 2-element vector of Float64/double", {
   testEcho(c(1, 2))
   testEcho("[1.0; 2.0]")
})
test_that("Echo: matrix of Float64 in Julia", {
   testEcho(juliaEval("[1.0  2.0; 3.0 4.0]"))
   testEcho(matrix(c(1,2,3,4,4,5), nrow = 2))
})

test_that("Echo: 1-element vector of Float32 in Julia", {testEcho(juliaEval("[1.0f0]"))})
test_that("Echo: matrix of Float32 in Julia", {testEcho(juliaEval("[1.0f0  2.0f0; 3.0f0 4.0f0]"))})

test_that("Echo: 1-element vector of String in Julia", {testEcho(juliaEval('String["bla"]'))})
test_that("Echo: 0-element vector of String in Julia", {testEcho("String[]")})
test_that("Echo: 1-element vector of String in R", {testEcho("bla")})
test_that("Echo: 2-element vector of String in R", {testEcho(c("bla", "blup"))})
test_that("Echo: 2-element vector of String in Julia", {testEcho(juliaEval('String["bla", "blup"]'))})

test_that("Echo: logical vectors", {
   testEcho(juliaEval('Bool[]'))
   testEcho(logical())
   testEcho(TRUE)
   testEcho(FALSE)
   testEcho(c(TRUE, FALSE))
   testEcho("Bool[true]")
})

test_that("Complex are handled first class", {
   testEcho(1i)
   testEcho(juliaEval("1+im"))
   testEcho(juliaEval("[1+im]"))
   testEcho(juliaEval("[1+im; 2+im]"))
   testEcho(c(1+1i,2 + 2i))

   juliaImport("LinearAlgebra", alias = "jla")
   testEcho(matrix(c(1, 0, 0, -1), ncol = 2))
   expect(all(jla.eigvals(matrix(c(1, 0, 0, -1), ncol = 2),
                   matrix(c(0, 1, 1, 0), ncol = 2)) == c(1i, -1i)),
          "")
   expect(all(jla.eigmax(matrix(c(0, 1i, -1i, 0), ncol = 2)) == 1.0), "")
})


#TODO raw
juliaCall("string", c(as.raw(0xca), as.raw(0xfe)))

# TODO Int32

test_that("Echo: Single Int16", {testEcho(juliaEval('Int16(300)'))})
test_that("Echo: Int16 Vector", {testEcho(juliaEval('Int16[1,2,3]'))})
test_that("Echo: 1-element Int16 Vector", {
   expect(juliaCall("string", juliaEval('Int16[300]')) ==
      "Int16[300]", "Failed")
})


test_that("Echo: Single UInt128", {testEcho(juliaEval('UInt128(2)^100 +1'))})
test_that("Echo: UInt128 Vector", {testEcho(juliaEval('UInt128[1,2,3]'))})
test_that("Echo: 1-element UInt128 Vector", {
   expect(juliaCall("string", juliaEval('UInt128[1]')) ==
         "UInt128[0x00000000000000000000000000000001]", "Failed")
})

# Test Let
test_that("let: used like eval", {expect(is.null(juliaLet("print(1)")), "Failed")})
test_that("Let: must error with no named argument", {expect_error(juliaLet("print(x)", 1), "")})
test_that("Let: basic echo", {expect(all(juliaLet("identity(x)", x=c(2, 3)) == c(2,3)), "Failed")})


#Test Pairs
testEcho(juliaEval("(1 => 2.0)"))
testEcho(juliaEval("1 => 2.0 => 3.0"))
testEcho(juliaEval("[1 => 2.0, 2 => 3.0]"))

# Test Tuples
testEcho(juliaEval("(1, 2.0)"))
testEcho(juliaEval("((1, 2.0), 3.0)"))
testEcho(juliaLet("collect(zip(x,y))", x = c(1L,2L, 3L), y = c(1,0,1)))

# Test Named Tuples
namedTuple <- juliaLet("y=2*x; z = 3*u + 1; (x=y, y=z)", x=2, u=4)
identical(juliaEcho(namedTuple), namedTuple)

# Test Module
juliaEval("module TestModule end")
testEcho(juliaEval("TestModule"))

# Test Dictionary
d <- juliaEval("Dict(:bla => 1.0, :blup => 3.0)")
d$keys
d$values
identical(juliaEcho(d), d)
d <- juliaLet("Dict(zip(x, y))", x = c("bla", "blup"), y = c(1,2))
d$keys
d$values
identical(juliaEcho(d), d)
d <- juliaLet("Dict(zip(x, y))", x = list("bla"), y = list(1))
d$keys
d$values
identical(juliaEcho(d), d)
d <- juliaLet("Dict(zip(x, y))", x = list(), y = list())
identical(juliaEcho(d), d)

# Test Set
s1 <- juliaEval("Set([1; 2; 3; 4])")
s2 <- juliaEval("Set([1; 2])")
length(setdiff(juliaEval("Set([1; 2; 3; 4])"), c(1,2,3,4))) == 0
length(setdiff(juliaLet("setdiff(s1, s2)", s1 = s1, s2 = s2), c(3,4))) == 0
identical(s1, juliaEcho(s1))

# Test types with bitstypes
juliaUsing("UUIDs")
juliaCall("string", uuid4())


# Test struct with private constructor
juliaEval('struct MyPrivateX
            x::Int
            function MyPrivateX()
               new(5)
            end
          end')
p <- juliaEval("MyPrivateX()")
testEcho(p)


test_that("Errors are handled gracefully", {
   expect_error(juliaCall("sum", c(1,2,3, "bla")))
   expect_error(juliaCall("thisisnotarealfunction", 100, 4))
   expect_error(juliaCall("RConnector.thisisnotarealfunction", "haha"))
   expect_error(juliaCall("NotARealModule.thisisnotarealfunction", list(1,2,3)))
})




