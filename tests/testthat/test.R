juliaEcho <- function(x) juliaCall("identity", x)
testEcho <- function(x) {
   if(is.list(x)) {
      expect_identical(x, juliaEcho(x))
   } else {
      expect_equivalent(x, juliaEcho(x))
   }
}


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


test_that("Example for juliaEval runs", {
   v1 <- juliaExpr('v"1.0.5"')
   v2 <- juliaExpr('v"1.3.0"')
   expect(juliaCall("<", v1, v2), "Comparison must work")
})


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

test_that("UInt32, Float16, and Float32 translated to doubles in R", {
   toTest <- c("Float16(1.5)", "Float16[]", "Float16[1.5]",
               "Float32(1.5)", "Float32[]", "Float32[1.5]",
               "UInt32(32)", "UInt32[]", "UInt32[32]")
   for (expr in toTest) {
      expect(is.double(juliaEval(expr)), "Must be double")
   }
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
                   matrix(c(0, 1, 1, 0), ncol = 2)) %in% c(1i, -1i)),
          "")
   expect(all(jla.eigmax(matrix(c(0, 1i, -1i, 0), ncol = 2)) == 1.0), "")
})


test_that("Echo: raw vector", {
   testEcho(as.raw(c(1,2,3)))
   testEcho(juliaEval("[0x01]"))
   testEcho(juliaEval("UInt8[]"))
   expect_equivalent(juliaEval("[0x01 0x02; 0x03 0x04]"),
                     matrix(c(1,3,2,4), nrow = 2))
   expect_equal(juliaCall("string", c(as.raw(0xca), as.raw(0xfe))),
                "UInt8[0xca, 0xfe]")
})


test_that("Echo: raw vector", {
   testEcho(juliaEval("Int32[1, 2]"))
   testEcho(juliaEval("Int32[]"))
   testEcho(juliaEval("Int32[1]"))
})


test_that("Echo: Single Int16", {testEcho(juliaEval('Int16(300)'))})
test_that("Echo: Int16 Vector", {testEcho(juliaEval('Int16[1,2,3]'))})
test_that("Echo: 1-element Int16 Vector", {
   expect_equal(juliaCall("string", juliaEval('Int16[300]')),
                "Int16[300]")
})


test_that("Echo: Single UInt128", {testEcho(juliaEval('UInt128(2)^100 +1'))})
test_that("Echo: UInt128 Vector", {testEcho(juliaEval('UInt128[1,2,3]'))})
test_that("Echo: 1-element UInt128 Vector", {
   expect_equal(juliaCall("string", juliaEval('UInt128[1]')),
                "UInt128[0x00000000000000000000000000000001]")
})


# Test Let
test_that("let: used like eval", {expect(is.null(juliaLet("print(1)")), "Failed")})
test_that("Let: must error with no named argument", {expect_error(juliaLet("print(x)", 1), "")})
test_that("Let: basic echo", {expect(all(juliaLet("identity(x)", x=c(2, 3)) == c(2,3)), "Failed")})


#Test Pairs
test_that("Echo: Pairs", {
   testEcho(juliaEval("(1 => 2.0)"))
   testEcho(juliaEval("1 => 2.0 => 3.0"))
   testEcho(juliaEval("[1 => 2.0, 2 => 3.0]"))
})


# Test Tuples
test_that("Echo: Tuples", {
   testEcho(juliaEval("(1, 2.0)"))
   testEcho(juliaEval("((1, 2.0), 3.0)"))
   testEcho(juliaLet("collect(zip(x,y))", x = c(1L,2L, 3L), y = c(1,0,1)))
})


# Test Named Tuples
test_that("Echo: Named Tuples", {
   namedTuple <- juliaLet("y=2*x; z = 3*u + 1; (x=y, y=z)", x=2, u=4)
   testEcho(namedTuple)
})


# Test Module
test_that("Echo: Module", {
   juliaEval("module TestModule end")
   testEcho(juliaEval("TestModule"))
})

# Test Dictionary
test_that("Echo: Dictionary", {
   d <- juliaEval("Dict(:bla => 1.0, :blup => 3.0)")
   testEcho(d)
   d <- juliaLet("Dict(zip(x, y))", x = c("bla", "blup"), y = c(1,2))
   testEcho(d)
   d <- juliaLet("Dict(zip(x, y))", x = list("bla"), y = list(1))
   testEcho(d)
   d <- juliaLet("Dict(zip(x, y))", x = list(), y = list())
   testEcho(d)
})


# Test Set
test_that("Echo: Set", {
   s1 <- juliaEval("Set([1; 2; 3; 4])")
   s2 <- juliaEval("Set([1; 2])")
   expect_length(setdiff(juliaEval("Set([1; 2; 3; 4])"), c(1,2,3,4)), 0)
   expect_length(setdiff(juliaLet("setdiff(s1, s2)", s1 = s1, s2 = s2), c(3,4)), 0)
   testEcho(s1)
})


# Test types with bitstypes
test_that("Object with bitstype", {
   uuidregex <- '^[a-f0-9]{8}-?[a-f0-9]{4}-?4[a-f0-9]{3}-?[89ab][a-f0-9]{3}-?[a-f0-9]{12}$'
   juliaImport("UUIDs")
   expect_match(juliaCall("string", UUIDs.uuid4()), uuidregex)
})


# Test complex constructor with all kinds of types
test_that("Complex Julia object with different member type", {
   juliaEval('struct TestTypeWithAllKindsOfStuff
               n::Nothing
               f16::Float16
               f32::Float32
               f64::Float64
               b::Bool
               i8::Int8
               ui8::UInt8
               i16::Int16
               ui16::UInt16
               i32::Int32
               ui32::UInt32
               ch::Char
               i64::Int64
               ui64::UInt64
               i128::Int128
               ui128::UInt128
               f16vec::Vector{Float16}
               f32vec::Vector{Float32}
               f64vec::Vector{Float64}
               bvec::Vector{Bool}
               i8vec::Vector{Int8}
               ui8vec::Vector{UInt8}
               i16vec::Vector{Int16}
               ui16vec::Vector{UInt16}
               i32vec::Vector{Int32}
               ui32vec::Vector{UInt32}
               chvec::Vector{Char}
               i64vec::Vector{Int64}
               ui64vec::Vector{UInt64}
               i128vec::Vector{Int128}
               ui128vec::Vector{UInt128}
             end')
   juliaEval('function TestTypeWithAllKindsOfStuff()
               TestTypeWithAllKindsOfStuff(
                     nothing,
                     rand(Float16), rand(Float32), rand(Float64),
                     rand(Bool),
                     rand(Int8), rand(UInt8),
                     rand(Int16), rand(UInt16),
                     rand(Int32), rand(UInt32),
                     rand(Char),
                     rand(Int64), rand(UInt64),
                     rand(Int128), rand(UInt128),
                     rand(Float16, 2), rand(Float32, 2), rand(Float64, 2),
                     rand(Bool, 2),
                     rand(Int8, 2), rand(UInt8, 2),
                     rand(Int16, 2), rand(UInt16, 2),
                     rand(Int32, 2), rand(UInt32, 2),
                     rand(Char, 2),
                     rand(Int64, 2), rand(UInt64, 2),
                     rand(Int128, 2), rand(UInt128, 2))
             end')
   testEcho(juliaEval("TestTypeWithAllKindsOfStuff()"))
})


test_that("Private inner constructor is forged", {
   juliaEval('struct MyPrivateX
            x::Int
            function MyPrivateX()
               new(5)
            end
          end')
   p <- juliaEval("MyPrivateX()")
   testEcho(p)
})


test_that("Errors are handled gracefully", {
   expect_error(juliaCall("sum", c(1,2,3, "bla")))
   expect_error(juliaCall("thisisnotarealfunction", 100, 4))
   expect_error(juliaCall("RConnector.thisisnotarealfunction", "haha"))
   expect_error(juliaCall("NotARealModule.thisisnotarealfunction", list(1,2,3)))
})


test_that("Callback functions", {

   outputenv <- new.env(parent = emptyenv())
   outputenv$output <- c()
   doOutput <- function(x) {
      outputenv$output <- c(outputenv$output, x)
   }

   # Nested callback functions
   juliaEval('struct TestStruct f::Function end')
   juliaEval('function testNestedFun(ts::Vector{TestStruct}) map(t -> t.f(), ts) end')
   t <- juliaCall("testNestedFun",
                  list(juliaCall("TestStruct", function() {doOutput("a")}),
                       juliaCall("TestStruct", function() {doOutput("b")}),
                       juliaCall("TestStruct", function() {doOutput("c")})))
   expect_equal(outputenv$output, c("a", "b", "c"))

   # as named arguments
   outputenv$output <- c()
   juliaEval('function testNestedFunNamed(;ts::Vector{TestStruct}) map(t -> t.f(), ts) end')
   t <- juliaCall("testNestedFunNamed", ts =
                     list(juliaCall("TestStruct", function() {doOutput("x")}),
                          juliaCall("TestStruct", function() {doOutput("y")})))
   expect_equal(outputenv$output, c("x", "y"))


   # test repeated call of nested functions
   outputenv$output <- c()
   juliaEval('function testNestedFun2(ts::Vector{TestStruct}) for i = 1:2 map(t -> t.f(), ts) end end')
   t <- juliaCall("testNestedFun2",
                  list(juliaCall("TestStruct", function() {doOutput(1)}),
                       juliaCall("TestStruct", function() {doOutput(2)})))
   expect_equal(outputenv$output, c(1,2,1,2))

   # Test multiple arguments
   outputenv$output <- c()
   juliaEval('function testNestedFunArgs(ts::Vector{TestStruct}, args...) map(t -> t.f(args...), ts) end')
   t <- juliaCall("testNestedFunArgs",
                  list(juliaCall("TestStruct", function(x, y, z) {doOutput(x)}),
                       juliaCall("TestStruct", function(x, y, z) {doOutput(z); return(c(5,6,7))})),
                  1, 2, 3)
   expect_equal(outputenv$output, c(1,3))


   outputenv$output <- c()
   juliaEval('function testNestedAndUnnested(f::Function, ts::Vector{TestStruct}, args...)
             testNestedFunArgs(ts, args...)
             f(args...)
             end')

   t <- juliaCall("testNestedAndUnnested",
                  function(x, y) {doOutput(x)},
                  list(juliaCall("TestStruct", function(x,y) {doOutput(x)}),
                       juliaCall("TestStruct", function(x, y) {doOutput(y); return(list(5,6,7))})),
                  17,18) # TODO try with function with error
   expect_equal(outputenv$output, c(17, 18, 17))
})


test_that("Julia functions as members are transferred and usable in R", {
   op1 <- juliaFun("+")
   juliaEval('struct FunTestStruct f::Function end')
   funTestStruct <- juliaCall("FunTestStruct", op1)
   expect_equal(funTestStruct[["f"]](1,2), 3)
})


test_that("juliaCall checks given name before running", {
   expect_error(juliaCall())
   expect_error(juliaCall(1))
   expect_error(juliaCall(c("bla", "bla")))
})


test_that("Documentation example of juliaFun", {
   juliaSqrt <- juliaFun("sqrt")
   expect_equal(juliaSqrt(2), sqrt(2))
   expect_equal(juliaCall("map", juliaSqrt, c(1,4,9)), c(1,2,3))
})

