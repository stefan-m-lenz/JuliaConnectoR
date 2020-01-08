test_that("Some smoke tests", {
   expect((juliaCall("prod", c(1,2,3)) == 6), "Product")
   juliaCall("string", list())
   juliaCall("string", list(as.integer(1), "bla" = 23L))
   juliaEval("String[]")
   expect(!is.null(juliaEval("using Random; Random.seed!(5)")),
          "Must be able to set random seed")
   expect(is.null(juliaEval("Random.seed!(5);")),
          "Eval with semicolon at end returns NULL")
})


test_that("Output is transferred", {
   output <- capture_output({
      juliaCall("println", as.integer(22))
   })
   expect_match(output, "^[\n]*22[\n]*$")

   output <- capture_output({
      juliaCall("println", "hello world")
   })
   expect_match(output, "^[\n]*hello world[\n]*$")
})


test_that("Warnings are transferred", {
   output <- capture.output(type = "message", {
      juliaEval('@warn "This might be serious"')
   })
   expect_match(output[1], "This might be serious")
})


test_that("Warnings and normal output are both transferred and stable", {
   stdoutput <- capture_output({
      stderroutput <- capture.output(type = "message", {
         juliaLet('for i = 1:100
                  println(join(rand(["1", "ä", "ö"], rand(1:100))));
                  println(stderr,join(rand(["2", "u", "a"], rand(1:100))));
                  end')
         ret <- juliaLet('println(22);  17')
      })
   })
   expect_match(stdoutput, "[1äö\n]+")
   expect_match(stderroutput, "[2ua\n]+") # TODO problems with non-ascii-characters


   stdoutput <- capture_output({
      stderroutput <- capture.output(type = "message", {
         ret <- juliaLet('println(22); @warn "This might be serious"; 17')
      })
   })
   expect_equal(ret, 17)
   expect_match(stdoutput[1], "^[\n]*22[\n]*$")
   expect_match(stderroutput[1], "This might be serious")
})


test_that("A return value of nothing is not printed", {
   output <- capture_output({
      juliaEval("nothing")
      juliaCall("identity", NULL)
      juliaLet("x = y", y = NULL)
   })
   expect_match(output, "^[\n]*$")
})


test_that("Test loading and importing a complex package", {
   skip_on_cran()
   juliaEval('begin
                 using Pkg;
                 if !haskey(Pkg.installed(), "StatsBase")
                    Pkg.add("StatsBase")
                 end
             end
             ') # tests also trailing whitespace
   juliaImport("StatsBase")
   expectedMeanAndVar <- list(2,1)
   attr(expectedMeanAndVar, "JLTYPE") <- "Tuple{Float64, Float64}"
   expect(all(as.numeric(StatsBase.mean_and_var(c(1,2,3))) == c(2,1)),
          "Trivial mean and var calculation")
   StatsBase.renyientropy(rnorm(100), 1)
})


test_that("Error when trying to import a non-existent package or module", {
   expect_error(juliaImport("NonExistingPkg"))
   expect_error(juliaUsing("NonExistingPkg"))
   expect_error(capture.output({juliaImport(".NonExistingModule")}, type = "message"))
   expect_error(juliaUsing(".NonExistingModule"))
})

test_that("Error when passing multiple strings to import or using", {
   expect_error(juliaImport(c("Pkg", "UUID")), regexp = "exactly one")
   expect_error(juliaUsing(c("Pkg", "UUID")), regexp = "exactly one")
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


test_that("Echo: Pointers", {
   arrPtr <- juliaFun("Base.pointer", list(1,2,3))
   ptr <- arrPtr(2L)
   expect_type(ptr, "raw")
   testEcho(ptr)
   ptrs <- juliaCall("map", arrPtr, as.integer(1:3))
   testEcho(ptrs)
})


test_that("Echo: Ref", {
   testEcho(juliaCall("Ref", list(1,2,3), 2L))
   r <- juliaEval("global reftestvar = 1; Ref(reftestvar)")
   testEcho(r)
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

   complexTypeParameters = c("Int8", "Int16", "Int32", "Int64",
                             "Float16", "Float32", "Float64")
   for (complexPar in complexTypeParameters) {
      juliaComplexType <- paste0("Complex{", complexPar, "}")
      c <- juliaEval(paste0(juliaComplexType, "(5-3im)"))
      expect_equivalent(c, 5 - 3i)
      carr <- juliaEval(paste0(juliaComplexType, "[4+4im, 2-2im]"))
      expect_equivalent(carr, c(4+4i, 2-2i))
      if (complexPar == "Float64") {
         expect_null(attr(c, "JLTYPE"))
      } else {
         expect_equal(attr(c, "JLTYPE"), juliaComplexType)
      }
   }
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


test_that("Echo: List with NULL elements", {
   x <- list(1, NULL, 3)
   expect_equivalent(juliaEcho(x), x)
   testEcho(juliaEval('[1, nothing, 3]'))
   x <- list("bla", NULL)
   expect_equivalent(x, juliaEcho(x))
   x <- list(NULL, NULL)
   expect_equivalent(x, juliaEcho(x))
})


test_that("Mutable struct has reference", {
   juliaEval('mutable struct TestMutableStruct
                x::Int
             end')
   jlRefEnv <- attr(juliaEval("TestMutableStruct(1)"), "JLREF")
   expect_true(is.environment(jlRefEnv))
   expect_true(is.raw(jlRefEnv$ref))
})


test_that("Currying in juliaFun works", {
   plus1 <- juliaFun("+", 1)
   plus1(2)
   expect_equal(juliaCall("map", plus1, c(1,2,3)), c(2,3,4))
})


test_that("Multidimensional object arrays keep their dimensions", {
   juliaEval("struct MultiTestStruct
                f::Float64
              end")

   content <- juliaEval("rand(1,2,3)")
   testEcho(content)
   x <- juliaLet("map(MultiTestStruct, c)", c = content)
   testEcho(x)
   expect_equivalent(juliaCall("size", x), list(1,2,3))
   attr(x, "JLDIM") <- c(1L, 2L, 4L)
   expect_error(juliaEcho(x), regexp = "Incorrect dimensions")

   # Must also work with dimensions of zero
   testEcho(juliaEval("Matrix{MultiTestStruct}(undef, 0, 0)"))

   x <- juliaEval("Array{MultiTestStruct}(undef, 0, 0, 0)")
   testEcho(x)
   expect_equivalent(juliaCall("size", x), list(0,0,0))
})


test_that("Arrays with undef entries are translated", {
   juliaEval("mutable struct MutableTestStruct
                f::Float64
             end")

   # all undefs
   testEcho(juliaEval("Vector{MutableTestStruct}(undef, 3)"))

   # undefs and values
   juliaLet("x = Vector{MutableTestStruct}(undef, 3);
             x[1] = MutableTestStruct(x1);
             x[2] = MutableTestStruct(x2);
            x", x1 = 1.0, x2 = 2.0)
   testEcho(juliaEval("Vector{MutableTestStruct}(undef, 3)"))

   # multidimensional arrays with undefs and values
   x <- juliaLet("x = Array{MutableTestStruct}(undef, 2, 3, 4);
             x[1,1,1] = MutableTestStruct(x1);
            x[2,2,2] = MutableTestStruct(x2);
            x", x1 = 1.0, x2 = 2.0)
   testEcho(x)
   expect_equal(x[[1]]$f, 1)
})


test_that("Undefined strings are transferred as empty strings", {
   expect_equal(juliaEval("Vector{String}(undef, 3)"), c("", "", ""))
})


# Test Let
test_that("Let: used like eval", {
   output <- capture_output({expect(is.null(juliaLet("print(1)")), "Failed")})
   expect_equal(output, "1")
})
test_that("Let: must error with no named argument", {expect_error(juliaLet("print(x)", 1), "")})
test_that("Let: basic echo", {expect(all(juliaLet("identity(x)", x=c(2, 3)) == c(2,3)), "Failed")})

test_that("Let: Simple example from documentation works", {
   expect_equal(capture_output({juliaLet('println(x)', x = 1)}),
                capture_output({juliaEval('let x = 1.0; println(x) end')}))
})

test_that("Let: arguments without names not accepted", {
   expect_error(juliaLet("println(1)", 1), regexp = "names")
   expect_error(juliaLet("println(1)", x=1, 1, y=2), regexp = "names")
})


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


test_that("Empty module does not cause problems", {
   juliaEval("module EmptyTestModule end")
   expect_invisible(juliaImport(".EmptyTestModule"))
   expect_invisible(juliaUsing(".EmptyTestModule"))
   juliaEval("module EmptyTestModule2 end")
   expect_invisible(juliaImport(".EmptyTestModule2", importInternal = TRUE))
   juliaEval("module EmptyTestModule3 end")
   expect_invisible(juliaUsing(".EmptyTestModule3", importInternal = TRUE))
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
                  17,18)
   expect_equal(outputenv$output, c(17, 18, 17))
})


test_that("Callback function might have error in R", {

   fOK <- function(x) { return(x+1) }
   fnotOK <- function(x) {
      if (x == 2) {
         stop("No even numbers please")
      } else {
         return(1)
      }
   }
   expect_equal(juliaCall("map", fOK, c(1,2,3)), c(2,3,4))
   expect_error(juliaCall("map", fnotOK, c(1,2,3)), class = "error")
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


test_that("Parametric types are imported", {
   juliaEval("module ParametricTypeTestModule
               export MyParametricType
               struct MyParametricType{T}
                  i::T
               end
             end")
   juliaImport(".ParametricTypeTestModule")
   expect_equal(ParametricTypeTestModule.MyParametricType(2)$i, 2)
   expect_equal(ParametricTypeTestModule.MyParametricType("bla")$i, "bla")
})


test_that("JULIACONNECTOR_SERVER environment variable and Killing Julia works", {
   JuliaConnectoR:::stopJulia()
   oldJuliaConnectorServer <- Sys.getenv("JULIACONNECTOR_SERVER")
   # start new JuliaConnectoR server
   port <- JuliaConnectoR:::runJuliaServer(12980)

   # test with wrong variable
   Sys.setenv("JULIACONNECTOR_SERVER" = "wrong form")
   expect_error(JuliaConnectoR:::startJulia(), regexp = "<host>:<port>")

   # now for real
   Sys.setenv("JULIACONNECTOR_SERVER" = paste("localhost", port, sep = ":"))
   # test some command, if it works
   expect_equal(juliaCall("prod", c(1,2)), 2)
   # test killing
   JuliaConnectoR:::killJulia()
   Sys.setenv("JULIACONNECTOR_SERVER" = oldJuliaConnectorServer)
})


test_that("Error if Julia is not setup properly", {
   oldJuliaBindir <- Sys.getenv("JULIA_BINDIR")
   Sys.setenv("JULIA_BINDIR" = "/this/isnot/a/directory/")
   expect_error(JuliaConnectoR:::runJuliaServer(16752), "No Julia executable file found")
   Sys.setenv("JULIA_BINDIR" = oldJuliaBindir)
})


test_that("Circular references do not lead to a crash", {
   tryCatch({juliaEval("mutable struct TestRecur
                  r::Union{TestRecur, Int}
             end")}, error = function(e) {}) # ignore redefinition error
   r <- juliaEval("r1 = TestRecur(2); r2 = TestRecur(r1); r1.r = r2; r1")
   expect_error(juliaEcho(r), regex = "Circular reference")
})


test_that("Anonymous functions can be transferred", {
   af1 <- juliaEval("() -> 17")
   expect_equal(af1(), 17)
   expect_equal(juliaEcho(af1)(), 17)
   rm(af1)

   af2 <- juliaEval("(args...; kwargs...) -> 19")
   expect_equal(af2(list(), 3, bla = 2), 19)
   rm(af2)

   # Test cleaning of references
   invisible(gc(verbose = FALSE))
   expect_gt(length(JuliaConnectoR:::pkgLocal$finalizedRefs), 0)
   juliaEval("1")
   expect_null(JuliaConnectoR:::pkgLocal$finalizedRefs)


   juliaEval("struct TestAnonFunStruct
               f::Function
             end")
   afs <- juliaEval("TestAnonFunStruct(() -> 20)")
   expect_equal(afs[[1]](), 20)
})


test_that("Test BigInt: a Julia object with external pointers", {

   i1 <- juliaCall("BigInt", 2147483647L)
   i2 <- juliaCall("BigInt", 2147483647L)
   p <- juliaCall("prod", list(i1, i2, i1))
   p2 <- juliaCall("prod", list(i1, i2, i1))
   juliaCall("GC.gc") # references in sharedheap must survive this
   jldiv <- juliaFun("div")
   juliaCall("Int", jldiv(jldiv(p,i1), i2))
   expect_equal(juliaLet("Int(div(div(p,i1),i2))", p = p, i1 = i1, i2 = i2),
                2147483647)

   i1Ref <- attr(i1, "JLREF")$ref
   expect_true(juliaLet("RConnector.sharedheap[ref].refcount > 0", ref = i1Ref))
   i1 <- NULL
   invisible(gc())
   juliaEval("1") # after one command, the references from R should be cleaned up

   # should remove temporary Julia references
   # Needs multiple calls to gc as
   # one finalizer removes the references of another object that again.
   juliaCall("GC.gc")
   juliaCall("GC.gc")
   juliaCall("GC.gc")
   expect_false(juliaLet("haskey(RConnector.sharedheap, ref)", ref = i1Ref))
})

# # takes very long, so don't include:
# test_that("Flux model can be transferred", {
#    juliaUsing("Flux")
#    juliaImport("Flux.NNlib", alias = "NNlib", importInternal = TRUE)
#    c <- Chain(Dense(10, 5, NNlib.relu), Dense(5, 2), NNlib.softmax)
#    c$layers[[1]]$s(0)
# })

