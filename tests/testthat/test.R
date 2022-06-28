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
                 try
                    @eval import StatsBase
                 catch ex
                    Pkg.add("StatsBase")
                 end
             end
             ') # tests also trailing whitespace
   StatsBase <- juliaImport("StatsBase")
   expect_equivalent(juliaGet(StatsBase$mean_and_var(c(1,2,3))), list(2,1))
   StatsBase$renyientropy(rnorm(100), 1)
})


test_that("Error when trying to import a non-existent package or module", {
   expect_error(juliaImport("NonExistingPkg"))
   expect_error(capture.output({juliaImport(".NonExistingModule")}, type = "message"))
})

test_that("Error when passing multiple strings to import or using", {
   expect_error(juliaImport(c("Pkg", "UUID")), regexp = "exactly one")
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


test_that("Echo: String with missing values", {
   x <- c("Yes", "No", NA)
   y <- juliaEcho(x)
   expect_true(is.na(y[3]))

   x <- matrix(c("bla", NA, NA, "NA"), nrow = 2)
   y <- juliaEcho(x)
   expect_false(is.na(y[1,1]))
   expect_true(is.na(y[1,2]))
   expect_true(is.na(y[2,1]))
   expect_false(is.na(y[2,2]))
})


test_that("Echo: Int vector with missing values", {
   expect_true(juliaLet("x[1] == 1 && ismissing(x[2]) && x[3] == 3", x = c(1L,NA, 3L)))
   expect_true(juliaLet("ismissing(x)", x = NA_integer_))
   x <- juliaEval("[1 missing 3; 3 missing 4 ]")
   expect_type(x, "integer")
   expect_true(all(is.na(x[,2])))
   expect_null(attr(x, "JLTYPE"))

   x <- juliaEval("[2^52 missing 3; 2^53 missing 4 ]")
   expect_type(x, "double")
   expect_true(all(is.na(x[,2])))
   expect_equal(x[1,1], 2^52)
   expect_equal(x[2,1], 2^53)
   testEcho(x)

   x <- juliaEval("Union{Int32,Missing}[1 missing 3; 3 missing 4 ]")
   expect_type(x, "integer")
   expect_true(all(is.na(x[,2])))
   expect_equal(x[1,1], 1)
   expect_equal(x[2,1], 3)
   testEcho(x)

   x <- juliaEval("Union{Int16,Missing}[-1 missing 3; 3 missing 4 ]")
   expect_type(x, "integer")
   expect_true(all(is.na(x[,2])))
   expect_equal(x[1,1], -1)
   expect_equal(x[2,1], 3)
   testEcho(x)

   testEcho(c(1L, 2L, NA, NA))
   testEcho(matrix(c(1L, 2L, NA, NA), nrow = 2))
})


test_that("Echo: Double vector with missing values", {
   expect_true(juliaLet("x[1] == 1.0 && ismissing(x[2]) && x[3] == 3.0", x = c(1,NA, 3)))
   expect_true(juliaLet("ismissing(x)", x = NA_integer_))
   x <- juliaEval("[1.0 missing 3.0; 3.0 missing 4.0 ]")
   expect_type(x, "double")
   expect_true(all(is.na(x[,2])))
   expect_null(attr(x, "JLTYPE"))


   x <- juliaEval("Union{Float32,Missing}[1.0f0 missing 3.0f0; 3.0f0 missing 4.0f0 ]")
   expect_true(all(is.na(x[,2])))
   expect_equal(x[1,1], 1)
   expect_equal(x[2,1], 3)
   testEcho(x)

   testEcho(c(1, 2, NA, NA))
   testEcho(matrix(c(1, 2, NA, NA), nrow = 2))
})


test_that("Dirty missing values are recognized", {
   dirtyNa <- c(as.raw(0xa2), as.raw(0x07), as.raw(0x00), as.raw(0x00),
                as.raw(0x00), as.raw(0x00), as.raw(0xf8), as.raw(0x7f))
   dirtyNA <- readBin(dirtyNa, what = "double", endian = "little")
   expect_true(is.na(dirtyNA) && !is.nan(dirtyNA))
   expect_true(juliaCall("ismissing", dirtyNA))
   expect_true(juliaLet("ismissing(x[2])", x = c(1, dirtyNA)))
   echoDirtyNA <- juliaEcho(dirtyNA)
   expect_true(is.na(echoDirtyNA) && ! is.nan(echoDirtyNA))
   dirtyComplexNA <- complex(real = dirtyNA, imaginary = dirtyNA)
   expect_true(is.na(dirtyComplexNA) && !is.nan(dirtyComplexNA))
   expect_true(juliaCall("ismissing", dirtyComplexNA))
   x <- c(1+1i, dirtyComplexNA, NA)
   expect_true(juliaLet("ismissing(x[2]) && ismissing(x[3])", x = x))
   testEcho(x)
   # The following doesn't work: dirtNA coerced to NA+0i, not NA+NA*i:
   # expect_true(juliaLet("ismissing(x[2])", x = c(1+1i, dirtyNA)))
   echoDirtyComplexNA <- juliaEcho(dirtyComplexNA)
   expect_true(is.na(echoDirtyComplexNA) && !is.nan(echoDirtyComplexNA))
})


test_that("NaN and NA are handled differently", {
   # NaN in R is translated to a NaN Float64 value in Julia
   # NA in R is translated to a missing value in Julia

   x <- juliaEval("[1; missing; NaN]")
   expect_equal(x[1], 1)
   expect_true(is.na(x[2]) && !is.nan(x[2]))
   expect_true(is.nan(x[3]))

   y <- juliaEcho(x)
   expect_equal(y[1], 1)
   expect_true(is.na(y[2]) && !is.nan(y[2]))
   expect_true(is.nan(y[3]))

   expect_equal(juliaCall("typeof", c(1, NA)),
                juliaEval("Array{Union{Float64,Missing},1}"))
   expect_equal(juliaCall("typeof", c(1, NaN)),
                juliaEval("Array{Float64,1}"))

   expect_equal(juliaCall("typeof", c(1+1i, NA)),
                juliaEval("Array{Union{Complex{Float64},Missing},1}"))
   expect_equal(juliaCall("typeof", c(1+1i, NaN)),
                juliaEval("Array{Complex{Float64},1}"))

   expect_true(juliaLet("ismissing(x[2])", x = c(1+1i, NA)))
})


test_that("Missing values transferred as NA", {
   expect_true(is.na(juliaEval("missing")))
   expect_true(all(is.na(juliaEval("[missing; missing]"))))
   expect_true(all(is.na(juliaEval("[missing missing; missing missing]"))))
})

test_that("Logical with missing values", {
   juliaLet("x[1] && !x[2] && ismissing(x[3])", x = c(TRUE, FALSE, NA))
   x <- c(TRUE, FALSE, NA)
   testEcho(x)
})

test_that("Complex with missing values", {
   juliaLet("x[1] == 1+1im && x[2] == 2+2im && ismissing(x[3])", x = c(1 +1i, 2+2i, NA))
   x <- c(1 +1i, 2+2i, NA)
   testEcho(x)

   x <- juliaEval("[1+1im 1+2im; 0 missing]")
   expect_equivalent(x, matrix(c(1+1i, 0, 1+2i, NA), nrow = 2))
   testEcho(x)

   x <- juliaEval("[1.0+im 2.0+im; 0.0 missing]")
   expect_equal(x, matrix(c(1+1i, 0, 2+1i, NA), nrow = 2))
   expect_true(is.null(attr(x, "JLTYPE")))
   testEcho(x)
})


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


test_that("Complex values are handled first class", {
   testEcho(1i)
   testEcho(juliaEval("1+im"))
   testEcho(juliaEval("[1+im]"))
   testEcho(juliaEval("[1+im; 2+im]"))
   testEcho(c(1+1i,2 + 2i))

   suppressWarnings({jla <- juliaImport("LinearAlgebra")})
   testEcho(matrix(c(1, 0, 0, -1), ncol = 2))
   expect(all(jla$eigvals(matrix(c(1, 0, 0, -1), ncol = 2),
                   matrix(c(0, 1, 1, 0), ncol = 2)) %in% c(1i, -1i)),
          "")
   expect(all(jla$eigmax(matrix(c(0, 1i, -1i, 0), ncol = 2)) == 1.0), "")

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
         expect_true(juliaCall("==", juliaEval(attr(c, "JLTYPE")),
                               juliaEval(juliaComplexType)))
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
   x <- juliaGet(juliaEcho(list(1, NULL, 3)))
   expect_equal(x[[1]], 1)
   expect_null(x[[2]])
   expect_equal(x[[3]], 3)
   expect_equal(length(x), 3)
   testEcho(juliaEval('[1, nothing, 3]'))
   x <- list("bla", NULL)
   expect_equal(x[[1]], "bla")
   expect_null(x[[2]])
   x <- list(NULL, NULL)
   expect_null(x[[1]])
   expect_null(x[[2]])
})


test_that("Mutable struct usable by reference", {
   juliaEval('mutable struct TestMutableStruct
                x::Int
             end')
   jlRef <- juliaEval("TestMutableStruct(1)")
   refEcho <- juliaEcho(jlRef)
   expect_true(juliaCall("==", jlRef, refEcho))
   expect_true(all(get("ref", jlRef) == get("ref", refEcho)))
   expect_equal(refEcho$x, 1)
   refEcho$x <- 2L
   expect_equal(jlRef$x, 2)
   ref <- get("ref", jlRef)
   jlRef <- NULL
   refEcho <- NULL
   invisible(gc())
   juliaEval("1")
   expect_false(juliaLet("haskey(communicator.sharedheap, ref)", ref = ref,
                         communicator = JuliaConnectoR:::pkgLocal$communicator))

   # Test behaviuor with juliaGet:
   # The reference must be attached
   jlRefEnv <- attr(juliaGet(juliaEval("TestMutableStruct(1)")), "JLREF")
   expect_true(is.environment(jlRefEnv))
   expect_true(is.raw(jlRefEnv$ref))
})


test_that("Immutable struct usable by reference and translated", {
   juliaEval('struct TestImmutableStruct
                x::Int
             end')
   jlRef <- juliaEval("TestImmutableStruct(1)")
   expect_s3_class(jlRef, "JuliaStructProxy")
   refEcho <- juliaEcho(jlRef)
   expect_true(juliaCall("==", jlRef, refEcho))
   expect_equal(refEcho$x, 1)
   ref <- get("ref", jlRef)
   expect_equal(juliaGet(jlRef)$x, 1)
   expect_false(juliaLet("haskey(communicator.sharedheap, ref)", ref = ref,
                         communicator = JuliaConnectoR:::pkgLocal$communicator))
   gc()
   juliaEval("1")
})


test_that("Currying in juliaFun works", {
   plus1 <- juliaFun("+", 1)
   plus1(2)
   expect_equal(juliaCall("map", plus1, c(1,2,3)), c(2,3,4))
})


test_that("Multidimensional object arrays work", {
   juliaEval("struct MultiTestStruct
                f::Float64
              end")

   content <- juliaEval("rand(4,5,6)")
   testEcho(content)
   x <- juliaLet("map(MultiTestStruct, c)", c = content)
   testEcho(x)
   expect_equivalent(dim(x), c(4,5,6))

   expect_equivalent(juliaGet(x[1,1,1]), list(juliaGet(x[[1,1,1]])))

   x[[1,2,3]] <- juliaEval("MultiTestStruct(17.0)")
   expect_equal(x[[1,2,3]]$f, 17)

   x[1,2,2] <- juliaEval("MultiTestStruct(19.0)")
   expect_equal(x[[1,2,2]]$f, 19)

   x[1,2,2] <- list(juliaEval("MultiTestStruct(19.0)"))
   expect_equal(x[[1,2,2]]$f, 19)

   expect_equal(dim(x[3:4, 4:5, 1]), c(2,2,1))
   expect_equal(dim(x[3:4, as.integer(4:5), 1L]), c(2,2,1))

   x[3:4, as.integer(4:5), 1L] <- juliaEval("MultiTestStruct(20.0)")
   expect_equal(juliaGet(x[[3, 4, 1]])$f, 20)
   expect_equal(juliaGet(x[[4, 5, 1]])$f, 20)

   y <- juliaGet(x)
   attr(y, "JLDIM") <- c(1L, 2L, 3L)
   expect_error(juliaEcho(y), regexp = "Incorrect dimensions")

   # Must also work with dimensions of zero
   testEcho(juliaEval("Matrix{MultiTestStruct}(undef, 0, 0)"))

   x <- juliaEval("Array{MultiTestStruct}(undef, 0, 0, 0)")
   testEcho(x)
   expect_equivalent(dim(x), c(0,0,0))

   # Two dimensions
   content <- juliaEval("rand(2,3)")
   x <- juliaLet("map(MultiTestStruct, c)", c = content)
   x[[1,2]] <- juliaEval("MultiTestStruct(3.0)")
   expect_equal(x[[1,2]]$f, 3)
})


test_that("Arrays with undef entries are translated", {
   juliaEval("mutable struct MutableTestStruct
                f::Float64
             end")

   # all undefs
   testEcho(juliaEval("Vector{MutableTestStruct}(undef, 3)"),
            comparableInJulia = FALSE)

   # undefs and values
   juliaLet("x = Vector{MutableTestStruct}(undef, 3);
             x[1] = MutableTestStruct(x1);
             x[2] = MutableTestStruct(x2);
            x", x1 = 1.0, x2 = 2.0)
   testEcho(juliaEval("Vector{MutableTestStruct}(undef, 3)"),
            comparableInJulia = FALSE)

   # multidimensional arrays with undefs and values
   x <- juliaLet("x = Array{MutableTestStruct}(undef, 2, 3, 4);
             x[1,1,1] = MutableTestStruct(x1);
            x[2,2,2] = MutableTestStruct(x2);
            x", x1 = 1.0, x2 = 2.0)
   testEcho(x, comparableInJulia = FALSE)
   expect_equal(x[[1]]$f, 1)
})


test_that("Undefined strings are transferred as empty strings", {
   expect_equal(juliaEval("Vector{String}(undef, 3)"), c("", "", ""))
})


test_that("String with null can be translated back to Julia", {
   # einfacher string
   jlStrWithNul <- "String([0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x00, 0x5a, 0x65, 0x72, 0x6f, 0x57])"
   str <- juliaEval(jlStrWithNul)
   expect_true(is.raw(str))
   expect_equal(as.character(juliaCall("typeof", str)), "String")
   expect_equal(as.character(juliaLet("String(x[1:5])", x = str)), "Hello")

   # string array

   strArr <- juliaLet("String[a,b]", a = str, b = str)
   expect_true(is.list(strArr))

   strArr <- juliaLet("String[a,b,c]", a = str, b = str, c = str)
   expect_true(is.list(strArr))
   expect_equal(juliaCall("typeof", strArr), juliaEval("Array{String,1}"))
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
   x <- juliaEval("(1, 17, 18)")
   expect_equal(x[[2]], 17)
   expect_equal(juliaGet(x[1]), juliaGet(juliaEval("(1,)")))
   expect_length(x, 3)
   expect_length(x[1:2], 2)

   testEcho(juliaEval("(1, 2.0)"))
   testEcho(juliaEval("((1, 2.0), 3.0)"))
   testEcho(juliaLet("collect(zip(x,y))", x = c(1L,2L, 3L), y = c(1,0,1)))

   testEcho(juliaEval("(Ref(false), Ref(true))"))
   testEcho(juliaEval("Tuple{}()"))
})


# Test Named Tuples
test_that("Echo: Named Tuples", {
   namedTuple <- juliaLet("y=2*x; z = 3*u + 1; (x=y, y=z)", x=2, u=4)
   expect_s3_class(namedTuple, "JuliaStructProxy")
   expect_equal(namedTuple$y, 13)
   expect_type(juliaGet(namedTuple), "list")
   expect_equal(juliaGet(namedTuple)$y, 13)
   testEcho(namedTuple)
})


# Test Module
test_that("Echo: Module", {
   juliaEval("module TestModule end")
   testEcho(juliaEval("TestModule"))
})

test_that("Echo: Symbol", {
   x <- juliaEval(":asymbol")
   expect_true(is.symbol(x))
   expect_true(juliaCall("isa", as.symbol("s"), juliaExpr("Symbol")))
   testEcho(x)
})


# Test Dictionary
test_that("Echo: Dictionary", {
   d <- juliaEval("Dict(:bla => 1.0, :blup => 3.0)")
   expect_equivalent(d[juliaExpr(":bla")][[1]], 1)
   expect_equal(d[juliaExpr(":bla"), juliaExpr(":bla")], as.list(c(1, 1)))
   expect_equal(d[[as.symbol("bla")]], 1)
   expect_equal(d[as.symbol("blup")],list(3))
   d[juliaExpr(":blup")] <- 4
   expect_equal(d[[juliaExpr(":blup")]], 4)
   d[[juliaExpr(":blup")]] <- 5
   expect_equal(d[[juliaExpr(":blup")]], 5)
   d2 <- juliaGet(d)
   expect_setequal(d2[["keys"]], juliaGet(juliaEval("[:bla, :blup]")))
   expect_setequal(d2[["values"]], list(1, 5))

   d[juliaExpr(":hi"), juliaExpr(":hello")] <- c(15, 14)
   expect_equal(d[juliaExpr(":bla"), juliaExpr(":hi")], as.list(c(1, 15)))
   d[juliaExpr(":hi"), juliaExpr(":hello")] <- as.list(c(16, 17))
   expect_equal(d[juliaExpr(":bla"), juliaExpr(":hello"), juliaExpr(":hi")], as.list(c(1, 17, 16)))
   expect_length(d[juliaExpr(":bla"), juliaExpr(":hi"), juliaExpr(":hello"), juliaExpr(":hello")], 4)

   d <- juliaLet("Dict(zip(x, y))", x = c("bla", "blup"), y = c(1,2))
   expect_equal(d[["bla"]], 1)
   expect_equivalent(d["blup"], list(2))
   d2 <- juliaGet(d)
   expect_setequal(d2[["keys"]], list("bla", "blup"))
   expect_setequal(d2[["values"]], list(1, 2))

   d2 <- juliaGet(d)
   expect_setequal(d2[["keys"]], list("bla", "blup"))
   expect_setequal(d2[["values"]], list(1,2))

   d$bla <- 17
   expect_equal(d$bla, 17)

   d <- juliaLet("Dict(zip(x, y))", x = list("bla"), y = list(1))
   expect_equal(length(juliaCall("keys", d)), 1)
   testEcho(d)

   d <- juliaLet("Dict(zip(x, y))", x = list(), y = list())
   d2 <- juliaGet(d)
   expect_length(d2[["keys"]], 0)
   expect_length(d2[["values"]], 0)
   testEcho(d)
})


# Test Set
test_that("Echo: Set", {
   s1 <- juliaEval("Set([1; 2; 3; 4])")
   s2 <- juliaEval("Set([1; 2])")
   expect_length(setdiff(juliaGet(juliaEval("Set([1; 2; 3; 4])")),
                         c(1,2,3,4)), 0)
   expect_length(setdiff(juliaGet(juliaLet("setdiff(s1, s2)", s1 = s1, s2 = s2)),
                         c(3,4)), 0)
   testEcho(s1)
})


# Test types with bitstypes
test_that("Object with bitstype", {
   uuidregex <- '^[a-f0-9]{8}-?[a-f0-9]{4}-?4[a-f0-9]{3}-?[89ab][a-f0-9]{3}-?[a-f0-9]{12}$'
   UUIDs <- juliaImport("UUIDs")
   expect_match(juliaCall("string", UUIDs$uuid4()), uuidregex)
})


test_that("Exotic objects handled gracefully", {
   x <- quote(1 + 1)
   expect_type(x, "language")
   expect_warning({ y <- juliaEcho(list(x = x))},
                  regexp = "not translatable", all = FALSE)
   expect_null(y$x)
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


test_that("Imported modules are printed", {
   # a stdlib module
   Pkg <- juliaImport("Pkg")
   expect_match(regexp = 'Julia module "Pkg": [0-9]+ function.*',
                capture.output(print(Pkg))[1])

   # a user defined module
   module <- juliaEval('module ImportTestModule1
               export f
               f(x)=1
             end')
   itm1 <- juliaImport(module, all = FALSE)
   expectedOutput <-
      'Julia module "Main.ImportTestModule1": 1 function.*'
   expect_match(capture.output(print(itm1))[1], regexp = expectedOutput)

   itm1_2 <- juliaImport(".ImportTestModule1", all = FALSE)
   expect_match(capture.output(print(itm1_2))[1], regexp = expectedOutput)
})


test_that("Empty module does not cause problems", {
   juliaEval("module EmptyTestModule end")
   expect_length(ls(juliaImport(".EmptyTestModule")), 2) # (eval and include)
   juliaEval("module EmptyTestModule2 end")
   expect_length(ls(juliaImport(".EmptyTestModule2", all = FALSE)), 0)
})


test_that("Errors are handled gracefully", {
   expect_error(juliaCall("sum", c(1,2,3, "bla")))
   expect_error(juliaCall("thisisnotarealfunction", 100, 4))
   expect_error(juliaCall("RConnector.thisisnotarealfunction", "haha"))
   expect_error(juliaCall("NotARealModule.thisisnotarealfunction", list(1,2,3)))
})


test_that("Vectors of objects can be accessed by index via proxy", {
   x <- juliaEval("[ [1;2;3], [4;5;6], [7;8;9] ]")
   expect_s3_class(x, "JuliaArrayProxy")
   expect_equal(x[[1]][2], 2)
   expect_equal(x[[2]][2], 5)
   x[[1]][1] <- 17L
   expect_equal(x[[1]][1], 17)
   expect_equal(x[2:3][[1]][[1]], 4)
   x[2:3][[1]][[1]] <- 18L
   expect_equal(x[2:3][[1]][[1]], 18)

   expect_equal(x[[1]][x[[2]] == 5], 2) # logical indexing

   # juliaGet version must behave in the same way as the proxy version
   y <- juliaGet(x)
   expect_equal(x[[2]], y[[2]])
   expect_equal(x[[1]][2], y[[1]][2])
   expect_equal(x[[2]][2], y[[2]][2])
   x[[1]][1] <- 17L
   y[[1]][1] <- 17L
   expect_equal(x[[1]][1], 17)
   expect_equal(y[[1]][1], 17)
   x[2:3][[1]][[1]] <- 18L
   y <- juliaGet(x)
   expect_equal(y[2:3][[1]][[1]], 18)
   expect_equal(x[2:3][[1]][1], y[2:3][[1]][1])
   expect_equal(x[[2]], y[[2]])

   # Multiple dimensions
   y <- juliaEval("map( x-> [1;x;3], rand(2,2,2))")
   expect_equivalent(y[c(1,2)][[1]], y[[1]])
   expect_equivalent(y[c(1,3)][[2]], y[[3]])
   y[c(1,3)] <- juliaEval("[[17.0], [18.0]]")
   expect_equivalent(juliaGet(y[1]), list(17))
   expect_equivalent(juliaGet(y[3]), list(18))
   expect_equivalent(juliaGet(y)[c(4,5)], juliaGet(y[c(4,5)]))
})


test_that("Callback functions", {

   x <- juliaEcho(function() {})
   expect_equal(typeof(x), "closure")
   expect_equal(x, function() {})

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


test_that("Callback function calling Julia erroring in Julia", {
   f <- juliaEval('(x) -> error("Error")')
   g <- function(x) {cat("OK"); f(x); print("NotOK"); x}
   output <- capture.output(expect_error(juliaCall("map", g, c(1,2,3))))
   expect_equal(output, "OK")
})


test_that("Builtin functions can be used as callback functions", {
   expect_equal(juliaCall("map", sqrt, c(1,4,9)), c(1,2,3))
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
   expect_equal(funTestStruct$f(1,2), 3)
})


test_that("juliaCall checks given name before running", {
   expect_error(juliaCall())
   expect_error(juliaCall(1))
   expect_error(juliaCall(c("bla", "bla")))
})


test_that("juliaCall can handle keyword arguments with key \"name\"", {
   Pkg <- juliaImport("Pkg")
   # indirect via created function object
   expect_equal(Pkg$PackageSpec(name = "RData", version = "v0.10")$name,
                "RData")
   # direct call of juliaCall
   pkgspec <- juliaCall("Pkg.PackageSpec", name = "RData", version = "0.10")
   expect_equal(pkgspec$name, "RData")
})


test_that("Documentation example of juliaFun", {
   juliaSqrt <- juliaFun("sqrt")
   expect_equal(juliaSqrt(2), sqrt(2))
   expect_equal(juliaCall("map", juliaSqrt, c(1,4,9)), c(1,2,3))
})


test_that("Constructors stand for their types", {
   juliaEval('module TestTypeModule
                export TestType
                struct TestType end
             end')
   TestTypeModule <- juliaImport(".TestTypeModule")
   expect_equal(as.character(juliaCall("typeof", TestTypeModule$TestType)),
                "DataType")
})


test_that("Parametric types are imported", {
   juliaEval("module ParametricTypeTestModule
               export MyParametricType
               struct MyParametricType{T}
                  i::T
               end
             end")
   ParametricTypeTestModule <- juliaImport(".ParametricTypeTestModule")
   expect_equal(ParametricTypeTestModule$MyParametricType(2)$i, 2)
   expect_equal(ParametricTypeTestModule$MyParametricType("bla")$i, "bla")
})


test_that("Starting and stopping Julia server" , {
   stopJulia()
   port <- startJuliaServer()
   expect_match(Sys.getenv("JULIACONNECTOR_SERVER"), paste0("localhost:", port))

   # starting the server twice should give a warning and not do anything
   expect_warning(startJuliaServer())
   expect_match(Sys.getenv("JULIACONNECTOR_SERVER"), paste0("localhost:", port))

   # now the server should start successfully
   expect_equal(juliaEval("1"), 1)

   # set a variable in a separate process:
   rPath <- file.path(R.home("bin"), "R")
   if (Sys.info()["sysname"] == "Windows") {
      rPath <- paste0(rPath, ".exe")
   }
   system2(rPath, c("-e", shQuote(
      paste0("library(JuliaConnectoR); juliaEval('global x = 1')"))),
      stdout = FALSE)

   # check that variable is set in this Julia session,
   # i.e. that the two R processes share the same Julia process
   expect_equal(juliaEval("x"), 1)

   stopJulia()
   expect_equal(Sys.getenv("JULIACONNECTOR_SERVER"), "")
   expect_null(JuliaConnectoR:::pkgLocal$con)
})


test_that("Julia startup options can be modified", {
   stopJulia()
   oldJuliaOpts <- Sys.getenv("JULIACONNECTOR_JULIAOPTS")

   # set project directory via startup opts
   projectPathWithSpaces <- file.path(tempdir(), "Dirname with Spaces")
   dir.create(file.path(tempdir(), "Dirname with Spaces"))
   specifyProjectOpts <- paste0('--project="', projectPathWithSpaces, '"')
   Sys.setenv("JULIACONNECTOR_JULIAOPTS" = paste0('--project="',
                                                  projectPathWithSpaces, '"'))
   expect_match(capture.output({juliaEval("import Pkg; Pkg.status()")}),
                regexp = "Dirname with Spaces", fixed = TRUE, all = FALSE)
   stopJulia()

   # set two startup opts: project directory and deprecation warnings
   Sys.setenv("JULIACONNECTOR_JULIAOPTS" = paste(specifyProjectOpts,
                                                "--depwarn=no"))
   juliaDepWarn <- 'Base.depwarn("there is a deprecation warning", :foo)'
   expect_length(capture.output({juliaEval(juliaDepWarn)}), 0)
   stopJulia()

   # clean up
   unlink(projectPathWithSpaces, recursive = TRUE)
   if (oldJuliaOpts == '') {
      Sys.unsetenv("JULIACONNECTOR_JULIAOPTS")
   } else {
      Sys.setenv("JULIACONNECTOR_JULIAOPTS" = oldJuliaOpts)
   }
})


test_that("JULIACONNECTOR_SERVER environment variable and Killing Julia works", {
   # if JULIACONNECTOR_SERVER is used, the server must be started with
   # "keeprunning = true" to work.
   skip_on_cran()
   JuliaConnectoR:::stopJulia()
   oldJuliaConnectorServer <- Sys.getenv("JULIACONNECTOR_SERVER")
   # start new JuliaConnectoR server
   port <- JuliaConnectoR:::startJuliaServer(12980)
   expect_match(Sys.getenv("JULIACONNECTOR_SERVER"), paste0("localhost:", port))

   # test with wrong variable
   Sys.setenv("JULIACONNECTOR_SERVER" = "wrong form")
   expect_error(JuliaConnectoR:::startJulia(), regexp = "<host>:<port>")

   # now for real
   Sys.setenv("JULIACONNECTOR_SERVER" = paste("localhost", port, sep = ":"))
   # test some command, if it works
   expect_equal(juliaCall("prod", c(1,2)), 2)
   # test killing
   JuliaConnectoR:::killJulia()
   if (oldJuliaConnectorServer == "") {
      Sys.unsetenv("JULIACONNECTOR_SERVER")
   } else {
      Sys.setenv("JULIACONNECTOR_SERVER" = oldJuliaConnectorServer)
   }
})


test_that("Error if Julia is not setup properly", {
   oldJuliaBindir <- Sys.getenv("JULIA_BINDIR")
   Sys.setenv("JULIA_BINDIR" = "/this/isnot/a/directory/")
   expect_error(JuliaConnectoR:::runJuliaServer(16752), "No Julia executable file found")
   if (oldJuliaBindir == "") {
      Sys.unsetenv("JULIA_BINDIR")
   } else {
      Sys.setenv("JULIA_BINDIR" = oldJuliaBindir)
   }
   expect_equal(1, juliaEval("1"))
})


test_that("Circular references do not lead to a crash", {

   definition <- 'mutable struct TestRecur
                  r::Union{TestRecur, Int}
                  end'
   try({juliaEval(definition)}, silent = TRUE) # (ignore redefinition error)

   r <- juliaEval("r1 = TestRecur(2); r2 = TestRecur(r1); r1.r = r2; r1")
   expect_match(capture.output({print(juliaEcho(r))}), regex = "circular reference", all = FALSE)
   expect_error(juliaEcho(juliaGet(r)), regex = "Circular reference")

   # reference only
   r <- juliaEval("r1 = TestRecur(2); r2 = TestRecur(r1); r1.r = r2; r1")
   expect_match(capture.output({print(juliaEcho(r))}), regex = "circular reference", all = FALSE)
   expect_equal(juliaCall("typeof", r$r$r), juliaCall("typeof", r))
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
   expect_equal(afs$f(), 20)
})


test_that("Test BigInt: a Julia object with external pointers", {

   # Note: It is not really recommended to translate objects with
   # external pointers. Nevertheless, it should work and not cause a crash.

   i1 <- juliaGet(juliaCall("BigInt", 2147483647L))
   i2 <- juliaGet(juliaCall("BigInt", 2147483647L))
   p <- juliaGet(juliaCall("prod", list(i1, i2, i1)))
   p2 <- juliaGet(juliaCall("prod", list(i1, i2, i1)))
   juliaCall("GC.gc") # references in sharedheap must survive this
   jldiv <- juliaFun("div")
   expect_equal(juliaCall("Int", jldiv(jldiv(p,i1), i2)), 2147483647L)
   expect_equal(juliaLet("Int(div(div(p,i1),i2))", p = p, i1 = i1, i2 = i2),
                2147483647)

   f <- juliaLet("p/i1/i2", p = p, i1 = i1, i2 = i2)
   expect_equal(juliaCall("Float64", f), 2147483647)

   i1Ref <- attr(i1, "JLREF")$ref
   expect_true(juliaLet("communicator.sharedheap[ref].refcount > 0",
                        ref = i1Ref, communicator = pkgLocal$communicator))
   i1 <- NULL
   invisible(gc())
   juliaEval("1") # after one command, the references from R should be cleaned up

   # Multiple calls to the Julia garbage are necessary for a complete clean-up as
   # one finalization may make another object unreachable, which will then
   # be cleaned up in the next run.
   juliaCall("GC.gc")
   juliaCall("GC.gc")
   juliaCall("GC.gc")
   expect_false(juliaLet("haskey(communicator.sharedheap, ref)", ref = i1Ref,
                         communicator = pkgLocal$communicator))
})


test_that("Serialized mutable struct can be restored", {
   juliaEval("mutable struct TestSerializationStruct
               i::Int
             end")
   x <- juliaGet(juliaEval("TestSerializationStruct(1)"))
   tmpfile <- tempfile()
   save("x", file = tmpfile)
   x <- NULL
   #juliaEval("println(RConnector.sharedheap)")
   invisible(gc())
   juliaEval("1")
   juliaCall("GC.gc")
   load(tmpfile)
   msg <- capture.output(type = "message", {
      expect_equal(juliaEcho(x)$i, 1)
   })
   expect_match(msg, "external references", all = FALSE)
   x <- NULL
   juliaEval("1")
   juliaCall("GC.gc") # copy of the serialized copy is cleaned up
   file.remove(tmpfile)
})


test_that("Object with unexported function defined in different modules", {
   juliaEval('
   module FirstModule
      struct FirstType
         f::Function
      end
      f(x::FirstType) = 1
   end

   module SecondModule

      import ..FirstModule.f
      struct SecondType
         f::Function
      end

      f(x::SecondType) = 2
   end #module')

   # It doesn't matter whether SecondModule.f or FirstModule.f is used
   ft1 <- juliaEval("FirstModule.FirstType(SecondModule.f)")
   ft2 <- juliaEval("SecondModule.SecondType(FirstModule.f)")

   expect_equal(juliaLet("x.f(x)", x = ft1), 1)
   expect_equal(juliaLet("x.f(x)", x = ft2), 2)
   expect_equal(juliaLet("x.f(y)", x = ft1, y = ft2), 2)
   expect_equal(juliaLet("x.f(y)", x = ft2, y = ft1), 1)
})


test_that("AbstractArrays are transferred by reference and can be translated to struct", {
   a <- juliaEval('using SparseArrays
             A = sparse([1, 2, 3], [1, 2, 3], [0, 2, 0])')
   expect_s3_class(a, "JuliaArrayProxy")
   # indexing
   expect_equal(a[[2,2]], 2)
   # backtranslation
   expect_true(juliaCall("issparse", juliaGet(a)))
   testEcho(a)
})


test_that("Operators can be imported and used", {
   suppressWarnings({jla <- juliaImport("LinearAlgebra", all = FALSE)})
   expect_equal(jla$`<cdot>`(1,2), 2)
   suppressWarnings({base <- juliaImport("Base", all = FALSE)})
   expect_true(base$`<subseteq>`(c(1,2), c(1,2,3)))
})


test_that("Boltzmann machine can be trained and used", {
   #skip_on_cran()

   juliaEval('using Pkg;
              try
                 @eval import BoltzmannMachines
              catch ex
                 Pkg.add(PackageSpec(name = "BoltzmannMachines", version = v"1.2"))
              end')

   # Set a random seed in Julia
   juliaEval("using Random; Random.seed!(5);")

   BM <- juliaImport("BoltzmannMachines")

   # a test data set from the BoltzmannMachines-package, just to have some data
   x <- BM$barsandstripes(50L, 4L)
   x

   # Train DBMs with
   dbm <- BM$fitdbm(x, epochs = 2L, nhiddens = c(4L,3L))
   dbm
   dbm2 <- BM$fitdbm(x, epochs = 1L,
                     pretraining = list(BM$TrainLayer(nhidden = 4L),
                                        BM$TrainLayer(nhidden = 3L)))
   dbm2 <- juliaGet(dbm2)

   # Use a trained model to generate samples
   gensamples <- BM$samples(dbm, 10L)
   expect_equal(nrow(gensamples), 10)

   # Evaluate the model: Likelihood estimation ...
   BM$loglikelihood(dbm2, x)
   #  ... or exact calculation (possible for such a small model)
   BM$exactloglikelihood(dbm2, x)
})


test_that("juliaPut", {
   x <- juliaPut(c(1,2,3))
   expect_s3_class(x, "JuliaSimpleArrayProxy")
   expect_equal(x[[1]], 1)
   expect_equal(x[1:2], c(1,2))
   expect_type(x[3], "double")
   x[2:3] <- 0
   expect_equal(juliaCall("sum", x), 1)

   expect_error(juliaPut(juliaEval("(a=1,b=2,c=3)")))

   x <- juliaPut("bla")
   expect_equal(as.character(juliaCall("typeof", x)), "String")

   # use on translated object
   x <- juliaGet(juliaEval("[(1,2), (2,3)]"))
   expect_s3_class(juliaPut(x), "JuliaArrayProxy")
})


test_that("Display of default printing is limited to width", {
   x <- juliaEval("rand(100, 100)")
   y <- juliaPut(x)
   printOutput <- capture.output({print(y)})
   expect_lte(nchar(printOutput[length(printOutput)]),
              getOption("width"))
})


test_that("Error message for tables", {
   capture.output({JuliaConnectoR:::showUpdateTablesMsg()})

   x <- juliaEcho(list(c(1,2), c(1,2,3)))
   expect_s3_class(x, "JuliaProxy")
   expect_error({as.data.frame(x)})
})


test_that("Data frame can be translated", {
   x <- data.frame(x = c(0, 2, 4), y = c("bla", "blup", "ha"),
                   stringsAsFactors = FALSE)
   testEcho(x)
   y <- juliaEcho(x)

   x2 <- as.data.frame(juliaEcho(x))
   expect_s3_class(x2, "data.frame")
   expect_equal(x2$x, c(0,2,4))
   expect_equal(x2$y, c("bla", "blup", "ha"))

   testEcho(data.frame())

   juliaEval('import Pkg;
              try
                 @eval import IndexedTables
              catch ex
                 Pkg.add("IndexedTables")
              end
              import IndexedTables')
   x <- data.frame(x = c(0, 2, 4), y = c("bla", "blup", "ha"),
                   stringsAsFactors = FALSE)
   y <- juliaCall("IndexedTables.table", x)
   expect_equal(as.data.frame(x), x)

   # Works also without column names (Issue #16)
   df <- juliaCall("IndexedTables.table", 1:3, 11:13)
   df <- as.data.frame(df)
   expect_equal(df[, 1], 1:3)
   expect_equal(df[, 2], 11:13)
})


test_that("Julia types are converted to r compatible types in data frames", {
   x1 <- juliaEval("[1.0f0*im + 1.0f0, 0.0f0]")
   x2 <- juliaEval('map(i -> SubString(i, 2,3), ["asdf", "wert"])')
   x3 <- juliaEval('[true, false]')
   x4 <- juliaEval("['a', 'b']")
   x <- juliaLet('Dict(:x1 => x1, :x2 => x2, :x3 => x3, :x4 => x4)',
            x1 = x1, x2 = x2, x3 = x3, x4 = x4)
   x <- as.data.frame(x)
   expect_equivalent(x$x1, x1)
   expect_equal(x$x2, c("sd", "er"))
   expect_equal(x$x3, x3)
   expect_equivalent(x$x4, x4)
})


test_that("Broadcasting via dot syntax works", {
   expect_equal(juliaCall("sin.", c(0, 1/3*pi, pi/2)),
                c(0, sqrt(3)/2, 1))
   juliaEval("import Statistics")
   arrofarrs <- juliaEval("[[2;3], [4;5]]")
   expect_equal(juliaCall("Statistics.mean.", arrofarrs), c(2.5, 4.5))
   expect_equal(juliaGet(juliaCall("Statistics.mean.", arrofarrs, dims = 1L)),
                juliaGet(juliaEval("[[2.5], [4.5]]")))
})


test_that("Environemnt variables for Julia can be set", {
   # prepare
   os <- Sys.info()['sysname']
   oldJuliaEnv <- Sys.getenv("JULIACONNECTOR_JULIAENV")
   JuliaConnectoR:::stopJulia()

   # Test with one variable
   Sys.setenv(JULIACONNECTOR_JULIAENV = "TESTVARIABLE1=1")
   if (os == "Windows") {
      expect_warning(juliaEval("1+1"))
   } else {
      expect_equal(juliaEval('ENV["TESTVARIABLE1"]'), "1")
   }
   JuliaConnectoR:::stopJulia()

   # Test with two variables
   Sys.setenv(JULIACONNECTOR_JULIAENV = "TESTVARIABLE1=1; TESTVARIABLE2='abc'")
   if (os == "Windows") {
      expect_warning(juliaEval("1+1"))
   } else {
      expect_equal(juliaEval('ENV["TESTVARIABLE1"]'), "1")
      expect_equal(juliaEval('ENV["TESTVARIABLE2"]'), "abc")
   }
   JuliaConnectoR:::stopJulia()

   # LD_LIBRARY_PATH can be set via JULIACONNECTOR_JULIAENV
   Sys.setenv(JULIACONNECTOR_JULIAENV = "LD_LIBRARY_PATH='abc'")
   if (os == "Linux") {
      expect_match(JuliaConnectoR:::getJuliaEnv(), "LD_LIBRARY_PATH='abc'")
   }

   # clean up
   if (oldJuliaEnv == "") {
      Sys.unsetenv("JULIACONNECTOR_JULIAENV")
   } else {
      Sys.setenv(JULIACONNECTOR_JULIAENV = oldJuliaEnv)
   }
})


test_that("Iris/Flux example from README works", {
   skip_on_cran()
   skip_on_covr()
   skip_if(Sys.info()["login"] %in% c("lenz", "Stefan Lenz", "selectstern", "lenzstef"))
   cat("\nExecuting README examples...\n")

   if (grepl("^1\\.0", juliaEval('string(VERSION)'))) {
      skip_on_ci()
   } else {
      projectFolder <- "project_1_6"
   }

   Pkg <- juliaImport("Pkg")
   Pkg$activate(system.file("examples", "iris-example", projectFolder,
                            package = "JuliaConnectoR", mustWork = TRUE))
   Pkg$instantiate()
   Pkg$build()

   irisExampleJl <- system.file("examples", "iris-example", "iris-example.jl",
                                package = "JuliaConnectoR", mustWork = TRUE)
   irisExampleJuliaCode <- readLines(irisExampleJl)
   irisExampleJuliaCode <- sub("^epochs =.*", "epochs = 2", irisExampleJuliaCode)
   juliaEval(paste(irisExampleJuliaCode, collapse = "\n"))
   irisExampleR <- system.file("examples",  "iris-example", "iris-example.R",
                               package = "JuliaConnectoR", mustWork = TRUE)
   irisExampleRCode <- readLines(irisExampleR)
   irisExampleRCode <- sub("epochs <-.*", "epochs <- 2", irisExampleRCode)
   scriptEnv <- new.env(emptyenv())
   suppressMessages(eval(parse(text = paste(irisExampleRCode, collapse = "\n")),
                         envir = scriptEnv))
   # just test something
   expect_s3_class(scriptEnv$model, "JuliaProxy")

   Pkg$activate() # use default environment again
})



test_that("Boltzmann example from README works", {
   returningError <- function(x) {
      paste0("tryCatch({", x , '},',
             'error = function(e) { paste("e: ",e$message) },',
             'warning = function(w) { paste("w: ", w$message) })')
   }

   juliaEval('import Pkg;
            try
               @eval import BoltzmannMachines
            catch ex
               Pkg.add(PackageSpec(name = "BoltzmannMachines", version = v"1.3"))
            end')
   boltzmannExampleR <- system.file("examples", "boltzmann-example.R",
                                     package = "JuliaConnectoR", mustWork = TRUE)
   bmExample <- readLines(boltzmannExampleR)
   bmExample <- sub("plot\\(.*", "", bmExample)
   bmScriptEnv <- new.env(emptyenv())
   code <- paste(bmExample, collapse = "\n")
   code <- returningError(code)
   eval(parse(text = code), envir = bmScriptEnv)
   expect_s3_class(bmScriptEnv$dbm, "JuliaProxy")
})

