test_that("juliaCategorical rejects non-factors and actual NA levels", {
   skip_on_cran()
   for (input in list(NULL, character(0), "a", 1L, TRUE, list("a"),
                      data.frame(value = "a"))) {
      expect_error(juliaCategorical(input), "must be an R factor")
   }
   # exclude = NULL makes NA a category, distinct from a missing observation.
   expect_error(juliaCategorical(factor(c("a", NA), exclude = NULL)),
                "NA factor levels")
})

test_that("categorical support is lazy and reports a missing dependency", {
   skip_on_cran()
   # A fresh server proves that merely including categorical.jl does not import
   # the optional package. Restrict LOAD_PATH temporarily to simulate its absence
   # even on a developer machine where CategoricalArrays is already installed.
   stopJulia()
   expect_false(juliaEval("isdefined(RConnector, :CategoricalArrays)"))
   expect_error(juliaEval('let saved_path = copy(LOAD_PATH)
      try
         empty!(LOAD_PATH)
         push!(LOAD_PATH, "@stdlib")
         RConnector.categorical_from_factor([1], ["a"], false)
      finally
         empty!(LOAD_PATH)
         append!(LOAD_PATH, saved_path)
      end
   end'), "juliaCategorical.*requires CategoricalArrays")
   expect_equal(juliaCall("+", 1L, 2L), 3L)
   expect_false(juliaEval("isdefined(RConnector, :CategoricalArrays)"))
})

test_that("categorical conversion preserves values, levels and ordering", {
   skip_on_cran()
   # As in the existing optional-package tests, provision the Julia dependency
   # outside CRAN. Installation or runtime errors must fail local and CI tests.
   juliaEval('import Pkg
      if Base.find_package("CategoricalArrays") === nothing
         Pkg.add("CategoricalArrays")
      end')

   for (ordered in c(FALSE, TRUE)) {
      input <- factor(c("high", NA, "low", "high"),
                      levels = c("low", "unused", "high"), ordered = ordered)
      original <- input
      result <- juliaCategorical(input)
      expect_s3_class(result, "JuliaArrayProxy")
      expect_true(juliaLet("value isa RConnector.CategoricalArrays.CategoricalArray",
                           value = result))
      expect_identical(as.vector(juliaCall(
         "RConnector.CategoricalArrays.levels", result)), levels(input))
      expect_identical(juliaCall("RConnector.CategoricalArrays.isordered", result),
                       ordered)
      expect_identical(as.vector(juliaCall("ismissing.", result)), is.na(input))
      expect_identical(as.vector(juliaLet(
         "[ismissing(item) ? missing : String(item) for item in value]",
         value = result)), as.character(input))
      expect_identical(input, original)
   }
})

test_that("empty, singleton and all-missing factors remain categorical vectors", {
   skip_on_cran()
   inputs <- list(factor(character(0)),
                  factor(character(0), levels = c("unused", "other")),
                  factor("only"), factor(NA_character_),
                  factor(c(NA, NA), levels = "unused"),
                  factor("NA", levels = c("NA", "unused")))
   for (input in inputs) {
      result <- juliaCategorical(input)
      expect_equal(length(result), length(input))
      expect_equal(juliaCall("ndims", result), 1L)
      expect_true(juliaLet("value isa AbstractVector", value = result))
      expect_true(juliaLet("Missing <: eltype(value)", value = result))
      expect_identical(as.vector(juliaCall(
         "RConnector.CategoricalArrays.levels", result)), levels(input))
      expect_identical(as.vector(juliaCall("ismissing.", result)), is.na(input))
   }
})

test_that("categorical conversion handles Unicode and repeated calls", {
   skip_on_cran()
   input <- ordered(c("\u00e4", "\u03b2", "\u00e4"),
                    levels = c("\u03b2", "\u00e4", "unused"))
   names(input) <- c("first", "second", "third")
   for (i in 1:2) {
      result <- juliaCategorical(input)
      expect_identical(as.vector(juliaCall(
         "RConnector.CategoricalArrays.levels", result)), levels(input))
      expect_true(juliaLet("value[2] < value[1]", value = result))
   }
})

test_that("the generated juliaCategorical documentation examples execute", {
   skip_on_cran()
   # Run the installed Rd examples themselves, so documentation drift is caught
   # instead of testing a separately maintained copy of the example code.
   exampleFile <- tempfile(fileext = ".R")
   on.exit(unlink(exampleFile))
   tools::Rd2ex(utils:::.getHelpFile(help("juliaCategorical",
                                         package = "JuliaConnectoR")),
                out = exampleFile)
   exampleEnv <- new.env(parent = asNamespace("JuliaConnectoR"))
   sys.source(exampleFile, envir = exampleEnv)
   expect_true(exists("categorical", envir = exampleEnv, inherits = FALSE))
})
