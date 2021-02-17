test_that("Julia names not expressible in R native encoding are identified", {
   if (grepl("testthat", getwd())) {
      testModulePath <- normalizePath("StrangeNamesTests.jl")
   } else {
      testModulePath <- normalizePath("tests/testthat/StrangeNamesTests.jl")
   }
   # Note: If the module is evaluated via juliaEval,
   # the native encoding is applied to the string,
   # which means e.g. that a sigma is translated to an "s".
   # Sadly this cannot be circumvented because
   # the transformation happens in the call to list(...) in juliaCall.

   juliaCall("include", testModulePath)

   if (l10n_info()$`UTF-8`) { #UTF-8 locale
      # This is not exactly the same as enc2native on a Windows machine with
      # "latin1" native encoding, but suffices to test the functions on Linux.
      transformation <- function(x) { iconv(x, to = "latin1", sub = "byte") }
   } else { # non-UTF8 locale
      transformation <- enc2native
   }

   juliaVersionGeq1_6 <- juliaEval('VERSION >= v"1.6-"')

   # strange: symbols that have a normal latex representation
   # very strange: no normal latex representation (emojis)
   testStrangeNames <- function(i,
                                nFunsExternal,
                                nStrangeFunsExternal,
                                nTypesExternal,
                                nStrangeTypesExternal,
                                nVeryStrangeInternal,
                                nVeryStrangeExternal,
                                nFunsInternal,
                                nStrangeFunsInternal,
                                nTypesInternal,
                                nStrangeTypesInternal) {

      if (juliaVersionGeq1_6) {
         # on Julia 1.6, now all symbols have a "latex" representation
         nVeryStrangeInternal <- 0
         nVeryStrangeExternal <- 0
      }

      moduleInfo <- juliaCall("RConnector.moduleinfo",
                              paste0("Main.StrangeNamesTest", i),
                              all = TRUE)

      if (l10n_info()$`UTF-8`) { #UTF-8 locale: no strange names
         theStrangeNames <- JuliaConnectoR:::strangeNames(moduleInfo)
         expect_equal(nrow(theStrangeNames), 0)
      }

      theStrangeNames <- JuliaConnectoR:::strangeNames(moduleInfo, transformation)
      nDefaultInternalFuns <- 2 # eval and include

      nVeryStrange <- nVeryStrangeExternal +
         nVeryStrangeInternal
      nEscaped <- nStrangeTypesExternal + nStrangeTypesInternal +
         nStrangeFunsExternal + nStrangeFunsInternal - nVeryStrange

      expect_equal(length(moduleInfo$escapedFunctions$escaped) +
                      length(moduleInfo$escapedTypes$escaped), nEscaped)
      expect_equal(length(moduleInfo$escapedFunctions$original) +
                             length(moduleInfo$escapedTypes$original), nEscaped)

      expect_equal(length(moduleInfo$exportedFunctions), nFunsExternal)
      expect_equal(length(moduleInfo$internalFunctions), nFunsInternal + nDefaultInternalFuns)
      expect_equal(length(moduleInfo$exportedTypes), nTypesExternal)
      expect_equal(length(moduleInfo$internalTypes), nTypesInternal)

      expect_warning({moduleInfo <- JuliaConnectoR:::removeStrangeNames(moduleInfo, transformation)})

      expect_equal(length(moduleInfo$escapedFunctions$escaped) +
                      length(moduleInfo$escapedTypes$escaped), nEscaped)
      expect_equal(length(moduleInfo$escapedFunctions$original) +
                      length(moduleInfo$escapedTypes$original), nEscaped)
      expect_equal(length(moduleInfo$exportedFunctions), nFunsExternal - nStrangeFunsExternal)
      expect_equal(length(moduleInfo$internalFunctions), nFunsInternal - nStrangeFunsInternal + nDefaultInternalFuns)
      expect_equal(length(moduleInfo$exportedTypes), nTypesExternal - nStrangeTypesExternal)
      expect_equal(length(moduleInfo$internalTypes), nTypesInternal - nStrangeTypesInternal)


      suppressWarnings({
         mAll <- juliaImport(paste0("Main.StrangeNamesTest", i), all = TRUE)})
      suppressWarnings({
         mExp <- juliaImport(paste0("Main.StrangeNamesTest", i), all = FALSE)})
      if (JuliaConnectoR:::nativeEncodingIsUtf8()) {
         expect_equal(length(names(mAll)),
                      nTypesExternal + nTypesInternal +
                         nFunsInternal + nFunsExternal +
                         nEscaped + nDefaultInternalFuns)
         expect_equal(length(names(mExp)),
                      nTypesExternal + nFunsExternal +
                         nStrangeFunsExternal + nStrangeTypesExternal -
                         nVeryStrangeExternal)
      } else {
         expect_equal(length(names(mAll)),
                      nTypesExternal + nTypesInternal +
                         nFunsInternal + nFunsExternal +
                         nDefaultInternalFuns -
                         nVeryStrange)
         expect_equal(length(names(mExp)),
                      nTypesExternal + nFunsExternal -
                         nVeryStrangeExternal)
      }
   }

   testStrangeNames(1,
                    nFunsExternal = 1,
                    nStrangeFunsExternal = 1,
                    nTypesExternal = 1,
                    nStrangeTypesExternal = 1,
                    nVeryStrangeInternal = 1,
                    nVeryStrangeExternal = 0,
                    nFunsInternal = 2,
                    nStrangeFunsInternal = 2,
                    nTypesInternal = 0,
                    nStrangeTypesInternal = 0)


   testStrangeNames(2,
                    nFunsExternal = 1,
                    nStrangeFunsExternal = 1,
                    nTypesExternal = 0,
                    nStrangeTypesExternal = 0,
                    nVeryStrangeInternal = 0,
                    nVeryStrangeExternal = 0,
                    nFunsInternal = 0,
                    nStrangeFunsInternal = 0,
                    nTypesInternal = 0,
                    nStrangeTypesInternal = 0)

   testStrangeNames(3,
                    nFunsExternal = 0,
                    nStrangeFunsExternal = 0,
                    nTypesExternal = 0,
                    nStrangeTypesExternal = 0,
                    nVeryStrangeInternal = 0,
                    nVeryStrangeExternal = 0,
                    nFunsInternal = 1,
                    nStrangeFunsInternal = 1,
                    nTypesInternal = 0,
                    nStrangeTypesInternal = 0)

   testStrangeNames(4,
                    nFunsExternal = 4,
                    nStrangeFunsExternal = 2,
                    nTypesExternal = 0,
                    nStrangeTypesExternal = 0,
                    nVeryStrangeInternal = 0,
                    nVeryStrangeExternal = 2,
                    nFunsInternal = 2,
                    nStrangeFunsInternal = 1,
                    nTypesInternal = 0,
                    nStrangeTypesInternal = 0)
})

