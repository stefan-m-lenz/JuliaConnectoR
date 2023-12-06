nativeEncodingIsUtf8 <- function() {
   l10n_info()$`UTF-8`
}


# Returns all names in a module that cannot be expressed in the native encoding
# and an alternative name, if there is one, in a matrix with columns
# "original" and "alternative".
# "moduleInfo" is a list returned from "RConnector.moduleinfo"
strangeNames <- function(moduleInfo, transformation = enc2native) {
   theNames <- c(moduleInfo$exportedFunctions, moduleInfo$internalFunctions,
                 moduleInfo$internalTypes, moduleInfo$exportedTypes)

   # We don't care here whether its a name for a type or a function.
   escapedNames <- list()
   escapedNames$original <- c(moduleInfo$escapedFunction$original,
                              moduleInfo$escapedTypes$original)
   escapedNames$escaped <- c(moduleInfo$escapedFunction$escaped,
                             moduleInfo$escapedTypes$escaped)

   # Check whether names transformed by enc2native are different than the
   # original names. If yes, add them to the returned set of "strange" names.
   ret <- matrix(data = "", nrow = length(theNames), ncol = 2)
   colnames(ret) <- c("original", "alternative")
   i <- 0
   for (name in theNames) {
      if (transformation(name) != name) {
         i <- i + 1
         ret[i, 1] <- name
         escapeIndex <- which(escapedNames$original == name)
         if (length(escapeIndex) == 1) {
            # (may be zero if there is no latex replacement for this character)
            ret[i, 2] <- escapedNames$escaped[[escapeIndex]]
         }
      }
   }
   ret[seq_len(i), ]
}


warnAboutStrangeNames <- function(theStrangeNames) {
   warning("Some names could not be expressed in the native encoding.\n",
           "(Details see output of printing the returned object.)",
           call. = FALSE)
}


# Remove all names that cannot be expressed from the moduleInfo object and
# return the new moduleInfo object.
# The transformation can be changed to enable unit testing on Linux.
removeStrangeNames <- function(moduleInfo, transformation = enc2native) {
   theStrangeNames <- strangeNames(moduleInfo, transformation)
   if (NROW(theStrangeNames) != 0) {
      veryStrangeNames <- character()
      if (length(theStrangeNames) == 2) { # a vector
         if (theStrangeNames["alternative"] == "") {
            veryStrangeNames <- theStrangeNames["original"]
         }
         noStrangeNames <- function(v) {
            v[!(v == theStrangeNames["original"])]
         }
      } else { # a matrix
         veryStrangeNames <-
            theStrangeNames[theStrangeNames[, "alternative"] == "", "original"]
         noStrangeNames <- function(v) {
            v[!(v %in% theStrangeNames[, "original"])]
         }
      }
      moduleInfo$exportedFunctions <- noStrangeNames(moduleInfo$exportedFunctions)
      moduleInfo$exportedTypes <- noStrangeNames(moduleInfo$exportedTypes)
      if (!is.null(moduleInfo$internalFunctions)) {
         moduleInfo$internalFunctions <- noStrangeNames(moduleInfo$internalFunctions)
      }
      if (!is.null(moduleInfo$internalTypes)) {
         moduleInfo$internalTypes <- noStrangeNames(moduleInfo$internalTypes)
      }
      warnAboutStrangeNames(theStrangeNames)
      if (length(veryStrangeNames) != 0) {
         moduleInfo$veryStrangeNames <- veryStrangeNames
      }
   }
   moduleInfo
}


getFunctionList <- function(juliaNames, juliaPrefix, constructors = FALSE,
                            rNames = juliaNames) {

   if (length(juliaNames) == 0) {
      return(list())
   }

   getFunPath <- function(funname) {
      # add ":" so that it works also for operators
      paste0(juliaPrefix, ":", funname)
   }

   if (constructors) {
      # A constructor can also be used as an object that is translated to the
      # corresponding type in Julia. That's why they are handled differently
      # than normal functions.
      funlist <- lapply(juliaNames, function(funname) {
         funpath <- getFunPath(funname)
         constructor <- juliaFun(funpath)
         attributes(constructor)$JLTYPE <- funpath
         constructor
      })
   } else {
      funlist <- lapply(juliaNames, function(funname) {
         juliaFun(getFunPath(funname))
      })
   }

   names(funlist) <- rNames

   funlist
}


# Create an environment that contains all functions from a Julia module
# and some additional information about the module, which can be printed later.
createJuliaModuleImport <- function(moduleInfo) {
   funenv <- new.env(emptyenv())

   juliaPrefix <- paste0(moduleInfo$absoluteModulePath, ".")

   add2funenv <- function(funnames, constructors = FALSE, rNames = funnames) {
      list2env(envir = funenv,
               getFunctionList(funnames, juliaPrefix, constructors, rNames))
   }

   normalFuns <- c(moduleInfo$exportedFunctions, moduleInfo$internalFunctions)
   constructors <- c(moduleInfo$exportedTypes, moduleInfo$internalTypes)

   add2funenv(normalFuns)
   add2funenv(constructors, constructors = TRUE)
   add2funenv(moduleInfo$escapedFunctions$original,
              rNames = moduleInfo$escapedFunctions$escaped)
   add2funenv(moduleInfo$escapedTypes$original,
              constructors = TRUE,
              rNames = moduleInfo$escapedTypes$escaped)

   class(funenv) <- "JuliaModuleImport"
   attr(funenv, "moduleInfo") <- moduleInfo
   funenv
}


# Get the absolute module path from a Julia module object or a relative module
# path or return the input object if it is already an absolute path.
# Import the module in Julia if an absolute or relative path is given.
getAbsoluteModulePath <- function(moduleArg) {
   if (is.list(moduleArg) && !is.null(attr(moduleArg, "JLTYPE"))
       && attr(moduleArg, "JLTYPE") == "Module") {
      # this is a module that is assumed to be loaded in the Main module
      absoluteModulePath <- moduleArg$name
      if (substr(absoluteModulePath, 1, 5) != "Main.") {
         absoluteModulePath <- paste0("Main.", moduleArg)
      }
   } else if (!is.character(moduleArg) || length(moduleArg) != 1) {
      stop(paste("Expected exactly one Julia module or",
                 "exactly one package name or module path",
                 "as a single-element character vector"))
   } else { # normal module path
      juliaEval(paste("import", moduleArg))
      if (substr(moduleArg, 1, 1) == ".") {
         absoluteModulePath <- paste0("Main.", gsub("^\\.*", "", moduleArg))
      } else {
         absoluteModulePath <- moduleArg
      }
   }
   absoluteModulePath
}


#' Load and import a Julia package via \code{import} statement
#'
#' The specified package/module is loaded via \code{import} in Julia.
#' Its functions and type constructors are wrapped into R functions.
#' The return value is an environment containing all these R functions.
#'
#' @param modulePath a module path or a module object.
#' A module path may simply be the name of a package but it may also
#' be a relative module path.
#' Specifying a relative Julia module path like \code{.MyModule}
#' allows importing a module that does not correspond to a package,
#' but has been loaded in the \code{Main} module, e. g. by
#' \code{juliaCall("include", "path/to/MyModule.jl")}.
#' Additionally, via a path such as \code{SomePkg.SubModule},
#' a submodule of a package can be imported.
#' @param all \code{logical} value, default \code{TRUE}.
#' Specifies whether all functions and types shall be imported
#' or only those exported explicitly.
#' @return an environment containing all functions and type constructors
#' from the specified module as R functions
#'
#' @note
#' If a package or module contains functions or types with names that contain
#' non-ASCII characters, (additional) alternatives names are provided
#' if there are LaTeX-like names for the characters available in Julia.
#' In the alternative names of the variables, the LaTeX-like names of the
#' characters surrounded by \code{<...>} replace the original characters.
#' (See example below.)
#' For writing platform independent code, it is recommended to use those
#' alternative names.
#' (See also \link{JuliaConnectoR-package} under "Limitations".)
#'
#' @export
#'
#' @examples
#' if (juliaSetupOk()) {
#'
#'    # Importing a package and using one of its exported functions
#'    UUIDs <- juliaImport("UUIDs")
#'    juliaCall("string", UUIDs$uuid4())
#'
#'
#'    # Importing a module without a package
#'    testModule <- system.file("examples", "TestModule1.jl",
#'                              package = "JuliaConnectoR")
#'    # take a look at the file
#'    writeLines(readLines(testModule))
#'    # load in Julia
#'    juliaCall("include", testModule)
#'    # import in R via relative module path
#'    TestModule1 <- juliaImport(".TestModule1")
#'    TestModule1$test1()
#'
#'    # Importing a local module is also possible in one line,
#'    # by directly using the module object returned by "include".
#'    TestModule1 <- juliaImport(juliaCall("include", testModule))
#'    TestModule1$test1()
#'
#'
#'    # Importing a submodule
#'    testModule <- system.file("examples", "TestModule1.jl",
#'                              package = "JuliaConnectoR")
#'    juliaCall("include", testModule)
#'    # load sub-module via module path
#'    SubModule1 <- juliaImport(".TestModule1.SubModule1")
#'    # call function of submodule
#'    SubModule1$test2()
#'
#'
#'    # Functions using non-ASCII characters
#'    greekModule <- system.file("examples", "GreekModule.jl",
#'                              package = "JuliaConnectoR")
#'    suppressWarnings({ # importing gives a warning on non-UTF-8 locales
#'       GreekModule <- juliaImport(juliaCall("include", greekModule))
#'    })
#'    # take a look at the file
#'    cat(readLines(greekModule, encoding = "UTF-8"), sep = "\n")
#'    # use alternative names
#'    GreekModule$`<sigma>`(1)
#'    GreekModule$`log<sigma>`(1)
#' }
#'
#' \dontshow{
#' JuliaConnectoR:::stopJulia()
#' }
juliaImport <- function(modulePath, all = TRUE) {
   ensureJuliaConnection()

   absoluteModulePath <- getAbsoluteModulePath(modulePath)
   moduleInfo <- juliaCall("RConnector.moduleinfo", absoluteModulePath,
                           all = all)
   moduleInfo$absoluteModulePath <- getAbsoluteModulePath(modulePath)

   if (!nativeEncodingIsUtf8()) {
      moduleInfo <- removeStrangeNames(moduleInfo)
   }

   createJuliaModuleImport(moduleInfo)
}


print.JuliaModuleImport <- function(x, ...) {
   cat('Julia module \"')
   moduleInfo <- attr(x, "moduleInfo")
   cat(moduleInfo$absoluteModulePath)
   cat("\": ")
   nFunsTotal <- length(x)
   nEscaped <- length(moduleInfo$escapedFunctions$escaped) +
      length(moduleInfo$escapedTypes$escaped)
   if (nativeEncodingIsUtf8()) {
      nFunsTotal <- nFunsTotal - nEscaped
      # (otherwise the escaped version are counted twice)
   }
   cat(nFunsTotal)
   cat(" functions available (including type constructors).\n")

   if (nEscaped > 0) {
      if (nativeEncodingIsUtf8()) {
         cat("Additionally, there are ")
         cat(nEscaped)
         cat(" alternative names that are compatible with plaforms without UTF-8 native encoding:\n\n")
         print(data.frame(Original = c(moduleInfo$escapedFunctions$original,
                                       moduleInfo$escapedTypes$original),
                          Alternative = c(moduleInfo$escapedFunctions$escaped,
                                          moduleInfo$escapedTypes$escaped)), row.names = FALSE)
      } else {
         cat(nEscaped + length(moduleInfo$veryStrangeNames))
         cat(" names could not be expressed in the native encoding. Possible alternatives:\n\n")
         printEscapedAlternatives(moduleInfo)
      }
      cat("\n")
   }
}


# Print a matrix of names with one column for the original names
# and one column with the (Latex) alternative
printEscapedAlternatives <- function(moduleInfo) {
   m <- matrix(c(moduleInfo$escapedFunctions$original,
                 moduleInfo$escapedTypes$original,
                 moduleInfo$escapedFunctions$escaped,
                 moduleInfo$escapedTypes$escaped),
               ncol = 2)
   colnames(m) <- c("Original", "Alternative")
   print(m, quote = FALSE)
}
