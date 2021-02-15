nativeEncodingIsUtf8 <- function() {
   l10n_info()$`UTF-8`
}


# Returns all names in a module that cannot be expressed in the native encoding
# and an alternative name, if there is one, in a matrix with columns
# "original" and "alternative".
# "moduleInfo" is a list returned from "RConnector.moduleinfo"
strangeNames <- function(moduleInfo) {
   theNames <- c(moduleInfo$exportedFunctions, moduleInfo$internalFunctions,
                 moduleInfo$internalTypes, moduleInfo$exportedTypes)
   # Check whether names transformed by enc2native are different than the
   # original names. If yes, add them to the returned set of "strange" names.
   ret <- matrix(data = "", nrow = length(theNames), ncol = 2)
   colnames(ret) <- c("original", "alternative")
   i <- 0
   for (name in theNames) {
      if (enc2native(name) != name) {
         i <- i + 1
         ret[i, 1] <- name
         escapeIndex <- which(moduleInfo$escapedNames$original == name)
         if (length(escapeIndex) == 1) {
            # (may be zero if there is no latex replacement for this character)
            ret[i, 2] <- moduleInfo$escapedNames$escaped[[escapeIndex]]
         }
      }
   }
   ret[seq_len(i), ]
}


warnAboutStrangeNames <- function(theStrangeNames) {
   nofNotImported <- length(theStrangeNames)/2
   warnStr <- ifelse(nofNotImported == 1,
                     "name is not imported because it cannot be expressed in native encoding.",
                     "names are not imported because they cannot be expressed in native encoding.")
   # TODO for x names there are alternatives. for more information, print the imported module object
   warning(paste(nofNotImported, warnStr), call. = FALSE)
}


# TODO dokumentieren, testen mit Module mit 1 und mehreren strange names
removeStrangeNames <- function(moduleInfo) {
   theStrangeNames <- strangeNames(moduleInfo)
   if (NROW(theStrangeNames) != 0) {
      if (length(theStrangeNames) == 2) {
         noStrangeNames <- function(v) {
            v[!(v == theStrangeNames["original"])]
         }
      } else {
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
   }
   moduleInfo
}


getFunctionList <- function(juliaNames, juliaPrefix, constructors = FALSE,
                            rNames = juliaNames) {

   if (length(juliaNames) == 0) {
      return(list())
   }

   if (constructors) {
      # A constructor can also be used as an object that is translated to the
      # corresponding type in Julia. That's why they are handled differently
      # than normal functions.
      funlist <- lapply(juliaNames, function(funname) {
         funpath <- paste0(juliaPrefix, funname)
         constructor <- juliaFun(funpath)
         attributes(constructor)$JLTYPE <- funpath
         constructor
      })
   } else {
      funlist <- lapply(juliaNames, function(funname) {
         juliaFun(paste0(juliaPrefix, funname))
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

   list2env(envir = funenv,
            getFunctionList(moduleInfo$exportedFunctions, juliaPrefix))
   list2env(envir = funenv,
            getFunctionList(moduleInfo$exportedTypes, juliaPrefix,
                            constructors = TRUE))

   if (!is.null(moduleInfo$internalFunctions)) {
      list2env(envir = funenv,
               getFunctionList(moduleInfo$internalFunctions, juliaPrefix))
   }
   if (!is.null(moduleInfo$internalTypes)) {
      list2env(envir = funenv,
               getFunctionList(moduleInfo$internalTypes, juliaPrefix,
                               constructors = TRUE))
   }
   if (!is.null(moduleInfo))

   class(funenv) <- "JuliaModuleImport"
   attr(funenv, "moduleInfo") <- moduleInfo
   funenv
}


# Get the absolute module path from a Julia module object or a relative module
# path or return the input object if it is already an absolute path.
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
#'    \dontshow{
#'       JuliaConnectoR:::stopJulia()
#'    }
#'    # Importing a local module is also possible in one line,
#'    # by directly using the module object returned by "include".
#'    TestModule1 <- juliaImport(juliaCall("include", testModule))
#'    TestModule1$test1()
#' }
#'

#'
#' if (juliaSetupOk()) {
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
   cat(attr(x, "moduleInfo")$absoluteModulePath)
   cat("\": ")
   cat(length(x))
   cat(" functions available\n")
}
