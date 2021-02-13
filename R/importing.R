nativeEncodingIsUtf8 <- function() {
   l10n_info()$`UTF-8`
}


nativeNames <- if (nativeEncodingIsUtf8()) {
   function(nameset) { # native encoding supports unicode
      return(nameset)
   }
} else { # no unicode support
   function(nameset) {
   #TODO
   }
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


getFunctionList <- function(funnames, juliaPrefix,
                               constructors = FALSE) {

   if (length(funnames) == 0) {
      return(list())
   }

   if (constructors) {
      funlist <- lapply(funnames, function(funname) {
         funpath <- paste0(juliaPrefix, funname)
         constructor <- juliaFun(funpath)
         attributes(constructor)$JLTYPE <- funpath
         constructor
      })
   } else {
      funlist <- lapply(funnames, function(funname) {
         juliaFun(paste0(juliaPrefix, funname))
      })
   }

   names(funlist) <- funnames

   funlist
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

   if (is.list(modulePath) && !is.null(attr(modulePath, "JLTYPE"))
       && attr(modulePath, "JLTYPE") == "Module") {
      # this is a module that is assumed to be loaded in the Main module
      absoluteModulePath <- modulePath$name
      if (substr(absoluteModulePath, 1, 5) != "Main.") {
         absoluteModulePath <- paste0("Main.", modulePath)
      }
      juliaPrefix <- paste0(modulePath$name, ".")
   } else if (!is.character(modulePath) || length(modulePath) != 1) {
      stop(paste("Expected exactly one Julia module or",
                 "exactly one package name or module path",
                 "as a single-element character vector"))
   } else { # normal module path
      juliaEval(paste("import", modulePath))
      if (substr(modulePath, 1, 1) == ".") {
         absoluteModulePath <- paste0("Main.", gsub("^\\.*", "", modulePath))
      } else {
         absoluteModulePath <- modulePath
      }
      juliaPrefix <- paste0(absoluteModulePath, ".")
   }

   pkgContent <- juliaCall("RConnector.moduleinfo", absoluteModulePath,
                           all = all)

   funenv <- new.env(emptyenv())
   # TODO pkgContent anheften und in print.JuliaModuleImport benutzen
   list2env(envir = funenv,
            getFunctionList(pkgContent$exportedFunctions, juliaPrefix))
   list2env(envir = funenv,
            getFunctionList(pkgContent$exportedTypes, juliaPrefix,
                            constructors = TRUE))

   if (all) {
      list2env(envir = funenv,
               getFunctionList(pkgContent$internalFunctions, juliaPrefix))
      list2env(envir = funenv,
               getFunctionList(pkgContent$internalTypes, juliaPrefix,
                               constructors = TRUE))
   }

   class(funenv) <- "JuliaModuleImport"
   attr(funenv, "JLMODULEPATH") <- absoluteModulePath
   funenv
}


print.JuliaModuleImport <- function(x, ...) {
   cat('Julia module \"')
   cat(attr(x, "JLMODULEPATH"))
   cat("\": ")
   cat(length(x))
   cat(" functions available\n")
}
