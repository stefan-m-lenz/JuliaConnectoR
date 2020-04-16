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

   # create a function without "bang" for each "bang-function"
   # if there is not already one without bang
   for (funname in names(funlist)) {
      if (endsWithChar(funname, "!")) {
         funnameNoBang <- substr(funname, 1, nchar(funname)-1)
         if (is.null(funlist[[funnameNoBang]])) {
            funlist[[funnameNoBang]] <- funlist[[funname]]
         }
      }
   }

   funlist
}


#' Load and import a Julia package via \code{import} statement
#'
#' The specified package/module is loaded via \code{import} in Julia
#' and its functions are attached to the R search path.
#' This way, all functions (including constructors) exported by the
#' package are available in R under their name
#' prefixed with the module name plus "\code{.}", like in Julia.
#'
#' @param modulePath name of the package/module that is to be used,
#' or a relative module path.
#' Specifying a Julia module path like \code{.MyModule}
#' allows importing a module which does not correspond to a package,
#' but has been loaded in the \code{Main} module, e. g. by
#' \code{juliaCall("include", "path/to/MyModule.jl")}.
#' Additionally, via a path such as \code{SomePkg.SubModule},
#' a submodule of a package can be imported.
#' @param all \code{logical} value, default \code{TRUE}.
#' Specifies whether all functions and types shall be imported
#' or only those exported explicitly.
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
#'    # importing a local module is also possible in one line
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
      absoluteModulePath <- gsub("^\\.*", "", modulePath)
      juliaPrefix <- paste0(absoluteModulePath, ".")
   }

   pkgContent <- juliaCall("RConnector.moduleinfo", absoluteModulePath,
                           all = all)

   funenv <- new.env(emptyenv())
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

   funenv
}
