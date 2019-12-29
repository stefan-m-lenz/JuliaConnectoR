attachFunctionList <- function(funnames, name, rPrefix, juliaPrefix,
                               constructors = FALSE) {

   if (length(funnames) == 0) {
      return()
   }

   if (constructors) {
      funlist <- lapply(funnames, function(funname) {
         constructor <- juliaFun(paste0(juliaPrefix, funname))
         attributes(constructor)$JLTYPE <- funname
         constructor
      })
   } else {
      funlist <- lapply(funnames, function(funname) {
         juliaFun(paste0(juliaPrefix, funname))
      })
   }

   names(funlist) <- paste0(rPrefix, funnames)
   funenv <- list2env(funlist)

   # create a function without "bang" for each "bang-function"
   # if there is not already one without bang
   for (funname in ls(funenv)) {
      if (endsWithChar(funname, "!")) {
         funnameNoBang <- substr(funname, 1, nchar(funname)-1)
         if (is.null(funenv[[funnameNoBang]])) {
            funenv[[funnameNoBang]] <- funenv[[funname]]
         }
      }
   }

   attach(funenv, name = name)
}


attachJuliaPackage <- function(modulePath, alias, mode,
                               importInternal = FALSE) {
   ensureJuliaConnection()

   if (length(modulePath) != 1) {
      stop("Expected exactly one package name or module path")
   }

   moduleName <- gsub(".*\\.", "", modulePath)
   absoluteModulePath <- gsub("^\\.*", "", modulePath)
   if (is.null(alias)) {
      alias <- absoluteModulePath
   }

   if (mode == LOAD_MODE_USING) {
      loadMode <- "using"
      juliaPrefixExported <- ""
      rPrefixExported <- ""
   } else if (mode == LOAD_MODE_IMPORT) {
      loadMode <- "import"
      juliaPrefixExported <- paste0(absoluteModulePath, ".")
      rPrefixExported <- paste0(alias, ".")
   } else {
      stop(paste("Unknown mode:", mode))
   }

   juliaEval(paste(loadMode, modulePath))

   pkgContent <- juliaCall("RConnector.moduleinfo", absoluteModulePath, all = importInternal)
   if (!is.list(pkgContent)) {
      stop(paste0("Could not find Julia package or module \"",  modulePath, "\"."))
   }

   attachFunctionList(pkgContent$exportedFunctions, absoluteModulePath,
                      rPrefixExported, juliaPrefixExported)
   attachFunctionList(pkgContent$exportedDataTypes, absoluteModulePath,
                      rPrefixExported, juliaPrefixExported,
                      constructors = TRUE)

   juliaPrefixInternal <- paste0(absoluteModulePath, ".")
   rPrefixInternal <- paste0(alias, ".")

   if (mode == LOAD_MODE_USING) {
      attachFunctionList(pkgContent$exportedFunctions, absoluteModulePath,
                         rPrefixInternal, juliaPrefixInternal)
      attachFunctionList(pkgContent$exportedDataTypes, absoluteModulePath,
                         rPrefixInternal, juliaPrefixInternal,
                         constructors = TRUE)
   }

   if (importInternal) {
      attachFunctionList(pkgContent$internalFunctions, absoluteModulePath,
                         rPrefixInternal, juliaPrefixInternal)
      attachFunctionList(pkgContent$internalDataTypes, absoluteModulePath,
                         rPrefixInternal, juliaPrefixInternal,
                         constructors = TRUE)
   }
}


#' Load and import a Julia package via \code{using} statement
#'
#' The specified package/module is loaded via \code{using} in Julia
#' and its functions are attached to the R search path.
#' This way, all functions (including constructors) exported by the
#' package are available in R under their name, and under the name
#' prefixed with the module name (or module path for submodules)
#' plus "\code{.}", like in Julia.
#'
#' @param modulePath name of the package/module that is to be used
#' @param alias alternative prefix for the package
#' (useful e.g. to avoid naming collisions or for brevity)
#' If an alias is not explicitly specified, the name of the package/module
#' or its absolute module path is used.
#' @param importInternal \code{logical} value, default \code{FALSE}.
#' Specifies whether unexported functions shall be imported.
#'
#' @export
#'
#' @examples
#' # Using a package and one of its exported functions
#' juliaUsing("UUIDs")
#' juliaCall("string", uuid4())
#'
#' # Functions that are not exported can be imported
#' # by specifying the argument "importInternal":
#' juliaUsing("Pkg", importInternal = TRUE)
#' Pkg.status()
#'
#' # Using a module without a package
#' testModule <- system.file("examples", "TestModule1.jl",
#'                           package = "JuliaConnectoR")
#' # take a look at the file
#' writeLines(readLines(testModule))
#' # load in Julia
#' juliaCall("include", testModule)
#' # import via "using" in R
#' juliaUsing(".TestModule1")
#' # call exported function
#' test1()
#' # execute exported function via module name
#' TestModule1.test1()
#'
#' # Using a submodule
#' testModule <- system.file("examples", "TestModule1.jl",
#'                           package = "JuliaConnectoR")
#' juliaCall("include", testModule)
#' juliaUsing(".TestModule1.SubModule1")
#' # call exported function of submodule
#' test2()
#' # call exported function of submodule via module path
#' TestModule1.SubModule1.test2()
#'
#' \dontshow{
#' JuliaConnectoR:::stopJulia()
#' }
juliaUsing <- function(modulePath, alias = NULL,
                       importInternal = FALSE) {
   attachJuliaPackage(modulePath, alias,
                      mode = LOAD_MODE_USING,
                      importInternal = importInternal)
   invisible()
}


#' Load and import a Julia package via \code{import} statement
#'
#' The specified package/module is loaded via \code{import} in Julia
#' and its functions are attached to the R search path.
#' This way, all functions (including constructors) exported by the
#' package are available in R under their name
#' prefixed with the module name (or module path for submodules)
#' plus "\code{.}", like in Julia.
#'
#' @param modulePath name of the package/module that is to be used,
#' or a relative module path.
#' Using a module path in Julia like \code{.MyModule}
#' allows to import a module which does not correspond to a package,
#' but has been loaded in the \code{Main} module.
#' Additionally, via a path such as \code{SomePkg.SubModule}
#' a submodule of a package can be imported.
#' (See the examples.)
#' @param alias alternative prefix for the package
#' (useful e.g. to avoid naming collisions or for brevity).
#' If an alias is not explicitly specified, the name of the package/module
#' or its absolute module path is used.
#' @param importInternal \code{logical} value, default \code{FALSE}.
#' Specifies whether unexported functions shall be imported.
#'
#' @export
#'
#' @examples
#' # Importing a package and using one of its exported functions
#' juliaImport("UUIDs")
#' juliaCall("string", UUIDs.uuid4())
#'
#' # Functions that are not exported can be imported
#' # by specifying the argument "importInternal":
#' juliaImport("Pkg", importInternal = TRUE)
#' Pkg.status()
#'
#' # Importing a module without a package
#' testModule <- system.file("examples", "TestModule1.jl",
#'                           package = "JuliaConnectoR")
#' # take a look at the file
#' writeLines(readLines(testModule))
#' # load in Julia
#' juliaCall("include", testModule)
#' # import in R
#' juliaImport(".TestModule1")
#' TestModule1.test1()
#'
#' # Importing a submodule
#' testModule <- system.file("examples", "TestModule1.jl",
#'                           package = "JuliaConnectoR")
#' juliaCall("include", testModule)
#' juliaImport(".TestModule1.SubModule1")
#' # call exported function of submodule via module path
#' TestModule1.SubModule1.test2()
#' juliaImport(".TestModule1.SubModule1", alias = "Sub1")
#' # call exported function of submodule via alias
#' Sub1.test2()
#'
#' \dontshow{
#' JuliaConnectoR:::stopJulia()
#' }
juliaImport <- function(modulePath, alias = NULL,
                        importInternal = FALSE) {
   attachJuliaPackage(modulePath, alias,
                      mode = LOAD_MODE_IMPORT,
                      importInternal = importInternal)
   invisible()
}
