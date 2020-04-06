getFunctionList <- function(funnames, rPrefix, juliaPrefix,
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

   names(funlist) <- paste0(rPrefix, funnames)

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


attachJuliaPackage <- function(modulePath, alias, importViaUsing = FALSE,
                               importInternal = FALSE) {
   ensureJuliaConnection()

   if (length(modulePath) != 1) {
      stop("Expected exactly one package name or module path")
   }

   moduleName <- gsub(".*\\.", "", modulePath)
   absoluteModulePath <- gsub("^\\.*", "", modulePath)
   if (is.null(alias)) {
      alias <- moduleName
   }

   if (importViaUsing) {
      loadMode <- "using"
      juliaPrefixExported <- ""
      rPrefixExported <- ""
   } else {
      loadMode <- "import"
      juliaPrefixExported <- paste0(absoluteModulePath, ".")
      rPrefixExported <- paste0(alias, ".")
   }

   juliaEval(paste(loadMode, modulePath))

   pkgContent <- juliaCall("RConnector.moduleinfo", absoluteModulePath,
                           all = importInternal)

   funenv <- new.env(emptyenv())
   list2env(envir = funenv,
            getFunctionList(pkgContent$exportedFunctions,
                      rPrefixExported, juliaPrefixExported))
   list2env(envir = funenv,
            getFunctionList(pkgContent$exportedTypes,
                      rPrefixExported, juliaPrefixExported,
                      constructors = TRUE))

   juliaPrefixInternal <- paste0(absoluteModulePath, ".")
   rPrefixInternal <- paste0(alias, ".")

   if (importViaUsing) {
      list2env(envir = funenv,
               getFunctionList(pkgContent$exportedFunctions,
                               rPrefixInternal, juliaPrefixInternal))
      list2env(envir = funenv,
               getFunctionList(pkgContent$exportedTypes,
                               rPrefixInternal, juliaPrefixInternal,
                               constructors = TRUE))
   }

   if (importInternal) {
      list2env(envir = funenv,
               getFunctionList(pkgContent$internalFunctions,
                               rPrefixInternal, juliaPrefixInternal))
      list2env(envir = funenv,
               getFunctionList(pkgContent$internalTypes,
                               rPrefixInternal, juliaPrefixInternal,
                               constructors = TRUE))
   }

   envName <- paste0("JuliaConnectoR:", absoluteModulePath)
   if (envName %in% search()) {
      detach(envName, character.only = TRUE)
   }
   attach(funenv, name = envName)
}


#' Load and import a Julia package via \code{using} statement
#'
#' The specified package/module is loaded via \code{using} in Julia
#' and its functions are attached to the R search path.
#' This way, all functions (including constructors) exported by the
#' package are available in R under their name, and under the name
#' prefixed with the module name plus "\code{.}".
#'
#' @param modulePath name of the package/module that is to be used,
#' or a relative module path.
#' Specifying a Julia module path like \code{.MyModule}
#' allows using a module which does not correspond to a package,
#' but has been loaded in the \code{Main} module, e. g. by
#' \code{juliaCall("include", "path/to/MyModule.jl")}.
#' Additionally, via a path such as \code{SomePkg.SubModule},
#' a submodule of a package can be imported.
#' @param alias alternative prefix for the package
#' (useful e.g. to avoid naming collisions or for brevity)
#' If an alias is not explicitly specified, the name of the
#' package/module is used.
#' @param importInternal \code{logical} value, default \code{FALSE}.
#' Specifies whether unexported functions shall be imported.
#'
#' @export
#'
#' @examples
#' if (juliaSetupOk()) {
#'
#'    # Using a package and one of its exported functions
#'    juliaUsing("UUIDs")
#'    juliaCall("string", uuid4())
#'
#'    # Functions that are not exported can be imported
#'    # by specifying the argument "importInternal":
#'    juliaUsing("Pkg", importInternal = TRUE)
#'    Pkg.status()
#'
#'    # Using a module without a package
#'    testModule <- system.file("examples", "TestModule1.jl",
#'                              package = "JuliaConnectoR")
#'    # take a look at the file
#'    writeLines(readLines(testModule))
#'    # load in Julia
#'    juliaCall("include", testModule)
#'    # import via "using" in R
#'    juliaUsing(".TestModule1")
#'    # call exported function
#'    test1()
#'    # execute exported function via module name
#'    TestModule1.test1()
#'
#' }
#'
#' \dontshow{
#' JuliaConnectoR:::stopJulia()
#' }
#'
#' if (juliaSetupOk()) {
#'
#'    # Using a submodule
#'    testModule <- system.file("examples", "TestModule1.jl",
#'                              package = "JuliaConnectoR")
#'    juliaCall("include", testModule)
#'    juliaUsing(".TestModule1.SubModule1")
#'    # call exported function of submodule with
#'    test2()
#'    # ... or with
#'    SubModule1.test2()
#'
#' }
#'
#' \dontshow{
#' JuliaConnectoR:::stopJulia()
#' }
juliaUsing <- function(modulePath, alias = NULL,
                       importInternal = FALSE) {
   attachJuliaPackage(modulePath, alias,
                      importViaUsing = TRUE,
                      importInternal = importInternal)
   invisible()
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
#' @param alias alternative prefix for the package
#' (useful e.g. to avoid naming collisions or for brevity)
#' If an alias is not explicitly specified, the name of the
#' package/module is used.
#' @param importInternal \code{logical} value, default \code{FALSE}.
#' Specifies whether unexported functions shall be imported.
#'
#' @export
#'
#' @examples
#' if (juliaSetupOk()) {
#'
#'    # Importing a package and using one of its exported functions
#'    juliaImport("UUIDs")
#'    juliaCall("string", UUIDs.uuid4())
#'
#'    # Functions that are not exported can be imported
#'    # by specifying the argument "importInternal":
#'    juliaImport("Pkg", importInternal = TRUE)
#'    Pkg.status()
#'
#'    # Importing a module without a package
#'    testModule <- system.file("examples", "TestModule1.jl",
#'                              package = "JuliaConnectoR")
#'    # take a look at the file
#'    writeLines(readLines(testModule))
#'    # load in Julia
#'    juliaCall("include", testModule)
#'    # import in R
#'    juliaImport(".TestModule1")
#'    TestModule1.test1()
#'
#' }
#'
#' \dontshow{
#' JuliaConnectoR:::stopJulia()
#' }
#' 
#' if (juliaSetupOk()) {
#'
#'    # Importing a submodule
#'    testModule <- system.file("examples", "TestModule1.jl",
#'                              package = "JuliaConnectoR")
#'    juliaCall("include", testModule)
#'    juliaImport(".TestModule1.SubModule1")
#'    # call exported function of submodule via module path
#'    SubModule1.test2()
#'    juliaImport(".TestModule1.SubModule1", alias = "Sub1")
#'    # call exported function of submodule via alias
#'    Sub1.test2()
#'
#' }
#' 
#' \dontshow{
#' JuliaConnectoR:::stopJulia()
#' }
juliaImport <- function(modulePath, alias = NULL,
                        importInternal = FALSE) {
   attachJuliaPackage(modulePath, alias,
                      importInternal = importInternal)
   invisible()
}
