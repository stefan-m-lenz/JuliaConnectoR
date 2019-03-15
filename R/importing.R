attachFunctionList <- function(funnames, name, rPrefix, juliaPrefix,
                               constructors = FALSE) {

   if (constructors) {
      funlist <- lapply(funnames, function(funname) {
         constructor <- function(...) {
            juliaCall(paste0(juliaPrefix, funname), ...)
         }
         attributes(constructor)$JLDATATYPE <- funname
         constructor
      })
   } else {
      funlist <- lapply(funnames, function(funname) {
         function(...) {
            juliaCall(paste0(juliaPrefix, funname), ...)
         }
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


attachJuliaPackage <- function(pkgName, alias, mode, importInternal = FALSE) {
   ensureJuliaConnection()

   if (length(pkgName) != 1) {
      stop("Expected exactly one package name")
   }

   if (mode == LOAD_MODE_USING) {
      loadMode <- "using"
      juliaPrefixExported <- ""
      rPrefixExported <- ""
   } else if (mode == LOAD_MODE_IMPORT) {
      loadMode <- "import"
      juliaPrefixExported <- paste0(pkgName, ".")
      rPrefixExported <- paste0(alias, ".")
   } else {
      stop(paste("Unknown mode:", mode))
   }

   juliaEval(paste(loadMode, pkgName))

   pkgContent <- juliaCall("RConnector.pkgContentList", pkgName, all = importInternal)
   if (!is.list(pkgContent)) {
      # must be an error
      stop(paste0("Could not load Julia package \"",  pkgName,
                  "\" (is it installed?): ", pkgContent))
   }

   attachFunctionList(pkgContent$exportedFunctions, pkgName,
                      rPrefixExported, juliaPrefixExported)
   attachFunctionList(pkgContent$exportedDataTypes, pkgName,
                      rPrefixExported, juliaPrefixExported,
                      constructors = TRUE)

   juliaPrefixInternal <- paste0(pkgName, ".")
   rPrefixInternal <- paste0(alias, ".")

   if (mode == LOAD_MODE_USING) {
      attachFunctionList(pkgContent$exportedFunctions, pkgName,
                         rPrefixInternal, juliaPrefixInternal)
      attachFunctionList(pkgContent$exportedDataTypes, pkgName,
                         rPrefixInternal, juliaPrefixInternal,
                         constructors = TRUE)
   }

   if (importInternal) {
      attachFunctionList(pkgContent$internalFunctions, pkgName,
                         rPrefixInternal, juliaPrefixInternal)
      attachFunctionList(pkgContent$internalDataTypes, pkgName,
                         rPrefixInternal, juliaPrefixInternal,
                         constructors = TRUE)
   }
}


juliaUsing <- function(pkgName, alias = pkgName, importInternal = FALSE) {
   attachJuliaPackage(pkgName, alias,
                      mode = LOAD_MODE_USING,
                      importInternal = importInternal)
}


juliaImport <- function(pkgName, alias = pkgName, importInternal = FALSE) {
   attachJuliaPackage(pkgName, alias,
                      mode = LOAD_MODE_IMPORT,
                      importInternal = importInternal)
}
