writeString <- function(str) {
   str <- enc2utf8(str)
   utf8bytes <- charToRaw(str)
   writeBin(length(utf8bytes), pkgLocal$con)
   writeBin(utf8bytes, pkgLocal$con)
}

writeInt <- function(n) {
   writeBin(as.integer(n), pkgLocal$con, size = 4)
}

writeLogical <- function(b) {
   writeBin(as.logical(b), pkgLocal$con, size = 1)
}

writeAnonymousFunctionReference <- function(ref) {
   writeBin(ref, pkgLocal$con)
}

writeNofAttributes <- function(n) {
   writeBin(as.raw(n), pkgLocal$con)
}

dimensions <- function(x) {
   if (is.null(dim(x))) {
      if (length(x) == 1) {
         if (is.null(attr(x, "JLDIM"))) {
            return(0)
         } else {
            jldim <- attr(x, "JLDIM")
            return(c(length(jldim), jldim))
         }
      } else {
         return(c(1, length(x)))
      }
   } else {
      return(c(length(dim(x)), dim(x)))
   }
}


writeAttributes <- function(theAttributes) {
   theAttributes[["dim"]] <- NULL
   theAttributes[["dimnames"]] <- NULL
   theAttributes[["names"]] <- NULL # prevents infinite recursion

   nAttributes <- length(theAttributes)
   attributeNames <- names(theAttributes)
   writeNofAttributes(nAttributes)

   for (i in seq_len(nAttributes)) {
      writeString(attributeNames[i])
      writeElement(theAttributes[[i]])
   }
}


writeListAttributes <- function(theAttributes) {
   # The attribute "JLREF" needs to be handled in a special way:
   # Do not transfer the environment managing the reference,
   # only the reference itself.
   jlRefEnv <- theAttributes[["JLREF"]]
   if (!is.null(jlRefEnv)) {
      theAttributes[["JLREF"]] <- jlRefEnv$ref
   }

   writeAttributes(theAttributes)
}


writeElement <- function(elem, callbacks = list()) {

   if (is.null(elem)) {
      writeBin(TYPE_ID_NULL, pkgLocal$con)
      return(callbacks)
   }

   elemType <- typeof(elem)
   if (elemType == "closure") {
      if (!is.null(attr(elem, "JLTYPE"))) {
         writeExpression(attr(elem, "JLTYPE"))
      } else if (!is.null(attr(elem, "JLFUN"))) {
         writeExpression(attr(elem, "JLFUN"))
      } else if (!is.null(attr(elem, "JLREF"))) {
         # it's an anonymous function
         writeBin(TYPE_ID_ANONYMOUS_FUNCTION, pkgLocal$con)
         writeAnonymousFunctionReference(attr(elem, "JLREF")$ref)
      } else {
         writeBin(TYPE_ID_CALLBACK, pkgLocal$con)
         callbacks <- c(callbacks, elem)
         writeInt(length(callbacks))
      }
   } else {
      typeId <- TYPE_IDS[[typeof(elem)]]
      if (is.null(typeId)) {
         writeBin(TYPE_ID_NULL, pkgLocal$con)
         warning(paste0("Could not coerce type of element ", elem, ". Writing NULL."))
      }

      if (typeId <= TYPE_ID_RAW) {
         # all types with a clearly defined number of bytes in R
         writeBin(typeId, pkgLocal$con)
         writeInt(dimensions(elem))
         writeBin(as.vector(elem), pkgLocal$con)
         writeAttributes(attributes(elem))
      } else if (typeId == TYPE_ID_INTEGER) {
         writeBin(TYPE_ID_INTEGER, pkgLocal$con)
         writeInt(dimensions(elem))
         writeInt(elem)
         writeAttributes(attributes(elem))
      } else if (typeId == TYPE_ID_LOGICAL) {
         writeBin(TYPE_ID_LOGICAL, pkgLocal$con)
         writeInt(dimensions(elem))
         writeLogical(elem)
      } else if (typeId == TYPE_ID_STRING) {
         if (is.null(attr(elem, "JLEXPR"))) {
            writeBin(TYPE_ID_STRING, pkgLocal$con)
            writeInt(dimensions(elem))
            for (i in 1:length(elem)) {
               writeString(elem[i])
            }
            writeAttributes(attributes(elem))
         } else {
            writeExpression(elem)
         }
      } else if (typeId == TYPE_ID_LIST) {
         writeBin(typeId, pkgLocal$con)
         callbacks <- writeList(elem, callbacks)
      }
   }

   return(callbacks)
}


writeExpression <- function(str) {
   writeBin(TYPE_ID_EXPRESSION, pkgLocal$con)
   writeString(str)
}


writeList <- function(theList, callbacks = list()) {
   theNames <- names(theList)

   if (is.null(theNames)) {
      posargs <- theList
      nnamed <- 0
   } else {
      named <- (theNames != "")
      posargs <- theList[!named]
      namedargs <- theList[named]
      namedNames <- theNames[named]
      nnamed <- length(namedargs)
   }

   npositional <- length(posargs)
   writeInt(npositional)
   for (arg in posargs) {
      callbacks <- writeElement(arg, callbacks)
   }

   writeInt(nnamed)
   if (nnamed > 0) {
      for (i in seq_along(namedargs)) {
         writeString(namedNames[i])
         callbacks <- writeElement(namedargs[[i]], callbacks)
      }
   }

   writeListAttributes(attributes(theList))
   return(callbacks)
}


writeFailMessage <- function(msgStr) {
   writeBin(FAIL_INDICATOR, pkgLocal$con)
   writeString(msgStr)
}

writeResultMessage <- function(result) {
   writeBin(RESULT_INDICATOR, pkgLocal$con)
   writeElement(result)
}

