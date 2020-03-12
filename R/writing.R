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


writeCallback <- function(elem) {
   writeBin(TYPE_ID_CALLBACK, pkgLocal$con)
   callbackId <- registerCallback(elem)
   writeString(callbackId)
}


writeObjectReference <- function(objectClassId, ref) {
   writeBin(TYPE_ID_OBJECT_REFERENCE, pkgLocal$con)
   writeBin(objectClassId, pkgLocal$con)
   writeBin(ref, pkgLocal$con)
}


writeElement <- function(elem) {

   if (is.null(elem)) {
      writeBin(TYPE_ID_NULL, pkgLocal$con)
      return()
   }

   if (is.function(elem)) {
      if (!is.null(attr(elem, "JLTYPE"))) {
         writeExpression(attr(elem, "JLTYPE"))
      } else if (!is.null(attr(elem, "JLFUN"))) {
         writeExpression(attr(elem, "JLFUN"))
      } else if (!is.null(attr(elem, "JLREF"))) {
         writeObjectReference(OBJECT_CLASS_ID_ANONYMOUS_FUNCTION,
                              attr(elem, "JLREF")$ref)
      } else {
        writeCallback(elem)
      }
   } else if (inherits(elem, "JuliaProxy")) {
      # (Julia doesn't care about the class)
      writeObjectReference(as.raw(OBJECT_CLASS_ID_NO_INFO),
                           get("ref", elem))
   } else {
      elemType <- typeof(elem)
      typeId <- TYPE_IDS[[elemType]]
      if (is.null(typeId)) {
         warning(paste0("Type ", elemType, ", of element ", elem,
                        " not translatable. Writing NULL.\n"))
         writeElement(NULL)
         return()
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
         if (anyNA(elem)) {
            writeBin(TYPE_ID_INTEGER, pkgLocal$con)
            writeInt(dimensions(elem))
            writeInt(elem)
            writeAttributes(list("JLTYPE" = "Union{Missing, Bool}"))
         } else {
            writeBin(TYPE_ID_LOGICAL, pkgLocal$con)
            writeInt(dimensions(elem))
            writeLogical(elem)
         }
      } else if (typeId == TYPE_ID_STRING) {
         if (is.null(attr(elem, "JLEXPR"))) {
            writeBin(TYPE_ID_STRING, pkgLocal$con)
            writeInt(dimensions(elem))
            for (i in 1:length(elem)) {
               writeString(elem[i])
            }
            if (anyNA(elem)) {
               attr(elem, "NA") <- which(is.na(elem))
            }
            writeAttributes(attributes(elem))
         } else {
            writeExpression(elem)
         }
      } else if (typeId == TYPE_ID_LIST) {
         writeBin(typeId, pkgLocal$con)
         writeList(elem)
      } else if (typeId == TYPE_ID_SYMBOL) {
         writeBin(typeId, pkgLocal$con)
         writeString(as.character(elem))
      }
   }
}


writeExpression <- function(str) {
   writeBin(TYPE_ID_EXPRESSION, pkgLocal$con)
   writeString(str)
}


writeList <- function(theList) {
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
      writeElement(arg)
   }

   writeInt(nnamed)
   if (nnamed > 0) {
      for (i in seq_along(namedargs)) {
         writeString(namedNames[i])
         writeElement(namedargs[[i]])
      }
   }

   writeListAttributes(attributes(theList))
}


writeFailMessage <- function(msgStr) {
   writeBin(FAIL_INDICATOR, pkgLocal$con)
   writeString(msgStr)
}

writeResultMessage <- function(result) {
   writeBin(RESULT_INDICATOR, pkgLocal$con)
   writeElement(result)
}

