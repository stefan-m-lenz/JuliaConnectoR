writeString <- function(str) {
   str <- enc2utf8(str)
   utf8bytes <- charToRaw(str)
   writeBin(length(utf8bytes), pkgLocal$con)
   writeBin(utf8bytes, pkgLocal$con)
}

writeInt <- function(n) {
   writeBin(as.integer(n), pkgLocal$con, size = 4)
}

writeFloat64 <- function(x) {
   writeBin(x, pkgLocal$con)
}

writeLogical <- function(b) {
   writeBin(as.logical(b), pkgLocal$con, size = 1)
}


dimensions <- function(x) {
   if (is.null(dim(x))) {
      if (length(x) == 1) {
         return(0)
      } else {
         return(c(1, length(x)))
      }
   } else {
      return(c(length(dim(x)), dim(x)))
   }
}


writeElement <- function(elem, callbacks = list()) {

   if (is.null(elem)) {
      writeBin(TYPE_ID_NULL, pkgLocal$con)
      return(callbacks)
   }

   elemType <- typeof(elem)
   if (elemType == "closure") {
      if (is.null(attr(elem, "JLDATATYPE"))) {
         writeBin(TYPE_ID_CALLBACK, pkgLocal$con)
         if (identical(elem, emptyfun)) {
             writeInt(0L)
         } else {
            callbacks <- c(callbacks, elem)
            print(paste("write callback ", length(callbacks)))
            writeInt(length(callbacks))
         }
      } else {
         writeExpression(attr(elem, "JLDATATYPE"))
      }
   } else {
      typeId <- TYPE_IDS[[typeof(elem)]]
      if (is.null(typeId)) {
         writeBin(TYPE_ID_NULL, pkgLocal$con)
         warning(paste0("Could not coerce type of element ", element, ". Writing NULL."))
      }

      if (typeId <= TYPE_ID_RAW) { # clearly defined number of bytes
         writeBin(typeId, pkgLocal$con)
         writeInt(dimensions(elem))
         writeBin(as.vector(elem), pkgLocal$con)
      } else if (typeId == TYPE_ID_INTEGER) {
         writeBin(TYPE_ID_INTEGER, pkgLocal$con)
         writeInt(dimensions(elem))
         writeInt(elem)
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
         } else {
            writeExpression(elem)
         }
      } else if (typeId == TYPE_ID_LIST) {
         writeBin(typeId, pkgLocal$con)
         writeList(elem)
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

   npositional <- Position(function(name) {name != ""}, theNames,
                           nomatch = length(theList) + 1) - 1
   writeInt(npositional)
   for (i in seq_len(npositional)) {
      callbacks <- c(callbacks, writeElement(theList[[i]], callbacks))
   }

   nnamed <- length(theList) - npositional
   writeInt(nnamed)
   if (nnamed > 0) {
      for (i in (npositional + 1):length(theList)) {
         writeString(theNames[i])
         callbacks <- c(callbacks, writeElement(theList[[i]], callbacks))
      }
   }

   listAttributes <- attributes(theList)
   listAttributes[["names"]] <- NULL # prevents infinite recursion
   nAttributes <- length(listAttributes)
   attributeNames <- names(listAttributes)
   writeInt(nAttributes)
   for (i in seq_len(nAttributes)) {
      writeString(attributeNames[i])
      writeElement(listAttributes[[i]])
   }

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

