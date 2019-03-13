writeString <- function(str) {
   str <- enc2utf8(str)
   utf8bytes <- charToRaw(str)
   writeBin(length(utf8bytes), con)
   writeBin(utf8bytes, con)
}

writeInt <- function(n) {
   writeBin(as.integer(n), con, size = 4)
}

writeFloat64 <- function(x) {
   writeBin(x, con)
}

writeLogical <- function(b) {
   writeBin(as.logical(b), con, size = 1)
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

writeElement <- function(elem) {

   if (is.null(elem)) {
      writeBin(TYPE_ID_NULL)
      return()
   }

   typeId <- TYPE_IDS[[typeof(elem)]]
   if (is.null(typeId)) {
      stop("Type of element could not be identified") # TODO write a NULL value instead
   }

   writeBin(typeId, con)

   if (typeId <= TYPE_ID_INTEGER) {
      writeInt(dimensions(elem))
      writeBin(as.vector(elem), con)
   } else if (typeId == TYPE_ID_LOGICAL) {
      writeInt(dimensions(elem))
      writeLogical(elem)
   } else if (typeId == TYPE_ID_STRING) {
      writeInt(dimensions(elem))
      for (i in 1:length(elem)) {
         writeString(elem[i])
      }
   } else if (typeId == TYPE_ID_LIST) {
      writeList(elem)
   }
}


writeList <- function(theList) {
   theNames <- names(theList)

   npositional <- Position(function(name) {name != ""}, theNames,
                           nomatch = length(theList) + 1) - 1
   writeInt(npositional)
   for (i in seq_len(npositional)) {
      writeElement(theList[[i]])
   }

   nnamed <- length(theList) - npositional
   writeInt(nnamed)
   if (nnamed > 0) {
      for (i in (npositional + 1):length(theList)) {
         writeString(theNames[i])
         writeElement(theList[[i]])
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
}


