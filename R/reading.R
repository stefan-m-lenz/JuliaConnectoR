readMessageType <- function() {
   messageType <- c()
   while (length(messageType) == 0) {
      messageType <- readBin(pkgLocal$con, "raw", 1)
   }
   messageType
}


readLogical <- function(n) {
   readBin(pkgLocal$con, "logical", n = n, size = 1)
}


readInt <- function() {
   ret <- integer()
   while(length(ret) == 0) {
      ret <- readBin(pkgLocal$con, "integer", 1, size = 4)
   }
   ret
}

readInts <- function(n) {
   readBin(pkgLocal$con, "integer", n, size = 4)
}


readString <- function() {
   nbytes <- readInt()
   ret <- readBin(pkgLocal$con, "raw", nbytes)
   ret <- rawToChar(ret)
   Encoding(ret) <- "UTF-8"
   ret
}

readStrings <- function(n = 1) {
   ret <- character(n)
   for (i in seq_len(n)) {
      ret[i] <- readString()
   }
   ret
}


readAttributes <- function() {
   nAttributes <- readNofAttributes()
   theAttributes <- list()
   for (i in seq_len(nAttributes)) {
      name <- readString()
      theAttributes[[name]] <- readElement()
   }
   theAttributes
}


addAttributes <- function(x, theAttributes) {
   for (attrKey in names(theAttributes)) {
      attr(x, attrKey) <- theAttributes[[attrKey]]
   }
   x
}


readDimensions <- function() {
   ndimensions <- readInt()
   if (ndimensions == 0) {
      return(c())
   } else {
      return(readInts(ndimensions))
   }
}

readNofAttributes <- function() {
   readBin(pkgLocal$con, "integer", size = 1, signed = FALSE)
}


readElement <- function(callbacks) {
   theAttributes <- list()
   typeId <- readBin(pkgLocal$con, "raw", 1)
   if (typeId == TYPE_ID_LIST) {
      return(readList(callbacks))
   } else if (typeId == TYPE_ID_NULL) {
      return(NULL)
   } else if (typeId == TYPE_ID_EXPRESSION) {
      expr <- readString()
      attr(expr, "JLEXPR") <- TRUE
      return(expr)
   } else if (typeId == TYPE_ID_CALLBACK) {
      callbackId <- readInt()
      if (callbackId == 0) {
         return(emptyfun)
      } else {
         return(callbacks[[callbackId]])
      }
   } else {
      dimensions <- readDimensions()
      nElements <- prod(dimensions)
      if (nElements == 1 && length(dimensions) > 0) {
         theAttributes <- list("JLDIM" = dimensions)
      }

      if (typeId == TYPE_ID_DOUBLE) {
         ret <- readBin(pkgLocal$con, "double", nElements)
         theAttributes <- c(theAttributes, readAttributes())
      } else if (typeId == TYPE_ID_INTEGER) {
         ret <- readInts(nElements)
         theAttributes <- c(theAttributes, readAttributes())
      } else if (typeId == TYPE_ID_LOGICAL) {
         ret <- readLogical(nElements)
      } else if (typeId == TYPE_ID_STRING) {
         ret <- readStrings(nElements)
         theAttributes <- c(theAttributes, readAttributes())
      } else if (typeId == TYPE_ID_COMPLEX) {
         ret <- readBin(pkgLocal$con, "complex", nElements)
         theAttributes <- c(theAttributes, readAttributes())
      } else if (typeId == TYPE_ID_RAW) {
         ret <- readBin(pkgLocal$con, "raw", nElements)
         theAttributes <- c(theAttributes, readAttributes())
      } else {
         stopJulia()
         stop(paste("Invalid type ID", typeId))
      }

      if (length(dimensions) > 1) { # reshape
         ret <- array(ret, dim = dimensions)
      }
      ret <- addAttributes(ret, theAttributes)
   }
   ret
}


readList <- function(callbacks = list()) {
   ret <- list()

   npositional <- readInt()
   for (i in seq_len(npositional)) {
      ret[[i]] <- readElement(callbacks)
   }

   nnamed <- readInt()
   for (i in seq_len(nnamed)) {
      name <- readString()
      ret[[name]] <- readElement(callbacks)
   }

   attributes(ret) <- c(list(names = names(ret)), readAttributes())
   ret
}


readCall <- function(callbacks = list()) {
   name <- readString()
   args <- readList(callbacks)
   list(name = name, args = args)
}
