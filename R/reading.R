readLogical <- function(n) {
   readBin(con, "logical", n = n, size = 1)
}


readInt <- function() {
   readBin(con, "integer", 1, size = 4)
}

readInts <- function(n) {
   readBin(con, "integer", n, size = 4)
}


readString <- function() {
   nbytes <- readInt()
   ret <-  readBin(con, "raw", nbytes)
   ret <- rawToChar(ret)
   Encoding(ret) <- "UTF-8"
   ret
}

readStrings <- function(n = 1) {
   ret <- character(n)
   for (i in 1:n) {
      ret[i] <- readString()
   }
   ret
}


readDimensions <- function() {
   ndimensions <- readInt()
   if (ndimensions == 0) {
      return(1) # everything is a vector in R
   } else {
      return(readInts(ndimensions))
   }
}


readElement <- function() {
   typeId <- readBin(con, "raw", 1)
   if (typeId == TYPE_ID_LIST) {
      return(readList())
   } else if (typeId == TYPE_ID_FAIL) {
      return(readString())
   } else if (typeId == TYPE_ID_NULL) {
      return(NULL)
   } else {
      dimensions <- readDimensions()
      nElements <- prod(dimensions)

      if (typeId == TYPE_ID_DOUBLE) {
         ret <- readBin(con, "double", nElements)
      } else if (typeId == TYPE_ID_INTEGER) {
         ret <- readInts(nElements)
      } else if (typeId == TYPE_ID_LOGICAL) {
         ret <- readLogical(nElements)
      } else if (typeId == TYPE_ID_STRING) {
         ret <- readStrings(nElements)
      } else {
         stopJulia()
         stop(paste("Invalid type ID", typeId))
      }

      if (length(dimensions) > 1) {
         ret <- array(ret, dim = dimensions)
      }
   }
   ret
}


readList <- function() {
   ret <- list()

   npositional <- readInt()
   for (i in seq_len(npositional)) {
      ret <- c(ret, list(readElement()))
   }

   nnamed <- readInt()
   for (i in seq_len(nnamed)) {
      name <- readString()
      ret[[name]] <- readElement()
   }

   nAttributes <- readInt()
   listAttributes <- list()
   for (i in seq_len(nAttributes)) {
      name <- readString()
      listAttributes[[name]] <- readElement()
   }
   attributes(ret) <- c(list(names = names(ret)), listAttributes)

   ret
}
