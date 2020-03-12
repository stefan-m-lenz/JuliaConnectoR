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
   binstr <- readBin(pkgLocal$con, "raw", nbytes)
   retstr <- NULL
   try({retstr <- rawToChar(binstr)}, silent = TRUE)
   if (is.null(retstr)) {
      # Error: probably NUL character, which is allowed in Julia
      retstr <- binstr
      attr(retstr, "JLTYPE") <- "String"
   } else {
      Encoding(retstr) <- "UTF-8"
      retstr
   }
   retstr
}

readStrings <- function(n = 1) {
   ret <- character(n)
   for (i in seq_len(n)) {
      str <- readString()
      if (is.raw(str)) {
         if (n == 1) {
            return(str)
         } else {
            ret <- as.list(ret)
            ret[[i]] <- str
            return(c(ret[1:i], readStringList(n - i)))
         }
      }
      ret[i] <- str
   }
   ret
}

readStringList <- function(n) {
   if (n==0) {
      return(list())
   } else {
      ret <- list()
      ret[[n]] <- NULL
      for (i in seq_len(n)) {
         ret[[i]] <- readString()
      }
      return(ret)
   }
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

readListAttributes <- function() {
   listAttributes <- readAttributes()

   # If the attribute "JLREF" is given, attach the environment
   # managing a possible Julia heap reference
   jlRefAttr <- listAttributes[["JLREF"]]
   if (!is.null(jlRefAttr)) {
      listAttributes[["JLREF"]] <- juliaHeapReference(jlRefAttr)
   }

   listAttributes
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


readObjectReference <- function() {
   objectClassId <- readBin(pkgLocal$con, "raw", 1)
   ref <- readBin(pkgLocal$con, "raw", 8) # 64 bit reference
   obj <- juliaHeapReference(ref)
   if (objectClassId == OBJECT_CLASS_ID_STRUCT) {
      class(obj) <- c("JuliaStructProxy", "JuliaProxy")
      return(obj)
   } else if (objectClassId == OBJECT_CLASS_ID_ARRAY) {
      class(obj) <- c("JuliaArrayProxy", "JuliaProxy")
      return(obj)
   } else if (objectClassId == OBJECT_CLASS_ID_ANONYMOUS_FUNCTION) {
      fun <- juliaFun("RConnector.callanonymous", ref)
      attr(fun, "JLREF") <- obj
      return(fun)
   } else if (objectClassId == OBJECT_CLASS_ID_SIMPLE_ARRAY) {
      class(obj) <- c("JuliaSimpleArrayProxy", "JuliaArrayProxy", "JuliaProxy")
      return(obj)
   } else if (objectClassId == OBJECT_CLASS_ID_NO_INFO) {
      class(obj) <- "JuliaProxy"
      return(obj)
   } else {
      stop(paste("Unknown object class", objectClassId))
   }
}


readElement <- function() {
   theAttributes <- list()
   typeId <- readBin(pkgLocal$con, "raw", 1)
   if (typeId == TYPE_ID_LIST) {
      return(readList())
   } else if (typeId == TYPE_ID_NULL) {
      return(NULL)
   } else if (typeId == TYPE_ID_EXPRESSION) {
      expr <- readString()
      attr(expr, "JLEXPR") <- TRUE
      return(expr)
   } else if (typeId == TYPE_ID_OBJECT_REFERENCE) {
      return(readObjectReference())
   } else if (typeId == TYPE_ID_NAMED_FUNCTION) {
      funname <- readString()
      return(juliaFun(funname))
   } else if (typeId == TYPE_ID_CALLBACK) {
      callbackId <- readString()
      return(get(callbackId, pkgLocal$callbacks))
   } else if (typeId == TYPE_ID_SYMBOL) {
      return(as.symbol(readString()))
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
         newAttrs <- readAttributes()
         if (!is.null(newAttrs[["R_LOGICAL"]])) {
            ret <- as.logical(ret)
            newAttrs[["R_LOGICAL"]] <- NULL
         }
         theAttributes <- c(theAttributes, newAttrs)
      } else if (typeId == TYPE_ID_LOGICAL) {
         ret <- readLogical(nElements)
      } else if (typeId == TYPE_ID_STRING) {
         ret <- readStrings(nElements)
         strAttributes <- readAttributes()
         if ("NA" %in% names(strAttributes)) {
            ret[strAttributes[["NA"]]] <- NA
            strAttributes[["NA"]] <- NULL
         }
         theAttributes <- c(theAttributes, strAttributes)
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


readList <- function() {
   ret <- list()

   npositional <- readInt()
   for (i in seq_len(npositional)) {
      listElement <- readElement()
      if (is.null(listElement)) {
         ret[i] <- list(NULL)
      } else {
         ret[[i]] <- listElement
      }
   }

   nnamed <- readInt()
   for (i in seq_len(nnamed)) {
      name <- readString()
      listElement <- readElement()
      if (is.null(listElement)) {
         ret[name] <- list(NULL)
      } else {
         ret[[name]] <- listElement
      }
   }

   attributes(ret) <- c(list(names = names(ret)), readListAttributes())
   ret
}


readCall <- function() {
   name <- readString()
   args <- readList()
   list(name = name, args = args)
}


readOutput <- function(writeTo) {
   outputLength <- readInt()
   output <- readBin(pkgLocal$con, "raw", outputLength)
   # interpret as string
   output <- rawToChar(output)
   Encoding(output) <- "UTF-8"
   # TODO: check if valid UTF-8? binary output possible?
   cat(output, file = writeTo)
}
