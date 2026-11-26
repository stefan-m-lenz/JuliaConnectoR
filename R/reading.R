# Reads exactly `n` elements from the connection to Julia.
#
# The socket has a read timeout, so a read that comes back short usually
# means that Julia has not answered yet, e.g. during a long computation,
# and it is retried. But if the Julia process has exited, e.g. after a
# crash, the socket is at its end: every read returns nothing immediately,
# and retrying would spin forever at full CPU. Both cases look the same to
# readBin(). They differ in socketSelect(): a socket at its end is reported
# as readable even though nothing can be read from it.
# An empty read is only taken as the end of the connection if it happens
# twice in a row on a readable socket. After the first one, data may simply
# have arrived just after the read gave up.
readBinFully <- function(what, n, size = NA_integer_, signed = TRUE) {
   con <- pkgLocal$con
   ret <- readBin(con, what, n, size = size, signed = signed)
   emptyAndReadable <- FALSE
   while (length(ret) < n) {
      chunk <- readBin(con, what, n - length(ret), size = size, signed = signed)
      if (length(chunk) > 0) {
         emptyAndReadable <- FALSE
         ret <- c(ret, chunk)
      } else if (isTRUE(socketSelect(list(con), timeout = 0))) {
         if (emptyAndReadable) {
            juliaExited()
         }
         emptyAndReadable <- TRUE
      } else {
         emptyAndReadable <- FALSE
      }
   }
   ret
}


readLogical <- function(n) {
   readBinFully("logical", n, size = 1)
}


readInt <- function(n = 1) {
   readBinFully("integer", n, size = 4)
}


readDouble <- function(n) {
   readBinFully("double", n)
}


readComplex <- function(n) {
   readBinFully("complex", n)
}


readRaw <- function(n) {
   readBinFully("raw", n)
}


readMessageType <- function() {
   readRaw(1)
}


readString <- function() {
   nbytes <- readInt()
   binstr <- readRaw(nbytes)
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
      return(readInt(ndimensions))
   }
}


readNofAttributes <- function() {
   readBinFully("integer", 1, size = 1, signed = FALSE)
}


readObjectReference <- function() {
   objectClassId <- readRaw(1)
   ref <- readRaw(8) # 64 bit reference
   obj <- juliaHeapReference(ref)
   if (objectClassId == OBJECT_CLASS_ID_STRUCT) {
      class(obj) <- c("JuliaStructProxy", "JuliaProxy")
      return(obj)
   } else if (objectClassId == OBJECT_CLASS_ID_ARRAY) {
      class(obj) <- c("JuliaArrayProxy", "JuliaProxy")
      return(obj)
   } else if (objectClassId == OBJECT_CLASS_ID_ANONYMOUS_FUNCTION) {
      fun <- juliaFun("RConnector.callanonymous", pkgLocal$communicator, ref)
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


toDataFrame <- function(aList) {
   ret <- aList
   tryCatch({ret <- data.frame(aList, stringsAsFactors = FALSE)},
            error = function(e) {
               warning("Unable to coerce to data frame.")
            })
   ret
}


readElement <- function() {
   theAttributes <- list()
   typeId <- readRaw(1)
   if (typeId == TYPE_ID_LIST) {
      ret <- readList()
      if (!is.null(attr(ret, "IS_DF"))) {
         ret <- toDataFrame(ret)
      }
      return(ret)
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
         ret <- readDouble(nElements)
         theAttributes <- c(theAttributes, readAttributes())
      } else if (typeId == TYPE_ID_INTEGER) {
         ret <- readInt(nElements)
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
         ret <- readComplex(nElements)
         theAttributes <- c(theAttributes, readAttributes())
      } else if (typeId == TYPE_ID_RAW) {
         ret <- readRaw(nElements)
         theAttributes <- c(theAttributes, readAttributes())
      } else {
         warning(paste("Read invalid type ID", typeId))
         stopJulia()
         stop("Stopped Julia due to protocol error")
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
   rawOutput <- readRaw(outputLength)
   # interpret as string
   output <- rawToChar(rawOutput)
   if (as.raw(0x1B) %in% rawOutput) {
      # remove ANSI escape sequences,
      # because they make trouble, especially with RStudio
      output <- gsub('\x1B(?:[@-Z\\\\-_]|\\[[0-?]*[ -/]*[@-~])', "", output)
   }

   Encoding(output) <- "UTF-8"

   cat(output, file = writeTo)
}
