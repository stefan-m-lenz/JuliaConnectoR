# Outgoing messages are assembled in an in-memory buffer, pkgLocal$out, and
# sent to Julia with one writeBin() call (see sendBuffered()). Every writeBin()
# on the socket connection waits for the socket and sends a packet of its own,
# which made writing the parts of a message one by one dominate the time of
# calls with small arguments.
beginMessage <- function() {
   out <- pkgLocal$out
   if (is.null(out)) {
      pkgLocal$out <- rawConnection(raw(0), open = "r+b")
   } else {
      seek(out, 0)
      truncate(out)
   }
}

# Sends what has been buffered of the current message to Julia
sendBuffered <- function() {
   out <- pkgLocal$out
   writeBin(rawConnectionValue(out), pkgLocal$con)
   seek(out, 0)
   truncate(out)
}

writeCallMessage <- function(name, args) {
   beginMessage()
   writeBin(CALL_INDICATOR, pkgLocal$out)
   writeString(name)
   writeList(args)
   sendBuffered()
}

writeString <- function(str) {
   str <- enc2utf8(str)
   utf8bytes <- charToRaw(str)
   writeBin(length(utf8bytes), pkgLocal$out)
   writeBin(utf8bytes, pkgLocal$out)
}

# Writes the strings as the protocol's sequence of byte counts and UTF-8 bytes,
# all at once, instead of with two writeBin() calls per string
writeStrings <- function(strs) {
   n <- length(strs)
   if (n == 0) {
      return(invisible(NULL))
   }

   strs <- enc2utf8(strs)
   # (an NA is written as "NA", like writeString() does)
   nbytes <- as.vector(nchar(strs, type = "bytes", keepNA = FALSE))
   strbytes <- charToRaw(paste0(strs, collapse = ""))

   ret <- raw(4 * n + length(strbytes))
   headerStarts <- 4 * (seq_len(n) - 1) + c(0, cumsum(as.double(nbytes))[-n])
   headerPositions <- rep(headerStarts, each = 4) + 1:4
   ret[headerPositions] <- writeBin(nbytes, raw(), size = 4)
   ret[-headerPositions] <- strbytes
   writeArray(ret, size = 1L)
}

writeInt <- function(n) {
   writeBin(as.integer(n), pkgLocal$out, size = 4)
}

# define extra variable here to be able to change the value for debugging purposes
WRITE_BIN_MAX <- .Machine$integer.max

# Arrays of at least this many bytes are written to the socket directly
# instead of being copied into the message buffer first
DIRECT_WRITE_MIN <- 65536

# Wrapper around writeBin, writes to the current message.
# Can write arrays with more than 2^31 - 1 bytes, which writeBin cannot.
writeArray <- function(arr, size) {
   arr <- as.vector(arr)
   nbytes <- as.double(size) * as.double(length(arr))
   if (nbytes < DIRECT_WRITE_MIN) {
      writeBin(arr, pkgLocal$out, size = size)
      return(invisible(NULL))
   }

   # the part of the message before the array goes first
   sendBuffered()

   if (nbytes > WRITE_BIN_MAX) {
      offset <- 0
      nMaxWritable <- as.integer(floor(WRITE_BIN_MAX / size))
      while (offset < length(arr)) {
         chunkLength <- min(length(arr) - offset, nMaxWritable)
         chunk <- arr[(offset + 1):(offset + chunkLength)]
         writeBin(chunk, pkgLocal$con, size = size)
         offset <- offset + chunkLength
      }
   } else {
      writeBin(arr, pkgLocal$con, size = size)
   }
}

writeNofAttributes <- function(n) {
   writeBin(as.raw(n), pkgLocal$out)
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
   writeBin(TYPE_ID_CALLBACK, pkgLocal$out)
   callbackId <- registerCallback(elem)
   writeString(callbackId)
}


writeObjectReference <- function(objectClassId, ref) {
   writeBin(TYPE_ID_OBJECT_REFERENCE, pkgLocal$out)
   writeBin(objectClassId, pkgLocal$out)
   writeBin(ref, pkgLocal$out)
}


writeElement <- function(elem) {

   if (is.null(elem)) {
      writeBin(TYPE_ID_NULL, pkgLocal$out)
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
   } else if (inherits(elem, "data.frame")) {
      writeBin(TYPE_ID_LIST, pkgLocal$out)
      elem <- as.list(elem)
      attr(elem, "IS_DF") <- TRUE
      writeList(elem)
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
         writeBin(typeId, pkgLocal$out)
         writeInt(dimensions(elem))
         writeArray(elem, size = TYPE_SIZES[[elemType]])
         writeAttributes(attributes(elem))
      } else if (typeId == TYPE_ID_INTEGER) {
         writeBin(TYPE_ID_INTEGER, pkgLocal$out)
         writeInt(dimensions(elem))
         writeArray(as.integer(elem), size = 4L)
         writeAttributes(attributes(elem))
      } else if (typeId == TYPE_ID_LOGICAL) {
         if (anyNA(elem)) {
            writeBin(TYPE_ID_INTEGER, pkgLocal$out)
            writeInt(dimensions(elem))
            writeArray(as.integer(elem), size = 4L)
            writeAttributes(list("JLTYPE" = "Union{Missing, Bool}"))
         } else {
            writeBin(TYPE_ID_LOGICAL, pkgLocal$out)
            writeInt(dimensions(elem))
            writeArray(as.logical(elem), size = 1L)
         }
      } else if (typeId == TYPE_ID_STRING) {
         if (is.null(attr(elem, "JLEXPR"))) {
            writeBin(TYPE_ID_STRING, pkgLocal$out)
            writeInt(dimensions(elem))
            writeStrings(elem)
            if (anyNA(elem)) {
               attr(elem, "NA") <- which(is.na(elem))
            }
            writeAttributes(attributes(elem))
         } else {
            writeExpression(elem)
         }
      } else if (typeId == TYPE_ID_LIST) {
         writeBin(typeId, pkgLocal$out)
         writeList(elem)
      } else if (typeId == TYPE_ID_SYMBOL) {
         writeBin(typeId, pkgLocal$out)
         writeString(as.character(elem))
      }
   }
}


writeExpression <- function(str) {
   writeBin(TYPE_ID_EXPRESSION, pkgLocal$out)
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
   beginMessage()
   writeBin(FAIL_INDICATOR, pkgLocal$out)
   writeString(msgStr)
   sendBuffered()
}

writeResultMessage <- function(result) {
   beginMessage()
   writeBin(RESULT_INDICATOR, pkgLocal$out)
   writeElement(result)
   sendBuffered()
}

