## Cases.R -- Functions dealing with cases and case files.


CaseFileDelimiter <- function (newdelimiter=NULL) {
  if (length(newdelimiter)> 1) {
    stop("Only one delimiter allowed.")
  }
  if (!is.null(newdelimiter)) {
    if (!is.character(newdelimiter)) {
      stop("Expected a character, got",newdelimiter)
    }
    if (nchar(newdelimiter)>1) {
      warning("Delimiter has multiple characters, only the first will be used.")
    }
    newdelimiter <- utf8ToInt(newdelimiter)
    if (length(newdelimiter)==0) newdelimiter <- 0
  }
  olddelim <- .Call("RN_CaseFileDelimiter",newdelimiter,PACKAGE="RNetica")
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  intToUtf8(olddelim)
}

CaseFileMissingCode <- function (newcode=NULL) {
  if (length(newcode)> 1) {
    stop("Only one code allowed.")
  }
  if (!is.null(newcode)) {
    if (!is.character(newcode)) {
      stop("Expected a character, got",newcode)
    }
    if (nchar(newcode)>1) {
      warning("Missing code has multiple characters, only the first will be used.")
    }
    if (nchar(newcode)==0) {
      newcode <- 0L
    } else {
      newcode <- utf8ToInt(newcode)
    }
  }
  oldcode <- .Call("RN_MissingCode",newcode,PACKAGE="RNetica")
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  intToUtf8(oldcode)
}

###############################################################
## Case Stream Objects

OpenCaseStream <- function (oldstream) {
  if (is.NeticaCaseStream(oldstream)) {
    if (isCaseStreamOpen(oldstream)) {
      warning("Stream is already open:  nothing done.")
      return(oldstream)
    }
    if (is.CaseFileStream(oldstream)) {
      source <- getCaseStreamPath(oldstream)
      stream <-
        .Call("RN_OpenCaseFileStream",source,oldstream,PACKAGE="RNetica")
    } else {
      label <- getCaseStreamDataFrameName(oldstream)
      source <- MemoryStreamContents(oldstream)
      stream <-
        .Call("RN_OpenCaseMemoryStream",label,oldstream,PACKAGE="RNetica")
      print(source)
      MemoryStreamContents(stream) <- source
    }
  } else {
    stop("expected oldstram to be a NeticaCaseStream")
  }
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  stream
}

CaseFileStream <- function (pathname) {
  if (!is.character(pathname) || length(pathname)>1) {
    warning("OpenCaseStream:  expected single pathname as argument.")
  }
  stream <-
    .Call("RN_OpenCaseFileStream",pathname,NULL,PACKAGE="RNetica")
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  stream
}

MemoryCaseStream <- function (data.frame,
                              label=deparse(substitute(data.frame))) {
  stream <-
    .Call("RN_OpenCaseMemoryStream",label,NULL,PACKAGE="RNetica")
  MemoryStreamContents(stream) <- data.frame
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  stream
}


CloseCaseStream <- function (stream) {
  if (!is.NeticaCaseStream(stream))
    stop("Trying to close a non-stream object.")
  if (!isCaseStreamOpen(stream)) {
    warning("CloseCaseStream:  Stream already closed.")
    return (stream)
  }
  stream <- .Call("RN_CloseCaseStream",stream,PACKAGE="RNetica")
  stream
}

toString.NeticaCaseStream <- function (x, ...) {
  status <- ifelse(isCaseStreamOpen(x),"Open","Closed")
  src <- toString(unclass(x))
  paste("<",status,class(x)[1],":",src,">")
}

print.NeticaCaseStream <- function(x, ...) {
  cat(toString(x),"\n")
}


is.NeticaCaseStream <- function (x) {
  is(x,"NeticaCaseStream")
}

is.MemoryCaseStream <- function (x) {
  is(x,"MemoryCaseStream")
}

is.CaseFileStream <- function (x) {
  is(x,"CaseFileStream")
}

isCaseStreamOpen <- function (stream) {
  if (!is.NeticaCaseStream(stream)) return (NA_integer_)
  .Call("RN_isCaseStreamActive",stream,PACKAGE="RNetica")
}

getCaseStreamPath <- function (stream) {
  attr(stream,"Case_Stream_Path")
}

getCaseStreamLastId <- function (stream) {
  attr(stream,"Case_Stream_Lastid")
}
getCaseStreamPos <- function (stream) {
  attr(stream,"Case_Stream_Position")
}
getCaseStreamLastFreq <- function (stream) {
  attr(stream,"Case_Stream_Lastfreq")
}

getCaseStreamDataFrameName <- function (stream) {
  attr(stream,"Case_Stream_DataFrameName")
}



#####################################################################
## Reading and writing Cases

WriteFindings <- function (nodes,pathOrStream,id=-1L,freq=-1.0) {
  if (!is.list(nodes) || !all(sapply(nodes,is.NeticaNode)) ||
      !all(sapply(nodes,is.active))) {
    stop("Argument nodes must be a list of active Netica nodes.")
  }
  if (is.NeticaCaseStream(pathOrStream)) {
    if (!isCaseStreamOpen(pathOrStream)) {
      stop("Stream is not open for writing.")
    }
  } else if (!is.character(pathOrStream) || length(pathOrStream) >1L) {
    stop("expected filename (length 1) or CaseStream, got",
         pathOrStream)
  }
  id <- as.integer(id)
  if (length(id) >1L) {
    stop("Argument id must be an integer scalar.")
  }
  freq <- as.numeric(freq)
  if (length(freq) >1L) {
    stop("Argument freq must be a numeric scalar.")
  }
  stream <- .Call("RN_WriteFindings",nodes,pathOrStream,id,freq,PACKAGE="RNetica")
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  stream
}

ReadFindings <- function (nodes, stream, pos="NEXT", add=FALSE) {
  if (!is.list(nodes) || !all(sapply(nodes,is.NeticaNode)) ||
      !all(sapply(nodes,is.active))) {
    stop("Argument nodes must be a list of active Netica nodes.")
  }
  if (is.NeticaCaseStream(stream)) {
    if (!isCaseStreamOpen(stream)) {
      stop("Stream is not open for writing.")
    }
  } else if (!is.character(stream) || length(stream) >1L) {
    stop("expected filename (length 1) or CaseStream, got",
         stream)
  }
  add <- as.logical(add)
  if (is.na(add) || length(add) >1L) {
    stop("Argument add must be a logical scalar.")
  }
  if (is.character(pos) && length(pos) == 1L &&
      (toupper(pos)=="FIRST" || toupper(pos)=="NEXT")) {
    pos <- toupper(pos)
    if (pos=="NEXT" && is.null(getCaseStreamPos(stream))) {
      stop("ReadFindings not yet called on stream with pos='FIRST'")
    }
  } else {
    pos <- as.integer(pos)
    if (is.na(pos) || length(pos)>1L || pos < 0) {
      stop("Argument pos must be a postive integer, 'FIRST' or 'NEXT'.")
    }
  }
  stream <- .Call("RN_ReadFindings",nodes,stream,pos,add,PACKAGE="RNetica")
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  stream
}

read.CaseFile <- function(file,...) {
  read.table(file,header=TRUE,sep=CaseFileDelimiter(),
             na.strings=CaseFileMissingCode(),...)
}

write.CaseFile <- function(x,file,...) {
  if (!is.data.frame(x)) {
    stop("Must be data frame.")
  }
  ## Force IDnum and NumCases columns, if the exist, to cannoical positions.
  vars <- names(x)
  has.id <- "IDnum" %in% vars
  has.freq <- "NumCases" %in% vars
  vars <- setdiff(vars,c("IDnum","NumCases"))
  if (has.freq) vars <- c("NumCases",vars)
  if (has.id) vars <- c("IDnum",vars)
  write.table(x[,vars],file,col.names=TRUE,row.names=FALSE, quote=FALSE,
              sep=CaseFileDelimiter(), na=CaseFileMissingCode())

}

MemoryStreamContents <- function (stream) {
  if (!is.MemoryCaseStream(stream)) {
    stop("MemoryStreamContents only worksfor MemoryCaseStreams.")
  }
  if (isCaseStreamOpen(stream)) {
    contents <- .Call("RN_GetMemoryStreamContents",stream,PACKAGE="RNetica")
    ecount <- ReportErrors()
    if (ecount[1]>0) {
      stop("Netica Errors Encountered, see console for details.")
    }
    if (is.null(contents)) {
      result <- NULL
    } else {
      con <- textConnection(contents)
      result <- read.CaseFile(con)
      close(con)
    }
    ## Cache value
    attr(stream,"Case_Stream_DataFrame") <- result
    result
  } else {
    ## Use cached value
    attr(stream,"Case_Stream_DataFrame",exact=TRUE)
  }
}

"MemoryStreamContents<-" <- function (stream,value) {
  if (!is.MemoryCaseStream(stream)) {
    stop("MemoryStreamContents only worksfor MemoryCaseStreams.")
  }
  if (!is.data.frame(value) && !is.null(value)) {
    stop("MemoryStreamContents can only be set to a data.frame or NULL.")
  }
  if (!is.null(value) && is.null(value$IDnum)) {
    ## Force first row to be IDnum
    value <- data.frame(IDnum=1:nrow(value),value)
  }
  attr(stream,"Case_Stream_DataFrame") <- value
  if (isCaseStreamOpen(stream)) {
    contents <- NULL
    if (!is.null(value)) {
      con <- textConnection(NULL,open="w")
      write.CaseFile(value,con)
      contents <- textConnectionValue(con)
      close(con)
    }
    stream <- .Call("RN_SetMemoryStreamContents",stream,contents,PACKAGE="RNetica")
    ecount <- ReportErrors()
    if (ecount[1]>0) {
      stop("Netica Errors Encountered, see console for details.")
    }
  }
  stream
}



WithOpenCaseStream <- function (stream,expr) {
  if (!isCaseStreamOpen(stream)) OpenCaseStream(stream)
  tryCatch(expr,
           finally = CloseCaseStream(stream))
}



