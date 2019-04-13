## Cases.R -- Functions dealing with cases and case files.


CaseFileDelimiter <- function (newdelimiter=NULL, session=getDefaultSession()) {
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
  olddelim <- .Call("RN_CaseFileDelimiter",newdelimiter,session,PACKAGE=RNetica)
  ecount <- session$reportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  intToUtf8(olddelim)
}

CaseFileMissingCode <- function (newcode=NULL,session=getDefaultSession()) {
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
  oldcode <- .Call("RN_MissingCode",newcode,session,PACKAGE=RNetica)
  ecount <- session$reportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  intToUtf8(oldcode)
}

###############################################################
## Case Stream Objects

CaseStream <-
setRefClass("CaseStream",fields=c(Name="character",
                                  Session="NeticaSession",
                                  Netica_Case_Stream="externalptr",
                                  Case_Stream_Position="integer",
                                  Case_Stream_Lastid="integer",
                                  Case_Stream_Lastfreq="numeric"),
            contains="VIRTUAL",
            methods=list(
                initialize=function(Name=".Prototype",
                                    Session=NeticaSession(SessionName=".prototype"),
                                    ...) {
                  callSuper(Name=Name,Session=Session,
                            Netica_Case_Stream=externalptr(),
                            Case_Stream_Position=NA_integer_,
                            Case_Stream_Lastid=NA_integer_,
                            Case_Stream_Lastfreq=NA_real_,...)
                },
                isActive = function() {
                  .Call("RN_isCaseStreamActive",.self,PACKAGE=RNetica)
                },
                reportErrors = function(maxreport=9,clear=TRUE) {
                  Session$reportErrors(maxreport,clear)
                },
                clearErrors = function(severity="XXX_ERR") {
                  Session$clearErrors(severity)
                },
                isOpen = function() {
                  isActive()
                },
                close = function() {
                  if (!isOpen()) {
                    warning("CloseCaseStream:  Stream already closed.")
                    return (.self)
                  }
                  .Call("RN_CloseCaseStream",.self,PACKAGE=RNetica)
                },
                show=function() {
                  if (isOpen()) {
                    cat(" Stream is currently open.\n")
                    cat(" Stream is currently pointed at record ",
                        Case_Stream_Position, " ( ",Case_Stream_Lastid,
                        " ) x ",Case_Stream_Lastfreq, "\n")
                  } else {
                    cat(" Stream is currently closed.\n")
                  }
                }))

FileCaseStream <-
setRefClass("FileCaseStream",fields=c(Case_Stream_Path="character"),
            contains="CaseStream",
            methods=list(
                initialize=function(Name=basename(Case_Stream_Path),
                                    Session=NeticaSession(SessionName=".prototype"),
                                    Case_Stream_Path="/dev/null",
                                    ...) {
                  callSuper(Name=Name,Session=Session,
                            Case_Stream_Path=Case_Stream_Path,...)
                },
                open=function() {
                  if (isOpen()) {
                    warning("Stream is already open:  nothing done.")
                    return()
                  }
                  stream <-
                    .Call("RN_OpenCaseFileStream",Case_Stream_Path,
                          .self,Session,
                          PACKAGE=RNetica)
                  ecount <- Session$reportErrors()
                  if (ecount[1]>0) {
                    stop("Netica Errors Encountered, see console for details.")
                  }
                  stream
                },
                show=function() {
                  cat("Case File Stream ",Name,"\n")
                  cat("  Pathname = ",Case_Stream_Path,"\n")
                  callSuper()
                }))



## If the Memory stream is to be used as input to Netica, then it
## should be a data frame.  If Netica is going to write to it, then it should
## be NULL
setClassUnion("streamContents",c("data.frame","NULL"))


MemoryCaseStream <-
setRefClass("MemoryCaseStream",fields=c(Case_Stream_DataFrameName="character",
                                        Case_Stream_DataFrame="streamContents",
                                        Case_Stream_Buffer="externalptr"),
            contains="CaseStream",
            methods=list(
                initialize=function(Name=Case_Stream_DataFrameName,
                                    Session=NeticaSession(SessionName=".prototype"),
                                    Case_Stream_DataFrame=NULL,
                                    Case_Stream_DataFrameName=deparse(substitute(Case_Stream_DataFrame)),
                                    ...) {
                  callSuper(Name=Name,Session=Session,
                            Case_Stream_DataFrame=Case_Stream_DataFrame,
                            Case_Stream_DataFrameName=Case_Stream_DataFrameName,
                            Case_Stream_Buffer=externalptr(),
                            ...)
                },
                open=function() {
                  if (isOpen()) {
                    warning("Stream is already open:  nothing done.")
                    return()
                  }
                  stream <-
                    .Call("RN_OpenCaseMemoryStream",
                          Case_Stream_DataFrameName,
                          .self,Session,
                          PACKAGE=RNetica)
                  ecount <- Session$reportErrors()
                  if (ecount[1]>0) {
                    stop("Netica Errors Encountered, see console for details.")
                  }
                  stream
                },
                show=function() {
                  cat("Case Memory Stream ",Name,"\n")
                  cat("  Data Name = ",Case_Stream_DataFrameName,"\n")
                  cat("  Date:\n")
                  print(Case_Stream_DataFrame)
                  callSuper()
                }))


OpenCaseStream <- function (oldstream) {
  if (is.NeticaCaseStream(oldstream)) {
    oldstream$open()
  } else {
    stop("expected oldstram to be a CaseStream")
  }
  oldstream
}


CaseFileStream <- function (pathname,session=getDefaultSession()) {
  if (!is.character(pathname) || length(pathname)>1) {
    warning("OpenCaseStream:  expected single pathname as argument.")
  }
  stream <- FileCaseStream$new(Session=session,Case_Stream_Path=pathname)
  stream$open()
  stream
}

CaseMemoryStream <- function (data.frame,
                              label=deparse(substitute(data.frame)),
                              session=getDefaultSession()) {
  stream <-
    MemoryCaseStream(Session=session,
                     Case_Stream_DataFrame=data.frame,
                     Case_Stream_DataFrameName=label)
  stream$open()
  MemoryStreamContents(stream) <- data.frame
  stream
}


CloseCaseStream <- function (stream) {
  if (!is.NeticaCaseStream(stream))
    stop("Trying to close a non-stream object.")
  stream$close()
  stream
}

setMethod("toString","CaseStream", function (x, ...) {
  status <- ifelse(isCaseStreamOpen(x),"Open","Closed")
  src <- x$Name
  paste("<",status,class(x)[1],":",src,">")
})

setMethod("print","CaseStream",function(x, ...) {
  cat(toString(x),"\n")
})

setMethod("as.character", "CaseStream", function(x, ...) {
  toString(x)
})

setMethod("is.active","CaseStream",function(x) x$isActive())


is.NeticaCaseStream <- function (x) {
  is(x,"CaseStream")
}

is.MemoryCaseStream <- function (x) {
  is(x,"MemoryCaseStream")
}

is.CaseFileStream <- function (x) {
  is(x,"FileCaseStream")
}

isCaseStreamOpen <- function (stream) {
  if (!is.NeticaCaseStream(stream)) return (NA_integer_)
  stream$isOpen()
}

getCaseStreamPath <- function (stream) {
  stream$Case_Stream_Path
}

getCaseStreamLastId <- function (stream) {
  stream$Case_Stream_Lastid
}
getCaseStreamPos <- function (stream) {
  stream$Case_Stream_Position
}
getCaseStreamLastFreq <- function (stream) {
  stream$Case_Stream_Lastfreq
}

getCaseStreamDataFrameName <- function (stream) {
  stream$Case_Stream_DataFrameName
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
  if (length(id) >1L || is.na(id)) {
    stop("Argument id must be an integer scalar.")
  }
  freq <- as.numeric(freq)
  if (length(freq) >1L || is.na(freq)) {
    stop("Argument freq must be a numeric scalar.")
  }
  session <- NodeNet(nodes[[1]])$Session
  stream <- .Call("RN_WriteFindings",nodes,pathOrStream,id,freq,
                  session,PACKAGE=RNetica)
  ecount <- session$reportErrors()
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
    if (pos=="NEXT" && is.na(getCaseStreamPos(stream))) {
      stop("ReadFindings not yet called on stream with pos='FIRST'")
    }
  } else {
    pos <- as.integer(pos)
    if (is.na(pos) || length(pos)>1L || pos < 0) {
      stop("Argument pos must be a postive integer, 'FIRST' or 'NEXT'.")
    }
  }
  stream <- .Call("RN_ReadFindings",nodes,stream,pos,add,PACKAGE=RNetica)
  ecount <- nodes[[1]]$reportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  stream
}

read.CaseFile <- function(file,...,session=getDefaultSession()) {
  read.table(file,header=TRUE,sep=CaseFileDelimiter(session=session),
             na.strings=CaseFileMissingCode(session=session),...)
}

write.CaseFile <- function(x,file,...,session=getDefaultSession()) {
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
              sep=CaseFileDelimiter(session=session),
              na=CaseFileMissingCode(session=session))

}

MemoryStreamContents <- function (stream) {
  if (!is.MemoryCaseStream(stream)) {
    stop("MemoryStreamContents only worksfor MemoryCaseStreams.")
  }
  if (isCaseStreamOpen(stream)) {
    contents <- .Call("RN_GetMemoryStreamContents",stream,PACKAGE=RNetica)
    ecount <- stream$reportErrors()
    if (ecount[1]>0) {
      stop("Netica Errors Encountered, see console for details.")
    }
    if (is.null(contents)) {
      result <- NULL
    } else {
      con <- textConnection(contents)
      result <- read.CaseFile(con,session=stream$Session)
      close(con)
    }
    ## Cache value
    stream$Case_Stream_DataFrame <- result
    result
  } else {
    ## Use cached value
    stream$Case_Stream_DataFrame
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
  stream$Case_Stream_DataFrame <- value
  if (isCaseStreamOpen(stream)) {
    contents <- NULL
    if (!is.null(value)) {
      con <- textConnection(NULL,open="w")
      write.CaseFile(value,con,session=stream$Session)
      contents <- textConnectionValue(con)
      close(con)
    }
    stream <- .Call("RN_SetMemoryStreamContents",stream,contents,PACKAGE=RNetica)
    ecount <- stream$reportErrors()
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



