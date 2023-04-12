\name{CaseStream-class}
\Rdversion{1.1}
\docType{class}
\alias{CaseStream-class}
\alias{print,CaseStream-method}
\alias{toString,CaseStream-method}
\alias{as.character,CaseStream-method}

\title{Class \code{"CaseStream"}}
\description{

  This object is a wrapper around a Netica stream which is used to
  read/write cases---sets of findings entered into a Netica network.
  There are two subclasses:  \code{\linkS4class{FileCaseStream}} and
  \code{\linkS4class{MemoryCaseStream}}.  The function
  \code{\link{ReadFindings}} reads the findings from the stream and the
  function \code{\link{WriteFindings}} writes them out.

}
\section{Extends}{

All reference classes extend and inherit methods from
\code{"\linkS4class{envRefClass}"}. Note that because this is a reference
class unlike traditional S3 and S4 classes it can be destructively
modified.  Also fields (slots) are accessed using the \sQuote{$}
operator.
}
\details{

  A \code{CaseStream} object is an R wrapper around a Netica stream
  object.  There are two subclasses:
  \code{\linkS4class{FileCaseStream}} objects are streams focused on a
  case file, and \code{\linkS4class{MemoryCaseStream}} objects are
  streams focused on a hunk of memory corresponding to an R data frame
  object.

  Although the function \code{\link{WriteFindings}} always appends a new
  case to the end of a file (and hence does not need to keep the stream
  object open between calls), the function \code{\link{ReadFindings}}
  will read (by default) sequentially from the cases in the stream, and
  hence the stream needs to be kept open between calls.

  The functions \code{\link{CaseFileStream}} and
  \code{\link{CaseMemoryStream}} create new streams and open them.
  The function \code{\link{OpenCaseStream}} will reopen a previously closed
  stream, and will issue a warning if the stream is already open.
  The function \code{\link{CloseCaseStream}} closes an open case stream (and is
  harmless if the stream is already closed).  Although RNetica tries to
  close open case streams when they are garbage collected, users should
  not count on this behavior and should close them manually.  Also be
  aware that all case streams are automatically closed when R is closes
  or RNetica is unloaded.  The function \code{\link{isCaseStreamOpen}}
  tests to see if the stream is open or closed. The function
  \code{\link{WithOpenCaseStream}} executes an arbitrary R expression in
  a context where the stream is open, and then closed afterwards.

  Netica internally keeps track of the current position of the stream
  when it is read or written.  The functions \code{\link{getCaseStreamPos}},
  \code{\link{getCaseStreamLastId}} and
  \code{\link{getCaseStreamLastFreq}} get 
  information about the position in the file, the user generated id
  number and the frequency/weight assigned to the case at the time the
  stream was last read or written.  In particular, the function
  \code{\link{ReadFindings}} returns a \code{CaseStream} object, which
  should be queried to find the ID and Frequencies read from the
  stream.  When \code{\link{ReadFindings}} reaches the end of the
  stream, the value of \code{\link{getCaseStreamPos}(\var{stream})} will
  be \code{NA}.  
  
}
\references{
  \newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{NewFileStream_ns},\nref{NewMemoryStream_ns},
  \nref{DeleteStream_ns} 
  \url{http://homepage.stat.uiowa.edu/~luke/R/references/weakfinex.html}
}
\author{Russell Almond}
\note{

  The functions \code{\link{ReadNetworks}} and
  \code{\link{WriteNetworks}} also use Netica streams
  internally. However, as it is almost certainly a mistake to keep the
  stream open after the network has been read or written, no
  \code{NeticaCaseStream} object is created.
  
  Internally, a weak reference system is used to keep a list of Netica
  stream objects which need to be closed when RNetica is unloaded.
  Stream objects should also be forced closed when garbage collected.
  The weak reference system is somewhat experimental, so well designed
  code should manually close the streams when the program is through
  with them.

  Stream objects are fragile, and will not survive saving and restoring
  an R session.  However, the object retains information about itself,
  so that calling \code{OpenCaseStream} on the saved object, should
  reopen the stream.  Note that any position information will be lost.
  
}


\seealso{

  See \code{\linkS4class{FileCaseStream}} and
  \code{\linkS4class{MemoryCaseStream}} for specific details about the
  two subtypes.   \code{\link{CaseMemoryStream}} and
  \code{\link{CaseFileStream}} are the two constructors.


  \code{\link{OpenCaseStream}},
  \code{\link{CaseFileDelimiter}}, \code{\link{CaseFileMissingCode}},
  \code{\link{WriteFindings}}, \code{\link{ReadFindings}},
  \code{\link{WithOpenCaseStream}}
}
\examples{

sess <- NeticaSession()
startSession(sess)

abc <- CreateNetwork("ABC",sess)
A <- NewDiscreteNode(abc,"A",c("A1","A2","A3","A4"))
B <- NewDiscreteNode(abc,"B",c("B1","B2","B3"))
C <- NewDiscreteNode(abc,"C",c("C1","C2"))

AddLink(A,B)
AddLink(A,C)
AddLink(B,C)

## Outputfilename
casefile <- tempfile("testcase",fileext=".cas")

filestream <- CaseFileStream(casefile,sess)
stopifnot(is.NeticaCaseStream(filestream),
          isCaseStreamOpen(filestream))

## Case 1
NodeFinding(A) <- "A1"
NodeFinding(B) <- "B1"
NodeFinding(C) <- "C1"
filestream <- WriteFindings(list(A,B,C),filestream,1001,1.0)
stopifnot(getCaseStreamLastId(filestream)==1001,
          abs(getCaseStreamLastFreq(filestream)-1.0) <.0001)
pos1 <- getCaseStreamPos(filestream)
RetractNetFindings(abc)

## Case 2
NodeFinding(A) <- "A2"
NodeFinding(B) <- "B2"
NodeFinding(C) <- "C2"
## Double weight this case
filestream <- WriteFindings(list(A,B,C),filestream,1002,2.0)
pos2 <- getCaseStreamPos(filestream)
stopifnot(pos2>pos1,getCaseStreamLastId(filestream)==1002,
          abs(getCaseStreamLastFreq(filestream)-2.0) <.0001)
RetractNetFindings(abc)

## Case 3
NodeFinding(A) <- "A3"
NodeFinding(B) <- "B3"
## C will be missing
filestream <- WriteFindings(list(A,B,C),filestream,1003,1.0)
stopifnot(getCaseStreamLastId(filestream)==1003,
          abs(getCaseStreamLastFreq(filestream)-1.0) <.0001)
RetractNetFindings(abc)

## Close it
filestream <- CloseCaseStream(filestream)
stopifnot (is.NeticaCaseStream(filestream),
           !isCaseStreamOpen(filestream))

## Reopen it
filestream <- OpenCaseStream(filestream)
stopifnot (is.NeticaCaseStream(filestream),
           isCaseStreamOpen(filestream))

##Case 1
RetractNetFindings(abc)
filestream <- ReadFindings(list(A,B,C),filestream,"FIRST")
pos1a <- getCaseStreamPos(filestream)
stopifnot(pos1a==pos1,
          getCaseStreamLastId(filestream)==1001,
          abs(getCaseStreamLastFreq(filestream)-1.0) <.0001)

##Case 2
RetractNetFindings(abc)
filestream <- ReadFindings(list(A,B,C),filestream,"NEXT")
stopifnot(getCaseStreamPos(filestream)==pos2,
          getCaseStreamLastId(filestream)==1002,
          abs(getCaseStreamLastFreq(filestream)-2.0) <.0001)


##Clean Up
CloseCaseStream(filestream)
CloseCaseStream(filestream) ## This should issue a warning but be
## harmless. 
DeleteNetwork(abc)
stopSession(sess)

}
\keyword{ interface }
\keyword{ IO }
\keyword{classes}
\section{Fields}{
  Note these should be regarded as read-only from user code.

  \describe{
    \item{\code{Name}:}{Object of class \code{character} an identifier
      for the case stream, derived from the filename for
      \code{\linkS4class{FileCaseStream}} objects, and from the name of
      the R object for \code{\linkS4class{MemoryCaseStream}} }
    \item{\code{Session}:}{Object of class \code{NeticaSession}:: a back
      pointer to the \code{\linkS4class{NeticaSession}} object in which
      the stream was created. }
    \item{\code{Netica_Case_Stream}:}{Object of class \code{externalptr}
      a link to the stream in internal Netica memory. }
    \item{\code{Case_Stream_Position}:}{Object of class \code{integer}
      the number of the last read/writen record. This is \code{NA} if
      the end of the file has been reached. }
    \item{\code{Case_Stream_Lastid}:}{Object of class \code{integer} the
    ID number of the last read/written record.}
    \item{\code{Case_Stream_Lastfreq}:}{Object of class \code{numeric}
      giving the frequence of the last read/written record.  This is
      used as a weight in learning applications. }
  }
}
\section{Methods}{
  \describe{
    \item{\code{show()}:}{ Provides a printed record. }
    \item{\code{close()}:}{ Closes the stream.  Equivalent to
      \code{\link{CloseCaseStream}(\var{stream})}. }
    \item{\code{isOpen()}:}{ Checks to see if the stream is currently
      open.  Equivalent to \code{\link{isCaseStreamOpen}(\var{stream})}. }
    \item{\code{isActive()}:}{ Equivalent to \code{isOpen()}, name is
      symmetric with other Netica reference objects. }
    \item{\code{clearErrors(severity)}:}{ Calls \code{clearErrors} on
      the \code{Session} object. }
    \item{\code{reportErrors(maxreport, clear, call)}:}{ Calls
      \code{reportErrors} on the \code{Session} object. Returns an
      object of class \code{\link{NeticaCondition}} if there was a
      message, or \code{NULL} if not.}
    \item{\code{signalErrors(maxreport, clear, call)}:}{ Calls
      \code{signalErrors} on the \code{Session} object.  If there was a
      problem, the appropriate condition is signaled, see
    \code{\link{NeticaCondition}}.  }
    \item{\code{initialize(Name, Session, ...)}:}{ Internal initializer.
     User code should not call.}
  }
}
