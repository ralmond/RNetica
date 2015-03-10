\name{NeticaCaseStream}
\alias{NeticaCaseStream}
\alias{OpenCaseStream}
\alias{CloseCaseStream}
\alias{is.NeticaCaseStream}
\alias{toString.NeticaCaseStream}
\alias{print.NeticaCaseStream}
\alias{isCaseStreamOpen}
\alias{getCaseStreamPos}
\alias{getCaseStreamLastId}
\alias{getCaseStreamLastFreq}
\title{A stream of cases for reading/writing Netica findings}
\description{

  This object is a wrapper around a Netica stream which is used to
  read/write cases---sets of findings entered into a Netica network.
  There are two subclasses:  \code{\link{CaseFileStream}} and
  \code{\link{MemoryCaseStream}}.  The function
  \code{\link{ReadFindings}} reads the findings from the stream and the
  function \code{\link{WriteFindings}} writes them out.

}
\usage{
OpenCaseStream(oldstream)
CloseCaseStream(stream)
\method{toString}{NeticaCaseStream}(x,...)
\method{print}{NeticaCaseStream}(x,...)
is.NeticaCaseStream(x)
isCaseStreamOpen(stream)
getCaseStreamPos(stream)
getCaseStreamLastId(stream)
getCaseStreamLastFreq(stream)
}
\arguments{
  \item{oldstream}{A previously closed \code{NeticaCaseStream} object.
  }
  \item{stream}{A \code{NeticaCaseStream} object.}
  \item{x}{A object to be printed or whose type is to be determined.}
  \item{...}{Other arguments to \code{toString}.  These are ignored.}
}
\details{

  A \code{NeticaCaseStream} object is an R wrapper around a Netica stream
  object.  There are two special cases: \code{\link{CaseFileStream}}
  objects are streams focused on a case file, and
  \code{\link{MemoryCaseStream}} objects are streams focused on a hunk of
  memory corresponding to an R data frame object.

  Although the function \code{\link{WriteFindings}} always appends a new
  case to the end of a file (and hence does not need to keep the stream
  object open between calls), the function \code{\link{ReadFindings}}
  will read (by default) sequentially from the cases in the stream, and
  hence the stream needs to be kept open between calls.

  The functions \code{\link{CaseFileStream}} and
  \code{\link{MemoryCaseStream}} create new streams and open them.
  The function \code{OpenCaseStream} will reopen a previously closed
  stream, and will issue a warning if the stream is already open.
  The function \code{CloseCaseStream} closes an open case stream (and is
  harmless if the stream is already closed).  Although RNetica tries to
  close open case streams when they are garbage collected, users should
  not count on this behavior and should close them manually.  Also be
  aware that all case streams are automatically closed when R is closes
  or RNetica is unloaded.  The function \code{isCaseStreamOpen} tests to
  see if the stream is open or closed. The function
  \code{\link{WithOpenCaseStream}} executes an arbitrary R expression in
  a context where the stream is open, and then closed afterwards.

  Netica internally keeps track of the current position of the stream
  when it is read or written.  The functions \code{getCaseStreamPos},
  \code{GetCaseStreamLastId} and \code{getCaseStreamLastFreq} get
  information about the position in the file, the user generated id
  number and the frequency/weight assigned to the case at the time the
  stream was last read or written.  In particular, the function
  \code{\link{ReadFindings}} returns a \code{NeticaCaseStream} object, which
  should be queried to find the ID and Frequencies read from the
  stream.  When \code{\link{ReadFindings}} reaches the end of the
  stream, the value of \code{getCaseStreamPos(stream)} will be
  \code{NA}.  
  
  See \code{\link{CaseFileStream}} and \code{\link{MemoryCaseStream}}
  for specific details about these screen types.

}
\value{

  The functions \code{OpenCaseStream} and \code{CloseCaseStream} both
  return their argument, which should be a \code{NeticaCaseStream}.

  The function \code{toString.CaseStream} returns a string providing
  information about the source and status its argument.

  The functions \code{is.NeticaCaseStream} and \code{isCaseStreamOpen} both
  return logical values indicating whether or not the condition holds.
  The latter function returns \code{NA} if its argument is not a
  \code{NeticaCaseStream}.

  The function \code{getCaseStreamPos} returns a scalar integer values
  giving the position of the last record read from or written to the
  stream.  The position is an integer corresponding to the number of
  characters that have been read in the stream.  If an attempt has been
  made to read past the end of the stream, this value will be \code{NA}.

  The function \code{getCaseStreamLastId} is a user specified integer
  associated with the case last read from or written to the stream.
  It's value is \code{-1} if the user did not assign ID numbers.

  The function \code{getCaseStreamLastFreq} returns a numeric scalar
  which is the weight associated with the last case read from or written
  to \code{stream}.  If the user did not specify frequencies when the
  stream was written, the value returned is \code{-1}.
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
  \code{\link{CaseFileDelimiter}}, \code{\link{CaseFileMissingCode}},
  \code{\link{WriteFindings}}, \code{\link{ReadFindings}},
  \code{\link{MemoryCaseStream}},\code{\link{CaseFileStream}},
  \code{\link{WithOpenCaseStream}}
}
\examples{

abc <- CreateNetwork("ABC")
A <- NewDiscreteNode(abc,"A",c("A1","A2","A3","A4"))
B <- NewDiscreteNode(abc,"B",c("B1","B2","B3"))
C <- NewDiscreteNode(abc,"C",c("C1","C2"))

AddLink(A,B)
AddLink(A,C)
AddLink(B,C)

## Outputfilename
casefile <- tempfile("testcase",fileext=".cas")

filestream <- CaseFileStream(casefile)
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

}
\keyword{ interface }
\keyword{ IO }% __ONLY ONE__ keyword per line
