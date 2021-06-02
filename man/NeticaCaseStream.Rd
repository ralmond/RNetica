\name{NeticaCaseStream}
\alias{OpenCaseStream}
\alias{CloseCaseStream}
\alias{is.NeticaCaseStream}
\alias{isCaseStreamOpen}
\alias{getCaseStreamPos}
\alias{getCaseStreamLastId}
\alias{getCaseStreamLastFreq}
\title{Functions for manipulating Netica case streams}
\description{

  The \code{\linkS4class{CaseStream}} object is a wrapper around a
  Netica stream which is used to read/write cases---sets of findings
  entered into a Netica network.  There are two subclasses:
  \code{\linkS4class{FileCaseStream}} and
  \code{\linkS4class{MemoryCaseStream}}.  The function
  \code{\link{ReadFindings}} reads the findings from the stream and the
  function \code{\link{WriteFindings}} writes them out.

}
\usage{
OpenCaseStream(oldstream)
CloseCaseStream(stream)
is.NeticaCaseStream(x)
isCaseStreamOpen(stream)
getCaseStreamPos(stream)
getCaseStreamLastId(stream)
getCaseStreamLastFreq(stream)
}
\arguments{
  \item{oldstream}{A previously closed \code{\linkS4class{CaseStream}} object.
  }
  \item{stream}{A \code{\linkS4class{CaseStream}} object.}
  \item{x}{A object to be printed or whose type is to be determined.}
}
\details{

  A \code{\linkS4class{CaseStream}} object is an R wrapper around
  a Netica stream object.  There are two special cases:
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
  \code{getCaseStreamLastId} and \code{getCaseStreamLastFreq} get
  information about the position in the file, the user generated id
  number and the frequency/weight assigned to the case at the time the
  stream was last read or written.  In particular, the function
  \code{\link{ReadFindings}} returns a \code{\linkS4class{CaseStream}}
  object, which should be queried to find the ID and Frequencies read
  from the stream.  When \code{\link{ReadFindings}} reaches the end of
  the stream, the value of \code{getCaseStreamPos(\var{stream})} will be
  \code{NA}.
  
}
\value{

  The functions \code{OpenCaseStream} and \code{CloseCaseStream} both
  return their argument, which should be a \code{\linkS4class{CaseStream}}.

  The function \code{toString.CaseStream} returns a string providing
  information about the source and status its argument.

  The functions \code{is.NeticaCaseStream} and \code{isCaseStreamOpen} both
  return logical values indicating whether or not the condition holds.
  The latter function returns \code{NA} if its argument is not a
  \code{\linkS4class{CaseStream}}.

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

  The functions \code{\link{LearnCPTs}} and \code{\link{LearnCases}}
  update the CPTs of a Bayesian network based on the cases in the case
  stream. 
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
  \code{\linkS4class{CaseStream}} object is created.
  
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

  The functions \code{\link{LearnCPTs}} and \code{\link{LearnCases}}
  don't seem to work with \code{\linkS4class{MemoryCaseStream}}s; for
  now, work around by writing the data out to a file and then writing
  using a \code{\linkS4class{FileCaseStream}}.
  
}
\seealso{
  See \code{\linkS4class{CaseStream}} for details about the stream
  object. See \code{\linkS4class{FileCaseStream}} and
  \code{\linkS4class{MemoryCaseStream}} for specific details about these
  stream types. 


  \code{\link{CaseFileDelimiter}}, \code{\link{CaseFileMissingCode}},
  \code{\link{WriteFindings}}, \code{\link{ReadFindings}},
  \code{\link{CaseMemoryStream}},\code{\link{CaseFileStream}},
  \code{\link{WithOpenCaseStream}}
  \code{\link{LearnCPTs}},   \code{\link{LearnCases}}


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
