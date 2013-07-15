\name{NeticaCaseStream}
\alias{OpenCaseStream}
\alias{CloseCaseStream}
\alias{is.CaseStream}
\alias{toString.CaseStream}
\alias{isCaseStreamOpen}
\alias{getCaseStreamPath}
\alias{getCaseStreamLastId}
\alias{getCaseStreamLastFreq}
\alias{getCaseStreamDataFrameName}
\title{A stream of cases for reading/writing Netica findings}
\description{
  This opens up a stream object which can be used to either write out
  findings entered into a Netica network, or enter findings into a
  Network.  The function \code{\link{ReadFindings}} reads the findings
  from the stream and the function \code{\link{WriteFindings}} writes
  them out.
}
\usage{
OpenCaseStream(source)
CloseCaseStream(stream)
toString.CaseStream(x,...)
is.CaseStream(x)
isCaseStreamOpen(stream)
getCaseStreamPath(stream)
getCaseStreamPos(stream)
getCaseStreamLastId(stream)
getCaseStreamLastFreq(stream)
getCaseStreamDataFrameName(stream)
}
\arguments{
  \item{source}{The source of the stream.  This should either be a
    character scalar giving a pathname for a file stream, or a
    \code{data.frame} object providing the source for a memory stream
    (see details).  It also can be a \code{NeticaCaseStream} object in which
    case the stream is reopened if closed.
  }
  \item{stream}{A \code{NeticaCaseStream} object.}
  \item{x}{A object to be printed or whose type is to be determined.}
  \item{...}{Other arguments to \code{toString}.  These are ignored.}
}
\details{

  A \code{NeticaCaseStream} object is an R wrapper around a Netica stream
  object.  These are case files, basically a tab (or comma, see
  \code{\link{CaseFileDelimiter}}) separated value file where columns
  represent variables and rows represent cases.  Although the function
  \code{\link{WriteFindings}} always appends a new case to the end of a 
  file (and hence does not need to keep the stream object open between
  calls), the function \code{\link{ReadFindings}} will read (by default)
  sequentially from the cases in the stream, and hence the stream needs
  to be kept open between calls.

  The function \code{OpenCaseStream} will open a stream in Netica and
  create a new \code{NeticaCaseStream}.  If the argument \code{source} is a
  character, it is assumed to be the pathname of file on the file
  system.  (This function only looks at the first element of
  \code{source}).  This file should be a file previously written by
  \code{\link{WriteFindings}} or be in the same format.  The delimiter
  used should be the one given by \code{\link{CaseFileDelimiter}}, and
  the code used for missing values should be the value of
  \code{\link{CaseFileMissingCode}}.

  The second option is for \code{source} to be a data frame whose rows
  represent cases, and whose columns represent variables.  In this case
  a memory stream is opened in Netica.  This option is not yet
  implemented.

  If \code{source} is a \code{NeticaCaseStream} object, then the stream is
  reopened (if it was previously closed) and the \code{stream} object is
  returned (a new stream object is not created in this case).

  The function \code{CloseCaseStream} closes an open case stream (and is
  harmless if the stream is already closed.  Although RNetica tries to
  close open case streams when they are garbage collected, users should
  not count on this behavior and should close them manually.  Also be
  aware that all case streams are automatically closed when R is closes
  or RNetica is unloaded.  The function \code{isCaseStreamOpen} tests to
  see if the stream is open or closed.

  Netica internally keeps track of the current position of the stream
  when it is read or written.  The functions \code{getCaseStreamPos},
  \code{GetCaseStreamLastId} and \code{getCaseStreamLastFreq} get
  information about the position in the file, the user generated id
  number and the frequency/weight assigned to the case at the time the
  stream was last read or written.  In particular, the function
  \code{\link{ReadFindings}} returns a \code{NeticaCaseStream} object, which
  should be queried to find the ID and Frequencies read from the stream.
  
  The functions \code{getCaseStreamPath} (meaningful for file streams),
  and \code{getCaseStreamDataFrame} and
  \code{getCaseStreamDataFrameName} (meaningful for memory streams)
  provide information about the source of the stream.

}
\value{

  The functions \code{OpenCaseStream} and \code{CloseCaseStream} both
  return an object of class \code{NeticaCaseStream}.  In the former case, if
  \code{source} was a \code{NeticaCaseStream} it will be returned, otherwise a
  new \code{NeticaCaseStream} object will be created.  The function
  \code{CloseCaseStream} always returns its argument.

  The function \code{toString.CaseStream} returns a string providing
  information about the source and status its argument.

  The functions \code{is.CaseStream} and \code{isCaseStreamOpen} both
  return logical values indicating whether or not the condition holds.
  The latter function returns \code{NA} if its argument is not a
  \code{NeticaCaseStream}.

  The function \code{getCaseStreamPath} returns a string giving the path
  of the file associated with \code{stream} or \code{NULL} if
  \code{stream} is a memory stream made from a data frame.

  The function \code{getCaseStreamDataFrameName} returns the expression
  which was used to provide the data frame if \code{stream} is a memory
  stream.  Otherwise, it returns \code{NULL}.

  The functions \code{getCaseStreamPos} and \code{getCaseStreamLastId}
  both return scalar integer values giving the position or ID number of
  the last record read from or written to the stream.  The position is
  an integer corresponding to the number of characters that have been
  read in the stream.  The ID is a user specified integer associated
  with the row, and is \code{-1} if the user did not assign ID numbers.

  The function \code{getCaseStreamLastFreq} returns a numeric scalar
  which is the weight associated with the last case read from or written
  to \code{stream}.  If the user did not specify frequencies when the
  stream was written, the value returned is \code{-1}.
}
\references{
  \newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{NewFileStream_ns},\nref{NewMemoryStream_ns},
  \nref{DeleteStream_ns}, \nref{SetStreamContents_ns},
  \url{http://homepage.stat.uiowa.edu/~luke/R/references/weakfinex.html}
}
\author{Russell Almond}
\note{

  Memory Streams are not yet implemented.

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
  with it.

  Stream objects are fragile, and will not survive saving and restoring
  an R session.  However, the object retains information about itself,
  so that calling \code{OpenCaseStream} on the saved object, should
  reopen the stream.  Note that any position information will be lost.
  
}
\seealso{
  \code{\link{CaseFileDelimiter}}, \code{\link{CaseFileMissingCode}},
  \code{\link{WriteFindings}}, \code{\link{ReadFindings}},
  \code{\link{CaseStreamDataFrame}}
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

filestream <- OpenCaseStream(casefile)
stopifnot(is.CaseStream(filestream),
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
stopifnot (is.CaseStream(filestream),
           !isCaseStreamOpen(filestream))

## Reopen it
filestream <- OpenCaseStream(filestream)
stopifnot (is.CaseStream(filestream),
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
DeleteNetwork(abc)

}
\keyword{ interface }
\keyword{ IO }% __ONLY ONE__ keyword per line
