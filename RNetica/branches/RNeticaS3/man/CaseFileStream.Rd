\name{CaseFileStream}
\alias{CaseFileStream}
\alias{is.CaseFileStream}
\alias{getCaseStreamPath}
\title{A stream of cases for reading/writing Netica findings to a file}
\description{

  This object is subclass of \code{\link{NeticaCaseStream}} so it is a
  wrapper around a Netica stream which is used to read/write cases.  In
  this subclass, the case stream is associated with a Netica case file
  ('.cas' extension).  The function \code{\link{ReadFindings}} reads the
  findings from the stream and the function \code{\link{WriteFindings}}
  writes them out.

}
\usage{
CaseFileStream(pathname)
is.CaseFileStream(x)
getCaseStreamPath(stream)
}
\arguments{
  \item{pathname}{A character scalar giving a path to the case file.
    Netica expects case files to end with the extension ".cas"
  }
  \item{stream}{A \code{CaseFileStream} object.}
  \item{x}{A object to be printed or whose type is to be determined.}
}
\details{

  A \code{CaseFileStream} object is a subclass of the
  \code{\link{NeticaCaseStream}} object, which is an R wrapper around a
  Netica stream object, in this case one that reads or writes to a case
  file.  Case files are tab (or comma, see
  \code{\link{CaseFileDelimiter}}) separated value files where columns 
  represent variables and rows represent cases.  Although the function
  \code{\link{WriteFindings}} always appends a new case to the end of a 
  file (and hence does not need to keep the stream object open between
  calls), the function \code{\link{ReadFindings}} will read (by default)
  sequentially from the cases in the stream, and hence the stream needs
  to be kept open between calls.

  The function \code{CaseFileStream} will open a stream in Netica and
  create a new \code{CaseFileStream} if necessary.  The argument
  \code{pathname} should be the pathname of the case file in the file
  system.  This file should be a file previously written by
  \code{\link{WriteFindings}} or be in the same format.  The delimiter
  used should be the one given by \code{\link{CaseFileDelimiter}}, and
  the code used for missing values should be the value of
  \code{\link{CaseFileMissingCode}}.

  The function \code{\link{CloseCaseStream}} closes an open case stream
  (and is harmless if the stream is already closed).  Although RNetica
  tries to close open case streams when they are garbage collected,
  users should not count on this behavior and should close them
  manually.  Also be aware that all case streams are automatically
  closed when R is closed or RNetica is unloaded.  The function
  \code{\link{isCaseStreamOpen}} tests to see if the stream is open or
  closed, and the function \code{\link{OpenCaseStream}} reopens a
  previously closed case stream.

  The functions \code{getCaseStreamPath} returns the path on which the
  \code{CaseFileStream} is focused.

  For other properties of \code{CaseFileStream}, see
  \code{\link{NeticaCaseStream}}. 

}
\value{

  The function \code{CaseFileStream} returns a new, open
  \code{CaseFileStream} object.

  The functions \code{is.CaseFileStream} returns a logical value
  indicating whether or not the argument is a \code{CaseFileStream}.

  The function \code{getCaseStreamPath} returns a string giving the path
  of the file associated with \code{stream}, or \code{NULL} if the
  argument is not a \code{CaseFileStream}.

}
\references{
  \newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{NewFileStream_ns},
  \url{http://homepage.stat.uiowa.edu/~luke/R/references/weakfinex.html}
}
\author{Russell Almond}
\note{

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
  \code{\link{NeticaCaseStream}}, \code{\link{MemoryCaseStream}},
  \code{\link{WriteFindings}}, \code{\link{ReadFindings}},
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
stopifnot(is.CaseFileStream(filestream),
          isCaseStreamOpen(filestream))

## Case 1
NodeFinding(A) <- "A1"
NodeFinding(B) <- "B1"
NodeFinding(C) <- "C1"
filestream <- WriteFindings(list(A,B,C),filestream,1001,1.0)
stopifnot(getCaseStreamLastId(filestream)==1001,
          abs(getCaseStreamLastFreq(filestream)-1.0) <.0001)

## Close it
filestream <- CloseCaseStream(filestream)
stopifnot (is.CaseFileStream(filestream),
           !isCaseStreamOpen(filestream))

## Reopen it
filestream <- OpenCaseStream(filestream)
stopifnot (is.CaseFileStream(filestream),
           isCaseStreamOpen(filestream))

##Case 1
RetractNetFindings(abc)
filestream <- ReadFindings(list(A,B,C),filestream,"FIRST")
stopifnot(getCaseStreamLastId(filestream)==1001,
          abs(getCaseStreamLastFreq(filestream)-1.0) <.0001)

##Clean Up
CloseCaseStream(filestream)
DeleteNetwork(abc)

}
\keyword{ interface }
\keyword{ IO }% __ONLY ONE__ keyword per line
