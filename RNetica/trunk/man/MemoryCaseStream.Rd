\name{MemoryCaseStream}
\alias{MemoryCaseStream}
\alias{is.MemoryCaseStream}
\alias{getCaseStreamDataFrameName}
\title{A stream of cases for reading/writing Netica from memory}
\description{

  This object is subclass of \code{\link{NeticaCaseStream}} so it is a
  wrapper around a Netica stream which is used to read/write cases.  In
  this subclass, the case stream is associated with a memory buffer that
  corresponds to an R \code{\link[base]{data.frame}} object.
  The function \code{\link{MemoryStreamContents}} accesses the contents
  as a data frame.

}
\usage{
MemoryCaseStream(data.frame,label=deparse(substitute(data.frame)))
is.MemoryCaseStream(x)
getCaseStreamDataFrameName(stream)
}
\arguments{
  \item{data.frame}{A data frame in which columns correspond to Netica
    nodes, and rows correspond to cases.  See details.
  }
  \item{label}{A name for the stream object.}
  \item{stream}{A \code{NeticaCaseStream} object.}
  \item{x}{A object whose type is to be determined.}
}
\details{

  A Netica case file has a format that very much resembles the output of
  \code{\link[utils]{write.table}}.  The first row is a header row, which
  contains the names of the variables, the second and subsequent rows
  contain a set of findings:  an assignment of values to the nodes
  indicated in the columns.  There are no row numbers, and the separator
  and missing value codes are the values of
  \code{\link{CaseFileDelimiter}()}, and
  \code{\link{CaseFileMissingCode}()} respectively.

  In addition to columns representing variables, two special columns are
  allowed.  The column named \dQuote{IDnum}, if present should contain
  integers which correspond to ID numbers for the cases (this correspond
  to the \code{id} argument of \code{\link{WriteFindings}}).  The column
  named \dQuote{NumCases} should contain number values and this allows
  rows to be differentially weighted (this correspond to the \code{freq}
  argument of \code{\link{WriteFindings}}).

  A simple way to convert a data frame into a set of cases for use with
  various Netica functions that use cases would be to write the data
  frame to a file of the proper format, and then create a
  \code{\link{CaseFileStream}} on the just written file.  The
  \code{MemoryCaseStream} shortcuts that process by writing the data
  frame to a memory buffer and then creating a stream around the memory
  buffer. Like the \code{CaseFileStream}, the \code{MemoryCaseStream} is
  a subclass of \code{\link{NeticaCaseStream}} and follows the same
  conventions.  

  The function \code{MemoryCaseStream} opens a new memory stream using
  \code{data.frame} as the source.  If \code{data.frame} is \code{NULL}
  a new memory stream for writing is created.  The function
  \code{CloseCaseStream} closes an open case stream (and is harmless if
  the stream is already closed.  Although RNetica tries to close open
  case streams when they are garbage collected, users should not count
  on this behavior and should close them manually.  Also be aware that
  all case streams are automatically closed when R is closes or RNetica
  is unloaded.  The function \code{isCaseStreamOpen} tests to see if the
  stream is open or closed.  The function \code{OpenCaseStream} if
  called on a closed \code{MemoryCaseStream} will reopen the stream in Netica
  using the current value of \code{\link{MemoryStreamContents}} as the
  source.  (If called on an open stream it will do nothing but issue a
  warning).  

  The function \code{getCaseStreamDataFrameName} provides the value of
  \code{label} when the stream was created.


}
\value{

  The function \code{OpenMemoryCaseStream} returns a new, open
  \code{CaseFileStream} object.

  The functions \code{is.MemoryCaseStream} returns a logical value
  indicating whether or not the argument is a \code{CaseFileStream}.

  The function \code{getCaseStreamDataFrameName} returns the value of
  \code{label} used when the stream was created, usually this is the
  name of the \code{data.frame} argument.

}
\references{
  \newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{NewMemoryStream_ns},
  \url{http://homepage.stat.uiowa.edu/~luke/R/references/weakfinex.html}
}
\author{Russell Almond}
\note{

  \code{MemoryCaseStreams} are most useful for small to medium size data
  frames.  Larger data frames are probably better handled through case
  files.
  
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
\section{Netica Bugs}{
  In version 5.04 of the Netica API, there is a problem with using
  Memory Streams that seems to affect the functions
  \code{\link{LearnCases}} and \code{\link{LearnCPTs}}.  Until this
  problem is fixed, most uses of Memory Streams will require file
  streams instead.  Write the case file using
  \code{\link{write.CaseFile}}, and then create a file stream using
  \code{\link{CaseFileStream}}. 
}
\seealso{
  \code{\link{CaseFileDelimiter}}, \code{\link{CaseFileMissingCode}},
  \code{\link{WriteFindings}}, \code{\link{ReadFindings}},
  \code{\link{MemoryStreamContents}},\code{\link{NeticaCaseStream}}
}
\examples{

abc <- CreateNetwork("ABC")
A <- NewDiscreteNode(abc,"A",c("A1","A2","A3","A4"))
B <- NewDiscreteNode(abc,"B",c("B1","B2","B3"))
C <- NewDiscreteNode(abc,"C",c("C1","C2"))

AddLink(A,B)
AddLink(A,C)
AddLink(B,C)

## This is the file written in CaseFileStream help.
casefile <- paste(library(help="RNetica")$path,
                           "testData","abctestcases.cas",
                           sep=.Platform$file.sep)
CaseFileDelimiter("\t")
CaseFileMissingCode("*")
cases <- read.CaseFile(casefile)

memstream <- MemoryCaseStream(cases)


##Case 1
memstream <- ReadFindings(list(A,B,C),memstream,"FIRST")
stopifnot(NodeFinding(A) == "A1",
          NodeFinding(B) == "B1",
          NodeFinding(C) == "C1",
          getCaseStreamLastId(memstream)==1001,
          abs(getCaseStreamLastFreq(memstream)-1.0) <.0001)

##Case 2
memstream <- ReadFindings(list(A,B,C),memstream,"NEXT")
stopifnot(NodeFinding(A) == "A2",
          NodeFinding(B) == "B2",
          NodeFinding(C) == "C2",
          getCaseStreamLastId(memstream)==1002,
          abs(getCaseStreamLastFreq(memstream)-2.0) <.0001)

##Case 3
memstream <- ReadFindings(list(A,B,C),memstream,"NEXT")
stopifnot(NodeFinding(A) == "A3",
          NodeFinding(B) == "B3",
          NodeFinding(C) == "@NO FINDING",
          getCaseStreamLastId(memstream)==1003,
          abs(getCaseStreamLastFreq(memstream)-1.0) <.0001)

## EOF
memstream <- ReadFindings(list(A,B,C),memstream,"NEXT")
stopifnot (is.na(getCaseStreamPos(memstream)))


##Clean Up
CloseCaseStream(memstream)
DeleteNetwork(abc)

}
\keyword{ interface }
\keyword{ IO }% __ONLY ONE__ keyword per line
