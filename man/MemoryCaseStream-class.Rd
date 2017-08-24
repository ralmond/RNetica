\name{MemoryCaseStream-class}
\Rdversion{1.1}
\docType{class}
\alias{MemoryCaseStream-class}

\title{Class \code{"MemoryCaseStream"}}
\description{

  This object is subclass of \code{\linkS4class{CaseStream}} so it is a
  wrapper around a Netica stream which is used to read/write cases. In
  this subclass, the case stream is associated with a data frame
  containing the dase file information.  The function
  \code{\link{CaseMemoryStream}} is the constructor.  the case stream is
  associated with a memory buffer that corresponds to an R
  \code{\link[base]{data.frame}} object.  The function
  \code{\link{MemoryStreamContents}} accesses the contents as a data
  frame.

}
\section{Extends}{
Class \code{"\linkS4class{CaseStream}"}, directly.

All reference classes extend and inherit methods from
\code{"\linkS4class{envRefClass}"}. Note that because this is a reference
class unlike traditional S3 and S4 classes it can be destructively
modified.  Also fields (slots) are accessed using the \sQuote{$}
operator.

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
  buffer. Like the \code{\linkS4class{FileCaseStream}}, the
  \code{MemoryCaseStream} is a subclass of
  \code{\linkS4class{CaseStream}} and follows the same conventions.

  The function \code{\link{CaseMemoryStream}} opens a new memory stream using
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
\references{
  \newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{NewMemoryStream_ns},
  \url{http://homepage.stat.uiowa.edu/~luke/R/references/weakfinex.html}
}
\author{Russell Almond}
\note{
  In version 0.5 of RNetica, this class was renamed.  It is now called
  \code{MemoryCaseStream} and the constructor is called
  \code{\link{CaseMemoryStream}} (while previously the class and the
  constructore had the same name).  This matches the usage of
  \code{\linkS4class{FileCaseStream}} and its constructor
  \code{\link{CaseFileStream}}.   This is not likely to be a problem as
  memory streams are not working well.

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
  See \code{\linkS4class{CaseStream}} for the superclass and
  \code{\linkS4class{FileCaseStream}} for a sibling class.
  The function \code{\link{CaseMemoryStream}} is the constructor.

  
  \code{\link{CaseFileDelimiter}}, \code{\link{CaseFileMissingCode}},
  \code{\link{WriteFindings}}, \code{\link{ReadFindings}},
  \code{\link{MemoryStreamContents}}


}
\examples{
sess <- NeticaSession()
startSession(sess)

abc <- CreateNetwork("ABC", session=sess)
A <- NewDiscreteNode(abc,"A",c("A1","A2","A3","A4"))
B <- NewDiscreteNode(abc,"B",c("B1","B2","B3"))
C <- NewDiscreteNode(abc,"C",c("C1","C2"))

AddLink(A,B)
AddLink(A,C)
AddLink(B,C)

## This is the file written in CaseFileStream help.
casefile <- file.path(library(help="RNetica")$path,
                           "testData","abctestcases.cas")
                     
CaseFileDelimiter("\t", session=sess)
CaseFileMissingCode("*", session=sess)
cases <- read.CaseFile(casefile, session=sess)

memstream <- CaseMemoryStream(cases, session=sess)


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

stopSession(sess)

}
\keyword{ interface }
\keyword{ IO }
\keyword{classes}
\section{Fields}{
  Note these should be regarded as read-only from user code.
  \describe{
    \item{\code{Name}:}{Object of class \code{character} identifier for
      stream. Default is the expression used to reference the data. }
    \item{\code{Session}:}{Object of class \code{NeticaSession}  }
    \item{\code{Session}:}{Object of class \code{NeticaSession} a link
      to the session in which this case stream was created. }
    \item{\code{Netica_Case_Stream}:}{Object of class \code{externalptr}
      a pointer to the case stream in Netica memory. }
    \item{\code{Case_Stream_Position}:}{Object of class \code{integer}
      the number of the last read/writen record. This is \code{NA} if
      the end of the file has been reached. }
    \item{\code{Case_Stream_Lastid}:}{Object of class \code{integer} the
    ID number of the last read/written record.}
    \item{\code{Case_Stream_Lastfreq}:}{Object of class \code{numeric}
      giving the frequence of the last read/written record.  This is
      used as a weight in learning applications. }
    \item{\code{Case_Stream_DataFrameName}:}{Object of class
      \code{character} giving the expression used for the data frame.}
    \item{\code{Case_Stream_DataFrame}:}{Object of class
      \code{data.frame} or \code{NULL} the data object that is the
      contents of the buffer, or \code{NULL} if the stream was created
      for writing.}
    \item{\code{Case_Stream_Buffer}:}{Object of class \code{externalptr}
      used for an R-side string buffer, currently not used. }
  }
}
\section{Methods}{
  \describe{
    \item{\code{open()}:}{ Opens a connection too the file in Netica. }
    \item{\code{show()}:}{ Provides a description of the field }
    \item{\code{initialize(Name, Session, Case_Stream_Path, ...)}:}{
      internal constructor; user code should use \code{\link{CaseFileStream}}. }
  }

The following methods are inherited (from \code{\linkS4class{CaseStream}}):
close ("CaseStream"), isActive ("CaseStream"), isOpen ("CaseStream"), show ("CaseStream"), clearErrors ("CaseStream"), reportErrors ("CaseStream"), initialize ("CaseStream")
}
