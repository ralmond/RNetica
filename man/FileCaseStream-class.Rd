\name{FileCaseStream-class}
\Rdversion{1.1}
\docType{class}
\alias{FileCaseStream-class}

\title{Class \code{"FileCaseStream"}}
\description{

  This object is subclass of \code{\linkS4class{CaseStream}} so it is a
  wrapper around a Netica stream which is used to read/write cases. In
  this subclass, the case stream is associated with a Netica case file
  (\sQuote{.cas} extension).  The function \code{\link{CaseFileStream}}
  is the constructor.  The function \code{\link{ReadFindings}} reads the
  findings from the stream and the function \code{\link{WriteFindings}}
  writes them out.

  
}
\section{Extends}{
Class \code{"\linkS4class{CaseStream}"}, directly.

All reference classes extend and inherit methods from
\code{"\linkS4class{envRefClass}"}. Note that because this is a reference
class unlike traditional S3 and S4 classes it can be destructively
modified.  Also fields (slots) are accessed using the \sQuote{$}
operator.

}
\references{
  \newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{NewFileStream_ns},
  \nref{DeleteStream_ns} 
  \url{http://homepage.stat.uiowa.edu/~luke/R/references/weakfinex.html}
}
\author{Russell Almond}
\note{

  In version 0.5 of RNetica, this class was renamed.  It is now called
  \code{FileCaseStream} but the constructor is still called
  \code{\link{CaseFileStream}} (while previously the class and the
  filename had the same name).  This matches the usage of
  \code{\linkS4class{FileCaseStream}} and its constructor
  \code{\link{CaseFileStream}}.   It is also now a reference class
  instead of an informal S3 class.  This is only likely to be a problem
  for code that was using the hard coded class name.
  
  Stream objects are fragile, and will not survive saving and restoring
  an R session.  However, the object retains information about itself,
  so that calling \code{OpenCaseStream} on the saved object, should
  reopen the stream.  Note that any position information will be lost.

}


\seealso{
  See \code{\linkS4class{CaseStream}} for the superclass and
  \code{\linkS4class{MemoryCaseStream}} for a sibling class.
  The function \code{\link{CaseFileStream}} is the constructor.


  \code{\link{OpenCaseStream}},
  \code{\link{CaseFileDelimiter}}, \code{\link{CaseFileMissingCode}},
  \code{\link{WriteFindings}}, \code{\link{ReadFindings}},
  \code{\link{WithOpenCaseStream}}

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

## Outputfilename
casefile <- tempfile("testcase",fileext=".cas")

filestream <- CaseFileStream(casefile, session=sess)
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
stopSession(sess)

}
\keyword{ interface }
\keyword{ IO }
\keyword{classes}
\section{Fields}{
  Note these should be regarded as read-only from user code.
  \describe{
    \item{\code{Name}:}{Object of class \code{character} used in printed
      representation.  Default is
      \code{\link[base]{basename}(\var{Case_Stream_Path})}.}  
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
close ("CaseStream"), isActive ("CaseStream"), isOpen ("CaseStream"),
show ("CaseStream"), clearErrors ("CaseStream"), reportErrors
("CaseStream"), signalErrors ("CaseStream"), initialize ("CaseStream")
}
