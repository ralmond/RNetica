\name{ReadFindings}
\alias{ReadFindings}
\title{Retrieves a record from a Netica Case Stream}
\description{
  This function reads a row from a Netica case stream and instantiates
  the values of the listed nodes to the values found in that row of the
  case stream.
}
\usage{
ReadFindings(nodes, stream, pos = "NEXT", add = FALSE)
}
\arguments{
  \item{nodes}{
    The a list of active \code{\link{NeticaNode}} objects to be read.
    The findings of these nodes will be modified by this call.
  }
  \item{stream}{
    A \code{\link{NeticaCaseStream}} object which references the file or
    string object to be read from.
  }
  \item{pos}{A character or integer scalar.  This should almost
    certainly be one of the two string values \dQuote{FIRST} or
    \dQuote{NEXT}.  It also can be an integer giving the position (in
    characters) where to start the machine.  This is likely to produce
    surprising results unless the integer value is a value obtained from
    calling \code{\link{getCaseStreamPos}} on this stream earlier after
    a call to either \code{ReadFindings} or \code{\link{WriteFindings}}.
}
  \item{add}{A logical scalar. If true, the findings from the case
    stream are added to the existing node.  If false, they are ignored.
  }
}
\details{
  A case file is a table where the rows represent cases, and the columns
  represent variables.  \code{ReadFindings} reads a row out the
  table and instantiates (\code{\link{NodeFinding}}) the nodes in
  \code{nodeset} to those values.  If a the value corresponding a node
  is the value of \code{\link{CaseFileMissingCode}()}, then it is not
  instantiated.  The values in the columns are separated by the value of
  \code{\link{CaseFileDelimiter}()}.

  If \code{add} is false, it will first retract any findings associated
  with the nodes in \code{nodeset}.  If a finding is associated with a
  node, the the case file would cause it to be set to an inconsistent
  value, then an error will be generated.

  The argument \code{pos} determines which record will be read next.  If
  the value is \code{"NEXT"} the next code will be read.  If the value
  is \code{"FIRST"} the first code will be read.  If the value is a
  positive integer, then the record which starts at that character will
  be read.  On completion of the read, the value of
  \code{\link{getCaseStreamPos}(stream)} is set to the starting position
  of the last read stream.  This is also true when
  \code{\link{WriteFindings}} is called.  It is almost certainly an
  error to set the \code{pos} argument to anything but either one of the
  special string constants or a value which was previously cached after
  calling \code{getCaseStreamPos}.  If the case stream is at the end,
  then \code{\link{getCaseStreamPos}(stream)} will be set to \code{NA}.
  
  There are two special columns in the file.  The column \dQuote{IDnum}
  contains ID numbers for the cases.  The value of
  \code{\link{getCaseStreamLastId}(stream)} is set to the value of this
  column if it is present in the case stream, otherwise it will be set
  to \code{-1}. The value of the column
  \dQuote{NumCases} contains a weight to give to the current row.  The
  value of \code{\link{getCaseStreamLastFreq}(stream)} is set to this
  value, if it is present.  The returned stream object will have these
  updated properties, otherwise it will be set
  to \code{-1}.
  
}
\value{
  Returns the \code{caseOrStream} argument invisibly.  Note that the
  values of \code{\link{getCaseStreamPos}(stream)} will return the
  position of the next record or \code{NA} if there are no records left
  in the stream.  The values of \code{\link{getCaseStreamLastId}(stream)}, and
  \code{\link{getCaseStreamLastFreq}(stream)} will be updated to reflect
  the values from the last read record, or will be \code{-1} if these
  values are not provided in the stream.
}
\references{
  \newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{ReadNetFindings2_bn}
}
\author{Russell G. Almond}
\note{
  The first time that \code{ReadFindings} is called on a stream it must
  be called with \code{pos="FIRST"}.  Failing to do so produces a fatal
  error in Netica.  

  The value of \code{case_posn} returned by the Netica
  \code{ReadNetFindings2_bn} function (which is the value to which
  \code{\link{getCaseStreamPos}(stream)}) is undocumented.  I confirmed
  with Brent that this is in fact the position in characters from the
  start of the stream to the record.  It is not recommended, however,
  that program rely on that fact.

  The fact that the case positions are difficult to compute makes random
  access difficult.  If it is needed, programmers will need to save the
  values of \code{\link{getCaseStreamPos}} on previous calls to
  \code{ReadFindings} or \code{\link{WriteFindings}}.  Fetching cases by
  the ID requires scanning through the case file (see
  \code{\link{WithOpenCaseStream}} for an example).

}
\seealso{
  \code{\link{CaseFileDelimiter}}, \code{\link{CaseFileMissingCode}},
  \code{\link{NodeFinding}}, \code{\link{RetractNetFindings}}
  \code{\link{ReadFindings}}, \code{\link{NeticaCaseStream}},
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

## Input filename
## Note, this is a cached copy of the file written in the WriteFindings
## documentation. 
casefile <- paste(library(help="RNetica")$path,
                           "testData","abctestcases.cas",
                           sep=.Platform$file.sep)
filestream <- CaseFileStream(casefile)
## Case 1
filestream <- ReadFindings(list(A,B,C),filestream,"FIRST")
stopifnot( NodeFinding(A) == "A1",
           NodeFinding(B) == "B1",
           NodeFinding(C) == "C1",
           getCaseStreamLastId(filestream)==1001,
           abs(getCaseStreamLastFreq(filestream)-1.0) < .0001)

pos1 <- getCaseStreamPos(filestream)
           
## Case 2
filestream <- ReadFindings(list(A,B,C),filestream,"NEXT")
stopifnot( NodeFinding(A) == "A2",
           NodeFinding(B) == "B2",
           NodeFinding(C) == "C2",
           getCaseStreamLastId(filestream)==1002,
           abs(getCaseStreamLastFreq(filestream)-2.0) < .0001)

## Case 3
filestream <- ReadFindings(list(A,B,C),filestream,"NEXT")
stopifnot( NodeFinding(A) == "A3",
           NodeFinding(B) == "B3",
           NodeFinding(C) == "@NO FINDING",
           getCaseStreamLastId(filestream)==1003,
           abs(getCaseStreamLastFreq(filestream)-1.0) < .0001)


## At end of file
filestream <- ReadFindings(list(A,B,C),filestream,"NEXT")
stopifnot(is.na(getCaseStreamPos(filestream)))

## Restart from Case 1
filestream <- ReadFindings(list(A,B,C),filestream,"FIRST")
stopifnot( NodeFinding(A) == "A1",
           NodeFinding(B) == "B1",
           NodeFinding(C) == "C1",
           getCaseStreamLastId(filestream)==1001,
           abs(getCaseStreamLastFreq(filestream)-1.0) < .0001,
           pos1 == getCaseStreamPos(filestream))

## Test with memory stream
cases <- read.CaseFile(casefile)
abcstream <- MemoryCaseStream(cases)
MemoryStreamContents(abcstream)


abcstream <- ReadFindings(list(A,B,C),abcstream,"FIRST")
stopifnot( NodeFinding(A) == "A1",
           NodeFinding(B) == "B1",
           NodeFinding(C) == "C1",
           getCaseStreamLastId(abcstream)==1001,
           abs(getCaseStreamLastFreq(abcstream)-1.0) < .0001)


##Clean Up
CloseCaseStream(filestream)
CloseCaseStream(abcstream)
DeleteNetwork(abc)

}
\keyword{ interface }
\keyword{ io }
