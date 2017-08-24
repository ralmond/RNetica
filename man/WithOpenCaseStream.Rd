\name{WithOpenCaseStream}
\alias{WithOpenCaseStream}
\title{Evaluate an expression and then close the Netica Case Stream.}
\description{
  This function evaluates \var{expr} in a context where the
  \code{\linkS4class{CaseStream}} is open.  The stream is closed when the
  evaluation is complete.  The evaluation of \var{expr} is surrounded
  with a \code{\link[base]{tryCatch}} so that the stream is closed
  whether or not the expression is successfully executed.
}
\usage{
WithOpenCaseStream(stream, expr)
}
\arguments{
  \item{stream}{A \code{\linkS4class{CaseStream}} object.  This can be
    open or closed.  If closed it is reopened.}
  \item{expr}{
    An arbitrary R expression to be executed.
}
}
\value{
  Either the result of evaluating \var{expr} unless executing
  \var{expr} results in an error in which case it 
  returns a \code{try-error}.
}
\author{Russell Almond}
\seealso{
  \code{\linkS4class{CaseStream}},\code{\link{ReadFindings}}
}
\examples{

## This function reads findings from a stream until it finds one
## matching a certain case ID.
ReadCase <- function (stream,nodes,caseID) {
  WithOpenCaseStream(stream,
    {stream <- ReadFindings(nodes,stream,"FIRST")
     while(!is.na(getCaseStreamPos(stream)) &&
           getCaseStreamLastId(stream) != caseID) {
       ReadFindings(nodes,stream,"NEXT")
     }
     if (is.na(getCaseStreamPos(stream))) {
       warning("Case ID:",caseID," not found in stream.")
     }
     stream
   })
}

sess <- NeticaSession()
startSession(sess)


## Test it.
abc <- CreateNetwork("ABC", session=sess)
A <- NewDiscreteNode(abc,"A",c("A1","A2","A3","A4"))
B <- NewDiscreteNode(abc,"B",c("B1","B2","B3"))
C <- NewDiscreteNode(abc,"C",c("C1","C2"))

AddLink(A,B)
AddLink(A,C)
AddLink(B,C)

## Input filename
## Note, this is a cached copy of the file written in the WriteFindings
## documentation. 
casefile <- file.path(library(help="RNetica")$path,
                           "testData","abctestcases.cas")


filestream <- ReadCase(CaseFileStream(casefile, session=sess),list(A,B,C),1002)
stopifnot( !isCaseStreamOpen(filestream),
           NodeFinding(A) == "A2",
           NodeFinding(B) == "B2",
           NodeFinding(C) == "C2",
           getCaseStreamLastId(filestream)==1002,
           abs(getCaseStreamLastFreq(filestream)-2.0) < .0001)

##Clean Up
DeleteNetwork(abc)
stopSession(sess)

}
\keyword{ interface }
\keyword{ IO }
