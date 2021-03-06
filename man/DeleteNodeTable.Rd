\name{DeleteNodeTable}
\alias{DeleteNodeTable}
\title{
  Deletes the conditional probability table of a Netica node.
}
\description{
  This function completely removes the conditional probability table
  (CPT) associated with a node.
}
\usage{
DeleteNodeTable(node)
}
\arguments{
  \item{node}{
    An active \code{\linkS4class{NeticaNode}} whose conditional probability
    table is to be tested.
  }
}
\value{
  Returns the modified node invisibly.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{DeleteNodeTables_bn}
}
\author{
  Russell Almond
}
\seealso{
  \code{\linkS4class{NeticaNode}}, \code{\link{NodeParents}()},
  \code{\link{NodeInputNames}()}, \code{\link{HasNodeTable}()}
}
\examples{
sess <- NeticaSession()
startSession(sess)

a1 <- CreateNetwork("AB1", session=sess)
A <- NewDiscreteNode(a1,"A",c("A1","A2"))

NodeProbs(A) <- c(0,1)
stopifnot(
  all(HasNodeTable(A))==TRUE
)

DeleteNodeTable(A)
stopifnot(
  all(HasNodeTable(A))==FALSE
)

DeleteNetwork(a1)
stopSession(sess)
}
\keyword{ interface }
\keyword{ manip }
