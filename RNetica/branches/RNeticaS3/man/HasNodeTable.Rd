\name{HasNodeTable}
\alias{HasNodeTable}
\title{
  Tests to see if a Netica node has a conditional probability table.
}
\description{
  This function tests to see if a conditional probability table has been
  assigned to \code{node}.  The function returns two values, the first
  tests for existence of the table, the second tests for a complete
  table (no NAs).
}
\usage{
HasNodeTable(node)
}
\arguments{
  \item{node}{
    An active \code{\link{NeticaNode}} whose conditional probability
    table is to be tested.
  }
}
\details{
  This function returns two values.  The first is true or false
  according to whether the conditional probability table has been
  established, that is has \code{\link{NodeProbs}()} been set.  The
  second value tests to see whether the conditional probability table is
  complete, that is, does it have any \code{NA}s associated with it.

  In many cases, it is the second value that is of interest, so
  \code{all(HasNodeTable(node))} is often a useful idiom.
}
\value{
  A logical vector with two elements.  The first states whether or not
  the node has any of its conditional probabilities set.  The second
  tests whether or not the table has been completely specified.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{HasNodeTable_bn}
}
\author{
  Russell Almond
}
\note{
  Generating incomplete tables is pretty hard to do in RNetica, a row
  must be deliberately set to \code{NA}.  However, a network read in
  from a file might have incomplete tables.
}
\seealso{
  \code{\link{NeticaNode}}, \code{\link{NodeParents}()},
  \code{\link{NodeInputNames}()}, \code{\link{DeleteNodeTable}()}
}
\examples{
ab1 <- CreateNetwork("AB1")
A <- NewDiscreteNode(ab1,"A",c("A1","A2","A3"))
B <- NewDiscreteNode(ab1,"B",c("B1","B2"))
AddLink(A,B)

##Nodes start undefined.
stopifnot(
  HasNodeTable(A)==c(FALSE,FALSE)
)

NodeProbs(A) <- c(0,1,0)
stopifnot(
  HasNodeTable(A)==c(TRUE,TRUE)
)

for (node in NetworkAllNodes(ab1)) {
  if (!all(HasNodeTable(node))) {
   cat("Node ", node, " still needs a conditional probability table.\n")
  }
}

DeleteNetwork(ab1)

}
\keyword{ interface }
\keyword{ logic }% __ONLY ONE__ keyword per line
