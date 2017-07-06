\name{IsNodeDeterministic}
\alias{IsNodeDeterministic}
\title{
  Determines if a node in a Netica Network is deterministic or not.
}
\description{

  A node in a Bayesian network is deterministic if its value is
  determined by the states of its parents, that is if all conditional
  probabilities are 0 or 1.

}
\usage{
IsNodeDeterministic(node)
}
\arguments{
  \item{node}{
    An active \code{\link{NeticaNode}} whose conditional probability
    table is to be tested.
  }
}
\details{
  For discrete nodes, this returns \code{TRUE} if all the conditional
  probabilities are zero or one.  It returns \code{FALSE} otherwise.
}
\value{
  \code{TRUE} if the conditional probability table for \code{node} is
  deterministic, \code{FALSE} otherwise.  If the node is not active, or
  there is otherwise an error it returns \code{NA}.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{IsNodeDeterministic_bn}
}
\author{
  Russell Almond
}
\seealso{
  \code{\link{NeticaNode}}, \code{\link{NodeParents}()},
  \code{\link{NodeInputNames}()}, \code{\link{NodeStates}()}
}
\examples{
ab <- CreateNetwork("AB")
A <- NewDiscreteNode(ab,"A",c("A1","A2","A3"))
B <- NewDiscreteNode(ab,"B",c("B1","B2"))
AddLink(A,B)

##Undefined node is not deterministic.
stopifnot(!IsNodeDeterministic(A))

NodeProbs(A) <- c(0,1,0)
stopifnot(IsNodeDeterministic(A))

NodeProbs(A) <- c(1/3,1/3,1/3)
stopifnot(!IsNodeDeterministic(A))

NodeProbs(B) <- rbind(c(0,1), c(0,1), c(1,0))
stopifnot(IsNodeDeterministic(B))

DeleteNetwork(ab)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ interface }
\keyword{ logic }% __ONLY ONE__ keyword per line
