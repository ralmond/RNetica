\name{NodeProbs}
\alias{NodeProbs}
\alias{NodeProbs <-}
\title{
  Gets or sets the conditional probability table associated with a
  Netica node.
}
\description{
  A complete Bayesian networks defines a conditional probability
  distribution for a node given its parents.  If all the nodes are
  discrete, this comes in the form of a conditional probability table a
  multidimensional array whose first several dimensions follow the
  parent variable and whose last dimension follows the child variable.
}
\usage{
NodeProbs(node)
NodeProbs(node) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{node}{
    An active, discrete \code{\link{NeticaNode}} whose conditional
    probability table is to be accessed.
  }
  \item{value}{
    The new conditional probability table.  See details for the expected
    dimensions. 
}
\details{

  Let \code{node} be the node of interest and \code{parent\textit{1}},
  \code{parent\textit{2}}, ..., \code{parent\textit{p}}, where $p$ is
  the number of parents.  Let \code{pdim = sapply(\link{NodeParents}(node),
  \link{NodeNumStates})} be a vector with the number of states for each parent.
  A parent configuration is defined by assigning each of the parent
  values to one of its possible states.  Each parent configuration
  defines a (conditional) probability distribution over the possible
  states of \code{node}.

  The result of \code{NodeProbs(node)} will be an array with dimensions
  \code{c(pdim, NodeNumStates(node))}.  The first $p$ states will be
  named according to the \code{\link{NodeInputStates}(node)} or the
  \code{\link{NodeName}(parent)} if the input names are not set.  The
  last dimension will be named according to the node itself.  The
  \code{dimnames} for the resulting array will correspond to the state
  names.  

  The setter form expects an array of the same dimensions as an
  argument, although it does not need to have the dimnames set.
  
}
\value{
  A honkin' great array.  See details.
}
\references{
  \link{http://norsys.com/onLineAPI/Manual/index.html}:
  \code{GetNodeProbs_bn()}, \code{SetNodeProbs_bn()}
}
\author{
  Russell Almond
}
\note{
  All of this assumes that these are discrete nodes, that is
  \code{\link{is.discrete}(node)} will return true for both \code{node}
  and all of the parents.  It is unknown what Netica does is this is not
  right.

  This doc file is still pretty lame.  Probably need to redo output as a
  CPT class.
}
\examples{
abc <- CreateNetwork("ABC")
A <- NewDiscreteNode(abc,"A",c("A1","A2","A3","A4"))
B <- NewDiscreteNode(abc,"B",c("B1","B2","B3"))
C <- NewDiscreteNode(abc,"C",c("C1","C2"))

AddLink(A,B)
AddLink(A,C)
AddLink(B,C)

NodeProbs(A)<-c(.1,.2,.3,.4)
NodeProbs(B) <- normCPT(matrix(1:12,4,3))
NodeProbs(C) <- normCPT(array(1:24,c(4,3,2)))

NodeProbs(A)
NodeProbs(B)
NodeProbs(C)

}
\keyword{ interface }
\keyword{ model }% __ONLY ONE__ keyword per line
