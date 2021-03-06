\name{NodeProbs}
\alias{NodeProbs}
\alias{NodeProbs<-}
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
\arguments{
  \item{node}{
    An active, discrete \code{\linkS4class{NeticaNode}} whose conditional
    probability table is to be accessed.
  }
  \item{value}{
    The new conditional probability table.  See details for the expected
    dimensions.
  }
}
\details{

  Let \code{node} be the node of interest and \code{parent\var{1}},
  \code{parent\var{2}}, ..., \code{parent\var{p}}, where \eqn{p} is
  the number of parents.  Let \code{\var{pdim} =
  sapply(\link{NodeParents}(\var{node}), 
  \link{NodeNumStates})} be a vector with the number of states for each parent.
  A parent configuration is defined by assigning each of the parent
  values to one of its possible states.  Each parent configuration
  defines a (conditional) probability distribution over the possible
  states of \var{node}.

  The result of \code{NodeProbs(\var{node})} will be an array with dimensions
  \code{c(\var{pdim}, NodeNumStates(\var{node}))}.  The first \eqn{p}
  dimensions will be named according to the
  \code{\link{NodeInputNames}(\var{node})} or the 
  \code{\link{NodeName}(\var{parent})} if the input names are not set.  The
  last dimension will be named according to the node itself.  The
  \code{dimnames} for the resulting array will correspond to the state
  names.  

  In the \code{CPTtools} package, this known as the
  \code{\link[CPTtools]{CPA}} format, and tools exist to convert between
  this form an a two dimensional matrix, or \code{\link[CPTtools]{CPF}}
  format. 
 
  The setter form expects an array of the same dimensions as an
  argument, although it does not need to have the dimnames set.
  
}
\value{
  A conditional probability array of class
  \code{c("\link{CPA}","array")}.  See details.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GetNodeProbs_bn}, \nref{SetNodeProbs_bn}
}
\author{
  Russell Almond
}
\note{

  Note that the expression \code{node[...]} also accesses the partial or
  complete node conditional probability table.  See
  \code{\link{Extract.NeticaNode}}.

  All of this assumes that these are discrete nodes, that is
  \code{\link{is.discrete}(node)} will return true for both \code{node}
  and all of the parents.  If the nodes are continuous, they need to
  be discritized through the use of \code{\link{NodeLevels}}.  
}
\seealso{
  \code{\link{Extract.NeticaNode}},
  \code{\linkS4class{NeticaNode}}, \code{\link{NodeParents}()},
  \code{\link{NodeInputNames}()}, \code{\link{NodeStates}()},
  \code{\link[CPTtools]{CPA}},
  \code{\link[CPTtools]{CPF}}, \code{\link[CPTtools]{normalize}()}
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

NodeProbs(A)<-c(.1,.2,.3,.4)
NodeProbs(B) <- normalize(matrix(1:12,4,3))
NodeProbs(C) <- normalize(array(1:24,c(4,3,2)))

Aprobs <- NodeProbs(A)
Bprobs <- NodeProbs(B)
Cprobs <- NodeProbs(C)
stopifnot(
  CPTtools::is.CPA(Aprobs),
  CPTtools::is.CPA(Bprobs),
  CPTtools::is.CPA(Cprobs)
)

DeleteNetwork(abc)
stopSession(sess)
}
\keyword{ interface }
\keyword{ model }
