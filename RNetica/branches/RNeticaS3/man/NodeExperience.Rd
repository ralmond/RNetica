\name{NodeExperience}
\alias{NodeExperience}
\alias{NodeExperience<-}
\title{Gets or sets the amount of experience associated with a node.
}
\description{
  In learning, if the row of the conditional probability table has a
  Dirichlet distribution, this sets the sum of the parameters for the
  row.  This is the number of pseudo observations for that row of the
  CPT. 
}
\usage{
NodeExperience(node)
NodeExperience(node) <- value
}
\arguments{
  \item{node}{
    An active \code{\link{NeticaNode}}.
}
  \item{value}{
    An array of pseudo counts, these should be positive values.  The
    shape of the array should match the
    \code{\link{ParentStates}(node)}.  
  }
}
\details{
  When learning the conditional probabilities associated with a
  conditional probability table, the most general model considers each
  row of the conditional probability table as an independent Dirichlet
  distribution.  If there are \eqn{k} states, then the parameters of the
  Dirichlet distribution are \eqn{a_1,\ldots,a_k} and the expected value
  is \eqn{p_1=a_1/n,\ldots,p_k=a_k/n}, where \eqn{n=a_1+\ldots+a_k} is
  the normalization constant.  An alternative way to represent the
  Dirichlet parameters is with the probability vector and the
  normalization.  The \emph{experience} is the normalization constant.
  Note that after observing \eqn{m} additional observations, the
  normalization constant will become \eqn{n+m}, so the experience can be
  thought of as a pseudo-observation count.  Finally, the variance of
  the Dirichlet distribution decreases, as \eqn{n} increases, so it can
  also be thought of as a measure of precision.

  An unconditional distribution has exactly one normalization constant.
  A conditional distribution has on for each row of the conditional
  probability, that is associated with each possible configuration of
  the parent variables.  The value of \code{NodeExperience(node)} is an
  array with dimnames matching \code{\link{ParentStates}(node)}.  In
  particular, this means that specific values of experience can be
  accessed by using the names of the parent states.
  
}
\value{
  An array whose dimnames are \code{\link{ParentStates}(node)}.  If the
  node has no parents, the value is a scalar.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{SetNodeExperience_bn}, \nref{GetNodeExperience_bn}
}
\author{Russell Almond}
\note{
  I tend to refer to this distribution as a "hyper-Dirichlet"
  distribution, although Spiegelhalter and Lauritzen (1990) used that
  term to refer to a network in which all of the nodes were
  parameterized in that way.
}
\seealso{
  \code{\link{NeticaNode}}, \code{\link{NodeParents}()},
  \code{\link{NodeProbs}()}, \code{\link{CPA}}

}
\examples{
abc <- CreateNetwork("ABC")
A <- NewDiscreteNode(abc,"A",c("A1","A2","A3","A4"))
B <- NewDiscreteNode(abc,"B",c("B1","B2","B3"))
C <- NewDiscreteNode(abc,"C",c("C1","C2"))

AddLink(A,B)
AddLink(A,C)
AddLink(B,C)

## Parentless node, only need one value
NodeExperience(A) <- 10
stopifnot(
  abs(NodeExperience(A)-10)<.00001
)

NodeExperience(B) <- c(1,2,3,4)
stopifnot(
  length(NodeExperience(B))==4,
  all(names(NodeExperience(B))==NodeStates(A)),
  abs(NodeExperience(B)[2]-2)<.00001
)

## Set them all to the same value.
NodeExperience(C) <- 10
stopifnot(
  all(dim(NodeExperience(C))==sapply(ParentStates(C),length)),
  all(dimnames(NodeExperience(C))[[1]]==ParentStates(C)[[1]]),
  all(dimnames(NodeExperience(C))[[2]]==ParentStates(C)[[2]]),
  all(names(dimnames(NodeExperience(C)))==ParentNames(C)),
  abs(NodeExperience(C)[3,2]-10)<.00001
)
NodeExperience(C)["A3","B2"] <- 11
stopifnot(
  abs(NodeExperience(C)[3,2]-11)<.00001
)


DeleteNetwork(abc)
}
\keyword{ interface }
\keyword{ model }
