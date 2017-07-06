\name{NodeValue}
\alias{NodeValue}
\alias{NodeValue<-}
\title{Sets the numeric value of a continuous node
}
\description{
  This enters a numeric value (finding) for a continuous node or a
  discrete node which has numeric values assigned to the states.
}
\usage{
NodeValue(node)
NodeValue(node) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{node}{
    An active \code{\link{NeticaNode}} object that references the node.
    The node should be continuous or have a numeric value associated
    with each level (see \code{\link{NodeLevels}}).
}
  \item{value}{
    A real value for the node.
}
}
\details{
  The behavior of the levels depends on whether the node is discrete
  (\code{\link{is.discrete}(node)==TRUE}) or continuous
  (\code{\link{is.discrete}(node)==TRUE}).

  \bold{Discrete.}  For if \code{node} is a discrete node, then the
  states of \code{node} must have been assigned numeric values with
  \code{\link{NodeLevels}(node)} in order for \code{NodeValue()} to make
  sense.  If all states have not been assigned values,
  \code{NodeValue()} will generate an error.

  If the value of the node is determined, either because the value of
  the node has been set with \code{\link{NodeFinding}()} or because the
  value can be determined exactly from the value of other nodes in the
  network (through logical probability distributions or formulae), then
  \code{NodeValue(node)} will return the value associated with the
  state of the node.  Otherwise, it will return \code{NA}.

  The expression \code{NodeValue(node)<-value}, can be used to
  set the value of \code{node} to the state associate with the numerical
  \code{value}.  If \code{value} does not correspond to one of the node
  levels, this will generate an error.

  \bold{Continuous.}  For continuous nodes,
  \code{\link{NodeFinding}(node)} returns the value associated with the
  node, either set with a previous call to
  \code{NodeValue(node)<-value}, or which can be determined
  through formulae.

  The expression \code{NodeValue(node) <- value} will set the
  value (the equivalent of a finding for a discrete node) to
  \code{value}.  If \code{node} has been associated with cut
  scores through a previous call to \code{\link{NodeLevels}(node)}, then
  this will also associate a finding with the node.

}
\value{
  The function \code{NodeValue(node)} returns the value of \code{node}
  if that can be determined (see Details) or \code{NA} if it cannot.  It
  may generate an error if \code{node} is discrete and has not had
  numeric values associated with its states.

  The expression \code{NodeValue(node) <- value} returns \code{node}. 
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GetNodeValueEntered_bn}, \nref{EnterNodeValue_bn}
}
\author{
  Russell Almond
}
\note{
  Netica manual is not particularly clear on how continuous nodes are handled.
  
}
\seealso{
  \code{\link{NodeFinding}()}, \code{\link{NodeLevels}()},
  \code{\link{EnterNegativeFinding}()}, \code{\link{EnterFindings}()},
  \code{\link{RetractNodeFinding}()}, \code{\link{NodeStates}()},
  \code{\link{NodeEquation}()},\code{\link{is.continuous}()},
  \code{\link{NodeExpectedValue}()}, \code{\link{NodeBeliefs}()}
}
\examples{

aNet <- CreateNetwork("aNet")

dTheta <- NewDiscreteNode(aNet, "ThetaD",
          c("neg2","neg1","zero","pos1","pos2"))
NodeLevels(dTheta) <- c(-2,-1,0,1,2)

NodeFinding(dTheta) <- "pos1"
stopifnot(NodeValue(dTheta)==1)
NodeValue(dTheta) <- 0
stopifnot(NodeFinding(dTheta)=="zero")
\dontrun{
## The error handling seems broken under Windows.
cat("This next statement generates an error as 1/2 is not a legal value.")
stopifnot(class(try(NodeValue(dTheta) <- 1/2)) == "try-error")
}

cTheta <- NewContinuousNode(aNet, "ThetaC")
NodeLevels(cTheta) <- qnorm(c(.001,1/5,2/5,2/5,4/5,.999))
## Netica doesn't allow - sign or decimal point in state name, need to
## jump through a few hoops here.
midpoints <- round(qnorm((1:5)/5-.1),2)
NodeStates(cTheta) <- sub(".","o",
    paste(ifelse(midpoints<0,"n","p"),abs(midpoints),sep=""),
    fixed=TRUE)

NodeValue(cTheta) <- -1
stopifnot(NodeFinding(cTheta)=="n1o28")
NodeFinding(cTheta) <- "p0"
## No value associated with this finding.
stopifnot(is.na(NodeValue(cTheta)))


DeleteNetwork(aNet)


}
\keyword{ interface }
