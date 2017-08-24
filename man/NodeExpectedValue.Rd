\name{NodeExpectedValue}
\alias{NodeExpectedValue}
\title{Calculates expected value for a numeric node
}
\description{

  Calculates the expected value for \var{node} based on the current
  beliefs about the nodes states.  The \var{node} should either be
  continuous or a discrete node with levels assigned to the values.  The
  standard deviation is supplied as an attribute.

}
\usage{
NodeExpectedValue(node)
}
\arguments{
  \item{node}{
    An active \code{\linkS4class{NeticaNode}} object that references the node.
    The node should be continuous or have a numeric value associated
    with each level (see \code{\link{NodeLevels}}).
  }
}
\value{

  Returns a scalar real giving the expected value for \var{node}.  It
  has an attribute called \code{"std_dev"} which contains the standard
  deviation. 

}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GetNodeExpectedValue_bn}
}
\author{
  Russell Almond
}
\seealso{
  \code{\link{NodeBeliefs}()}, \code{\link{NodeLevels}()},
  \code{\link{NodeLevels}()},\code{\link{is.continuous}()}
  \code{\link{NodeValue}()},\code{\link{CalcNodeValue}()},

}
\examples{
sess <- NeticaSession()
startSession(sess)

irt5 <- ReadNetworks(file.path(library(help="RNetica")$path,
                           "sampleNets","IRT5.dne"), session=sess)

irt5.theta <- NetworkFindNode(irt5,"Theta")
irt5.x <- NetworkFindNode(irt5,paste("Item",1:5,sep="_"))

CompileNetwork(irt5) ## Ready to enter findings

## Prior should have mean 0, Std 1.095
stopifnot(abs(NodeExpectedValue(irt5.theta)) <.000001)
stopifnot(abs(attr(NodeExpectedValue(irt5.theta),"std_dev")-1.095445)<.00001)

NodeFinding(irt5.x[[1]]) <- "Right"
## Expected value should go up
stopifnot(NodeExpectedValue(irt5.theta)>0)

DeleteNetwork(irt5)
stopSession(sess)

}
\keyword{ interface }
\keyword{ manip }
