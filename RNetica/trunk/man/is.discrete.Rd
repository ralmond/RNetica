\name{is.discrete}
\alias{is.discrete}
\alias{is.continuous}
\title{
  Determines whether a Netica node is discrete or continuous.
}
\description{
  A \code{\link{NeticaNode}} object can take on either a discrete set of
  values or an arbitrary real value.  These functions determine which
  type of node this is.
}
\usage{
is.discrete(node)
is.continuous(node)
}
\arguments{
  \item{node}{
    A \code{NeticaNode} object to test.
  }
}
\details{
  While in the Netica GUI, one first creates a node and then determines
  whether it will be discrete or continuous, in the API this is
  determined at the time of creation (by calling
  \code{\link{NewContinuousNode}()} or \code{\link{NewDiscreteNode}()}.
  These functions determine which type of node the given node is.

  Note that setting \code{\link{NodeLevels}} can make a continuous node
  behave like a discrete one and vice versa.  For continuous nodes, the
  levels are cut points for getting a discrete state from the node.  For
  a discrete node, the levels are real values representing the midpoint
  of the states.  
}
\value{
  \code{TRUE} or \code{FALSE} depending on whether a node is discrete or
  continuous. 
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GetNodeType_bn}, \nref{SetNodeLevels_bn}
}
\author{
  Russell Almond
}
\note{
  Currently, this function does not actually look at the internal Netica
  state, but rather looks at the attribute \code{"node_discrete"} which
  is set when the node is created.
}
\seealso{
  \code{\link{NewDiscreteNode}()}, \code{\link{NewContinuousNode}()},
  \code{\link{NeticaNode}}, \code{\link{NodeLevels}()},
  \code{\link{NodeStates}()} 
}
\examples{
netx <- CreateNetwork("netx")

bnode <- NewDiscreteNode(netx,"bool",c("True","False"))
stopifnot(is.discrete(bnode))
stopifnot(!is.continuous(bnode))

rnode <- NewContinuousNode(netx,"real")
stopifnot(!is.discrete(rnode))
stopifnot(is.continuous(rnode))

DeleteNetwork(netx)
}
\keyword{ interface }
\keyword{ logic }% __ONLY ONE__ keyword per line
