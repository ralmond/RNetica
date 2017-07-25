\name{NodeVisPos}
\alias{NodeVisPos}
\alias{NodeVisPos<-}
\title{
  Gets, sets the visual position of the node on the Netica display.
}
\description{
  When displayed in the GUI, Netica nodes have a position.  The
  \code{NodeVisPos()} attribute controls where the node will be
  displayed.  
}
\usage{
NodeVisPos(node)
NodeVisPos(node) <- value
}
\arguments{
  \item{node}{
    A \code{\link{NeticaNode}} object whose position is to be determined.
  }
  \item{value}{
    A numeric vector of length 2 giving the \eqn{x} and \eqn{y} coordinates.
  }
}
\details{
  The visual position of the node doesn't make much different in RNetica,
  as R does not display the node.  However, it will control the
  appearance when the node is loaded into the Netica GUI.

}
\value{
  A numeric vector of length 2 with names \code{"x"} and \code{"y"}.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GetNodeVisPosition_bn}, \nref{SetNodeVisPosition_bn}, 
}
\author{
  Russell Almond
}
\note{
  The minimum possible node position appears to be (0,0) and the maximum
  is never stated.  Netica appears to round positions to the nearest
  integer.  Also, if the position appears too close to the boarder
  (Netica positions the center of the node), Netica will move it away
  from the edge.
}
\seealso{
  \code{\link{NeticaNode}}, \code{\link{NodeVisPos}()}
}
\examples{
pnet <- CreateNetwork("PositionNet")

pnode <- NewDiscreteNode(pnet,"PlaceMe")

NodeVisPos(pnode) <- c(100,300)
pos <- NodeVisPos(pnode)
stopifnot(
  pos["x"] ==100,
  pos["y"] ==300
)

## Netica rounds noninteger positions.
NodeVisPos(pnode) <- c(74.3,88.8)
pos <- NodeVisPos(pnode)
stopifnot(
  pos["x"] ==74,
  pos["y"] ==88
)

## Warning, setting a node too close to the edge can cause Netica to
## reposition the node
NodeVisPos(pnode) <- c(1,1)
pos <- NodeVisPos(pnode)
stopifnot(
  pos["x"] >1,
  pos["y"] >1
)

DeleteNetwork(pnet)


}
\keyword{ interface }
\keyword{ attribute }% __ONLY ONE__ keyword per line
