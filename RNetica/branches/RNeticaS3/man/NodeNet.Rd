\name{NodeNet}
\alias{NodeNet}
\title{
  Finds which Netica network a node comes from.
}
\description{
  Each active \code{\link{NeticaNode}} object lives inside of a
  \code{\link{NeticaBN}} object.  This function finds the network
  corresponding to a node.
}
\usage{
NodeNet(node)
}
\arguments{
  \item{node}{
    A \code{\link{NeticaNode}} object.
}
}
\details{
  Two nodes with the same details in different networks are not
  identical inside of Netica.

  This function only works for active nodes.  If
  \code{\link{is.active}(node)} returns false, this function will return
  \code{NULL}.

  The functions \code{\link{NetworkAllNodes}()} and
  \code{\link{NetworkFindNode}()} provide pseudo-inverses for this
  function. 
}
\value{
  A \code{\link{NeticaBN}} object which contains \code{node}, or
  \code{NULL} if \code{node} is not active.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
    \url{http://norsys.com/onLineAPIManual/index.html}: \nref{GetNodeNet_bn}
}
\author{
  Russell Almond
}
\seealso{
  \code{\link{NeticaBN}}, \code{\link{NeticaNode}},
  \code{\link{is.active}()}, \code{\link{NetworkAllNodes}()},
  \code{\link{NetworkFindNode}()}
}
\examples{
neta <- CreateNetwork("Net_A")
netb <- CreateNetwork("Net_B")

nodea <- NewContinuousNode(neta,"Node")
nodeb <- NewContinuousNode(netb,"Node")

stopifnot(NodeNet(nodea)==neta)
stopifnot(NodeNet(nodeb)==netb)

## Note
stopifnot(nodea != nodeb)
## But:
stopifnot(as.character(nodea) == as.character(nodeb))

DeleteNodes(nodeb)
stopifnot(is.null(NodeNet(nodeb)))

DeleteNodes(nodea)

## Now:
stopifnot(nodea == nodeb)

DeleteNetwork(list(neta,netb))



}
\keyword{ interface }
\keyword{ graphs }% __ONLY ONE__ keyword per line
