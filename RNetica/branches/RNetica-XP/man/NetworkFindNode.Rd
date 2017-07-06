\name{NetworkFindNode}
\alias{NetworkFindNode}
\alias{NetworkAllNodes}
\title{
  Finds nodes in a Netica network.
}
\description{

  The function \code{NetworkFindNode} finds a node in a
  \code{\link{NeticaBN}} with the given name.  If no node with the
  specified name found, it will return \code{NULL}. The function
  \code{NetworkAllNodes()} returns a list of all nodes in the network.

}
\usage{
NetworkFindNode(net, name)
NetworkAllNodes(net)
}
\arguments{
  \item{net}{
    The \code{NeticaBN} to search.
}
  \item{name}{
    A character vector giving the name or names of the desired nodes.
    Names must follow the \code{\link{IDname}} protocol.
}
}
\details{
  Although each \code{\link{NeticaNode}} belongs to a single network, a
  network contains many nodes.  Within a network, a node is uniquely
  identified by its name.  However, nodes can be renamed (see
  \code{\link{NodeName}()}).  

  The function \code{NetworkAllNodes()} returns all the nodes in the
  network, however, the order of the nodes in the network could be
  different in different calls to this function.
  
}
\value{
  The \code{\link{NeticaNode}} object or list of \code{NeticaNode}
  objects corresponding to \code{names}, or a list of all node objects for
  \code{NetworkAllNodes()}.  In the latter case, the names will be set
  to the node names.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html},
    \nref{GetNodeNamed_bn}, \nref{GetNetNodes_bn}
}
\author{
  Russell Almond
}
\note{
  \code{NeticaNode} objects do not survive the life of a
  Netica session (or by implication an R session).  So the safest way to
  "save" a \code{NeticaNode} object is to recreate it using
  \code{NetworkFindNode()} after the network is reloaded.
}
\seealso{
  \code{\link{NodeNet}()} retrieves the network from the node.
}
\examples{
tnet <- CreateNetwork("TestNet")
nodes <- NewDiscreteNode(tnet,c("A","B","C"))

nodeA <- NetworkFindNode(tnet,"A")
stopifnot (nodeA==nodes[[1]])

nodeBC <- NetworkFindNode(tnet,c("B","C"))
stopifnot(nodeBC[[1]]==nodes[[2]])
stopifnot(nodeBC[[2]]==nodes[[3]])

allnodes <- NetworkAllNodes(tnet)
stopifnot(length(allnodes)==3)
stopifnot(any(nodeA==allnodes)) ## NodeA in there somewhere.

\dontrun{
## Safe way to preserve node and network objects across R sessions.
tnet <- WriteNetworks(tnet,"Tnet.neta")
q(save="yes")
# R
library(RNetica)
tnet <- ReadNetworks(tnet)
nodes <- NetworkFindNodes(tnet,as.character(nodes))
}
DeleteNetwork(tnet)
}
\keyword{ interface }
\keyword{ graphs }
\keyword{ utilities }

