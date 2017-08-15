\name{NetworkFindNode}
\alias{NetworkFindNode}
\alias{NetworkAllNodes}
\title{
  Finds nodes in a Netica network.
}
\description{

  The function \code{NetworkFindNode} finds a node in a
  \code{\linkS4class{NeticaBN}} with the given name.  If no node with the
  specified name found, it will return \code{NULL}. The function
  \code{NetworkAllNodes()} returns a list of all nodes in the network.

}
\usage{
NetworkFindNode(net, name)
NetworkAllNodes(net)
}
\arguments{
  \item{net}{
    The \code{\linkS4class{NeticaBN}} to search.
}
  \item{name}{
    A character vector giving the name or names of the desired nodes.
    Names must follow the \code{\link{IDname}} protocol.
}
}
\details{

  Although each \code{\linkS4class{NeticaNode}} belongs to a single network, a
  network contains many nodes.  Within a network, a node is uniquely
  identified by its name.  However, nodes can be renamed (see
  \code{\link{NodeName}()}).  

  The function \code{NetworkAllNodes()} returns all the nodes in the
  network, however, the order of the nodes in the network could be
  different in different calls to this function.

  Starting with RNetica version 0.5, \code{\linkS4class{NeticaBN}}
  objects keep a cache of node objects in the environment
  \code{\var{net}$nodes}.  In particular, the methods
  \code{\var{net}$findNode()} will search the cache, and
  \code{\var{net}$listNodes()} will list the names of the nodes in the
  cache.  Also, \code{\var{net}$nodes$\var{nodename}} or
  \code{\var{net}$nodes[["\var{nodename}"]]} will fetch the cached node
  (if it exists) or return \code{NULL} if it does not.

  Nodes that are created in RNetica, using \code{\link{NewDiscreteNode}}
  or \code{\link{NewContinuousNode}} are automatically added to the
  cache.  This is also true of other functions which return
  \code{\linkS4class{NeticaNode}} objects.  For example,
  \code{\link{NodeParents}(\var{node})} will add the parents of
  \var{node} to the cache if they are not there already.

  A potential problem arises when the network is read from a file using
  \code{\link{ReadNetworks}}.  This function does not automatically
  cache the nodes.  Calling \code{NetworkFindNode} will add the nodes to
  the cache.  Calling \code{NetworkAllNodes} will add all nodes to the
  cache.  Calling \code{\link{NetworkNodesInSet}} can be used to pull
  just a subsetof nodes into the cache.
  
}
\value{
  The \code{\linkS4class{NeticaNode}} object or list of \code{NeticaNode}
  objects corresponding to \code{names}, or a list of all node objects for
  \code{NetworkAllNodes()}.  In the latter case, the \sQuote{names}
  attribute of the returned list will be set
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
  \code{\linkS4class{NeticaBN}} talks more about the node cache and has
  other functions for manipulating it.
  
  \code{\link{NetworkNodesInSet}} can be used to find a labeled subset
  of nodes.

  \code{\link{NodeNet}()} retrieves the network from the node.

}
\examples{
sess <- NeticaSession()
startSession(sess)

tnet <- CreateNetwork("TestNet", session=sess)
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
sess <- NeticaSession()
startSession(sess)
tnet <- ReadNetworks(tnet, session=sess)
nodes <- NetworkFindNodes(tnet,as.character(nodes))
}
DeleteNetwork(tnet)
stopSession(sess)
}
\keyword{ interface }
\keyword{ graphs }
\keyword{ utilities }

