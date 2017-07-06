\name{AbsorbNodes}
\alias{AbsorbNodes}
\title{
  Delete a Netica nodes in a way that maintains the connectivity.
}
\description{
  This function deletes \code{\link{NeticaNode}} connecting
  the parents of the deleted node to its children.  If multiple nodes
  are passed as the argument, then all of the nodes are absorbed.  The
  joint probability distribution over the remaining nodes should be the
  same as the marginal probability distribution over the remaining nodes
  before the nodes were deleted.
}
\usage{
AbsorbNodes(nodes)
}
\arguments{
  \item{nodes}{
    A \code{NeticaNode} or list of \code{NeticaNode}s to be deleted.
  }
}
\details{
  This function provides a way of removing a node without affecting the
  connectivity, or the joint probability of the remaining nodes.  In
  particular, all of the relationship tested by
  \code{\link{is.NodeRelated}()} among the remaining nodes should remain
  true (or false) when we are done.
}
\value{
  Returns \code{NULL}.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{AbsorbNodes_bn}
}
\author{
  Russell Almond
}
\seealso{
  \code{\link{NeticaNode}}, \code{\link{AddLink}()},
  \code{\link{NodeChildren}()}, \code{\link{NodeParents}()},
  \code{\link{ReverseLink}()}, \code{\link{is.NodeRelated}()}
}
\examples{
anet <- CreateNetwork("Absorbent")

xnodes <- NewDiscreteNode(anet,paste("X",1:5,sep="_"))
AddLink(xnodes[[1]],xnodes[[2]])
AddLink(xnodes[[2]],xnodes[[3]])
AddLink(xnodes[[3]],xnodes[[4]])
AddLink(xnodes[[3]],xnodes[[5]])

stopifnot(
 all(match(xnodes[4:5],NodeChildren(xnodes[[3]]),nomatch=0)>0),
 is.NodeRelated(xnodes[[2]],xnodes[[3]],"parent"),
 is.NodeRelated(xnodes[[2]],xnodes[[1]],"child")
)

## These are leaf nodes, shouldn't change topology, except locally. 
AbsorbNodes(xnodes[4:5])
stopifnot(
  ## Nodes 4 and 5 are now deleted
  all(!is.active(xnodes[4:5])),
  length(NodeChildren(xnodes[[3]]))==0,
  is.NodeRelated(xnodes[[2]],xnodes[[3]],"parent"),
  is.NodeRelated(xnodes[[2]],xnodes[[1]],"child")
)

## This should connect X1->X3
AbsorbNodes(xnodes[[2]])
stopifnot(
  ## Node 2 is now deleted
  !is.active(xnodes[[2]]),
  length(NodeChildren(xnodes[[3]]))==0,
  is.NodeRelated(xnodes[[1]],xnodes[[3]],"parent"),
  is.NodeRelated(xnodes[[3]],xnodes[[1]],"child")
)

DeleteNetwork(anet)

}
\keyword{ interface }
\keyword{ graphs }
\keyword{ manip }% __ONLY ONE__ keyword per line
