\name{NodeName}
\alias{NodeName}
\alias{NodeName<-}
\title{
  Gets or set of a Netica node.
}
\description{
  Gets or sets the name of the node. Names must conform to the
  \code{\link{IDname}} rules.
}
\usage{
NodeName(node)
NodeName(node)<- value
}
\arguments{
  \item{node}{
    An active \code{\link{NeticaNode}} object that references the node.
}
  \item{value}{
    An character vector of length 1 giving the new name.  
}
}
\details{
  Node names must conform to the \code{\link{IDname}} rules for
  Netica identifiers.  Trying to set the node to a name that does not
  conform to the rules will produce an error, as will trying to set the
  node name to a name that corresponds to a different node in the network.

  On a call to the setting method, if a node of the given name already
  exists, a warning will be issued and the \code{node} argument will be
  returned unchanged.

  The \code{\link{NodeTitle}()} function provides another way to name
  a node which is not subject to the \code{IDname} restrictions.

}
\value{
  The name of the node as a character vector of length 1.

  The setter method returns the \code{NeticaNode} object
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GetNodeName_bn}, \nref{SetNodeName_bn}
}
\author{
  Russell Almond
}
\note{
  \code{NeticaNode} objects are internally implemented as character vectors
  giving the name of the network.  If a node is renamed, then it is
  possible that R will hold onto an old reference that still using the
  old name.  In this case, \code{NodeName(node)} will give the correct
  name, and \code{NetworkFindNode(net,NodeName(node))} will return a
  reference to a corrected object.
}
\seealso{
  \code{\link{NewDiscreteNode}()}, \code{\link{NeticaNode}},
  \code{\link{NetworkFindNode}()}, \code{\link{NodeTitle}()} 
}
\examples{
net <- CreateNetwork("funNet")

pnode <- NewDiscreteNode(net,"play")
nodecached <- pnode

stopifnot(NodeName(pnode)=="play")

NodeName(pnode)<-"work"
stopifnot(as.character(pnode)=="work")

##Warning, the following expression is true!
as.character(nodecached) != NodeName(nodecached)
## But this one holds
stopifnot(NodeName(pnode)==NodeName(nodecached))
## And this one
stopifnot(pnode==nodecached)

## This fixes the problem
NodeName(nodecached) <- NodeName(nodecached)
stopifnot(as.character(nodecached) == NodeName(nodecached))

snode <- NewContinuousNode(net,"sleep")
NodeName(snode) <- "work" ## This should issue a warning
## And not change the name.
stopifnot(NodeName(snode)=="sleep")

allNodes <- NetworkAllNodes(net)
NodeName(allNodes$work) <- "effort"


DeleteNetwork(net)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ interface }
\keyword{ attribute }% __ONLY ONE__ keyword per line
