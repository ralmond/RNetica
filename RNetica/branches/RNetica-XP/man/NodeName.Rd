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
NodeName(node, internal=FALSE)
NodeName(node)<- value
}
\arguments{
  \item{node}{
    An active \code{\link{NeticaNode}} object that references the node.
  }
  \item{internal}{A logical scalar.  If true, the actual Netica object
    will be consulted, if false, a cached value in the R object will be
    used.}
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

  Note that the name of the node is stored in two places:  in the
  \code{Name} field of the \code{\linkS4class{NeticaNode}} object
  (\code{\var{node}$Name}), and internally in the Netica object.  These
  should be the same; however, may not be.  The \code{internal} field is
  used to force a check of the internal Netica object rather than the
  field in the R object.

}
\value{
  The name of the node as a character vector of length 1.

  The setter method returns the \code{\linkS4class{NeticaNode}} object.
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

  This paragraph is obsolete as of RNetica version 0.5, it describes the
  previous versions only.

  \code{NeticaNode} objects are internally implemented as character vectors
  giving the name of the network.  If a node is renamed, then it is
  possible that R will hold onto an old reference that still using the
  old name.  In this case, \code{NodeName(node)} will give the correct
  name, and \code{NetworkFindNode(net,NodeName(node))} will return a
  reference to a corrected object.

  Starting with RNetica 0.5, \code{\linkS4class{NeticaNode}} objects are
  cached in the \code{\linkS4class{NeticaBN}} object.  The setter
  method for \code{NodeName} updates the cache as well.

  In versions of RNetica less than 0.5, trying to set the name of a node
  to a name that was already used would generate a warning instead of an
  error.  It now generates an error.

}
\seealso{
  \code{\link{NewDiscreteNode}()}, \code{\linkS4class{NeticaNode}},
  \code{\link{NetworkFindNode}()}, \code{\link{NodeTitle}()},
  \code{\linkS4class{NeticaBN}}
}
\examples{
sess <- NeticaSession()
startSession(sess)
net <- CreateNetwork("funNet", session=sess)

pnode <- NewDiscreteNode(net,"play")
nodecached <- pnode

stopifnot(NodeName(pnode)=="play")
stopifnot(NodeName(pnode,internal=TRUE)=="play")
stopifnot(net$findNode("play")==pnode)
stopifnot(net$nodes$play==pnode)

NodeName(pnode)<-"work"
stopifnot(pnode$Name=="work")
stopifnot(is.null(net$findNode("play")))
stopifnot(net$nodes$work==pnode)

stopifnot(NodeName(pnode) == NodeName(nodecached))
stopifnot(NodeName(pnode) == NodeName(nodecached,internal=TRUE))

snode <- NewContinuousNode(net,"sleep")
cat("Next statement should generate an error message.\n")
nn <- try(NodeName(snode)<- "work") ## This should raise an error
stopifnot(is(nn,"try-error"))

allNodes <- NetworkAllNodes(net)
NodeName(allNodes$work) <- "effort"
stopifnot(net$nodes$effort == pnode)

DeleteNetwork(net)
stopSession(sess)

}
\keyword{ interface }
\keyword{ attribute }
