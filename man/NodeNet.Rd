\name{NodeNet}
\alias{NodeNet}
\title{

  Finds which Netica network a node comes from.

}
\description{

  Each active \code{\linkS4class{NeticaNode}} object lives inside of a
  \code{\link{NeticaBN}} object.  This function finds the network
  corresponding to a node.

}
\usage{
NodeNet(node, internal=FALSE)
}
\arguments{
  \item{node}{
    A \code{\linkS4class{NeticaNode}} object.
  }
  \item{internal}{A logical scalar.  If true, the actual Netica object
    will be consulted, if false, a cached value in the R object will be
    used.}
}
\details{

  Two nodes with the same details in different networks are not
  identical inside of Netica.  Nodes are always constructed inside of
  nets, and the \code{Net} field of a node cannot be changed.  (See
  \code{\link{CopyNodes}} for copying a node to a new network.)

  Starting with RNetica version 0.5, a \code{\linkS4class{NeticaNode}}
  object can figure out its network in two different ways.  First the
  field \code{\var{node}$Net} has the \code{\linkS4class{NeticaBN}}
  object associated with this node.  The second is by going into the
  Netica node object, finding the corresponding network and then looking
  it up by name in the \code{\linkS4class{NeticaSession}} object.  With
  the option \code{internal=TRUE} this is what is done to check the
  node.

  The node must be active.  If   \code{\link{is.active}(\var{node})}
  returns false, this function will return \code{NULL}.  Note that the
  expression \code{\var{node}$Net} will return the (possible inactive)
  \code{\link{NeticaBN}} object that the node used to belong to.

  The functions \code{\link{NetworkAllNodes}()} and
  \code{\link{NetworkFindNode}()} provide pseudo-inverses for this
  function. 

}
\value{
  A \code{\linkS4class{NeticaBN}} object which contains \code{node}, or
  \code{NULL} if \code{node} is not active and the internal method was
  selected. 
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
    \url{http://norsys.com/onLineAPIManual/index.html}: \nref{GetNodeNet_bn}
}
\author{
  Russell Almond
}
\seealso{
  \code{\linkS4class{NeticaBN}}, \code{\linkS4class{NeticaNode}},
  \code{\link{is.active}()}, \code{\link{NetworkAllNodes}()},
  \code{\link{NetworkFindNode}()}
}
\examples{
sess <- NeticaSession()
startSession(sess)
neta <- CreateNetwork("Net_A", session=sess)
netb <- CreateNetwork("Net_B", session=sess)

nodea <- NewContinuousNode(neta,"Node")
nodeb <- NewContinuousNode(netb,"Node")

stopifnot(NodeNet(nodea)==neta)
stopifnot(NodeNet(nodeb)==netb)

stopifnot(NodeNet(nodea)==NodeNet(nodea,internal=TRUE))

## Note
stopifnot(nodea != nodeb)
## But:
stopifnot(nodea$Name == nodeb$Name)

DeleteNodes(nodeb)
stopifnot(is.null(NodeNet(nodeb)))
stopifnot(nodeb$Net==netb)

DeleteNodes(nodea)

DeleteNetwork(list(neta,netb))
stopSession(sess)

}
\keyword{ interface }
\keyword{ graphs }
