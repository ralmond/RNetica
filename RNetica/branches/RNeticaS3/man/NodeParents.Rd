\name{NodeParents}
\alias{NodeParents}
\alias{NodeParents<-}
\title{
  Gets or sets the parents of a node in a Netica network.
}
\description{
  A parent of a \code{\link{NeticaNode}} is another node which has a
  link (created through \code{\link{AddLink}()} from that node to
  \code{child}.  This function returns the list of parents.  It also
  allows the list of parents for the node to be set, altering the
  topology of the network (see details).
}
\usage{
NodeParents(child)
NodeParents(child) <- value
}
\arguments{
  \item{child}{
    An active  \code{NeticaNode} object whose parents are of interest.
  }
  \item{value}{
    A list of \code{NeticaNode} objects (or \code{NULL}s) which will
    become the new parents.  Order of the nodes is important.  See details.
  }
}
\details{
  At its most basic level, \code{NodeParents()} reports on the topology
  of a network.  Suppose we add the links \code{A1 --> B},
  \code{A2 --> B},  and \code{A3 --> B} to the network.  Then
  \code{NodeParents(B)} should return \code{list(A1, A2, A3)}.  The
  order of the inputs is important, because that this determines the
  order of the dimensions in the conditional probability table
  (\code{\link{NodeProbs}()}).

  The parent list can be set.  This can accomplishes a number of different
  goals: it can replace a parent variable, it can add additional
  parents, it can remove extra parents, and it can reorder parents.
  Changing the parents alters the topology of the network.  Note that
  Netica networks must always be acyclic directed graphs.  In
  particular, if \code{\link{is.NodeRelated}(child,"decendent",parent)}
  returns true for any prospective parent, Netica will generate an
  error (new parents must node be descendants of the child as that would
  produce a cycle).

  Setting an element of the parent list to \code{list(NULL)} has special
  semantics.  In this case, the parent node becomes a special \emph{stub
  node} (or DISCONNECTED_TYPE, see \code{\link{NodeKind}()}).  This
  creates a Bayesian network fragment which can later be connected to
  another Bayesian network (using \code{SetParents()} with the new
  parent.

  The function \code{\link{NodeInputNames}(child)}, returns a list of
  names for the parent variables.  Naming the parent variables
  facilitates disconnecting the node and reconnecting it.  Whenever a
  node is disconnected, the corresponding input is named after the
  disconnected node, unless it already has an input name.

}
\value{
  A list of \code{NeticaNode} objects representing the parents in the
  order that they will be used to establish dimensions for the
  conditional probability table.  If \code{NodeInputNames(child)} has
  been set, the names of the result will be the input names.

  The setting variant returns the modified \code{child} object.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GetNodeParents_bn}, \nref{SwitchNodeParent_bn}
}
\author{
  Russell Almond
}
\note{
  Much of the checking for this function is done internally in the
  Netica API, and not in the RNetica interface layer.  In particular,
  creating directed cycles will produce errors in Netica and not in
  RNetica.

  This is actually an attempt to make the RNetica interface more R-like,
  covering the common cases of \code{NodeParents(child) <- value}.
  Under the hood it is using the Netica function
  \code{SwitchNodeParent_bn()} to produce the expected behavior.

  The fact that if \code{x} is a list \code{x[[2]]<-NULL} deletes the
  second element rather than replacing it with \code{NULL} is a serious
  design flaw in R.  However, it is documented in the FAQ and it is
  unlikely to change, so we need to workaround it.  We do this by
  setting the element we want to delete to \code{list(NULL)}.
  Nominally, we would do this through \code{x[2]<-list(NULL)}, which
  is the official workaround for the design flaw.  \code{NodeParents<-}
  will accept \code{list(NULL)} in place of \code{NULL} because nobody
  who isn't part of the R Core Development Team will ever remember which
  form they are suppose to use here.
}
\seealso{
  \code{\link{NeticaNode}}, \code{\link{AddLink}()},
  \code{\link{NodeChildren}()}, \code{\link{NodeKind}()},
  \code{\link{NodeInputNames}()}, \code{\link{is.NodeRelated}()}
}
\examples{
abnet <- CreateNetwork("AB")

anodes <- NewDiscreteNode(abnet, paste("A",1:3,sep=""))
B <- NewDiscreteNode(abnet,"B")

## Should be empty list
stopifnot(length(NodeParents(B))==0)

NodeParents(B) <- anodes
stopifnot(
  length(NodeParents(B))==3,
  NodeParents(B)[[2]] == anodes[[2]]
)

## Reorder nodes
NodeParents(B) <- anodes[c(2:3,1)]
stopifnot(
  length(NodeParents(B))==3,
  NodeName(NodeParents(B)[[2]])=="A3",
  all(nchar(names(NodeParents(B)))==0)
)

## Remove a node.
NodeParents(B) <- anodes[2:1]
stopifnot(
  length(NodeParents(B))==2,
  NodeName(NodeParents(B)[[2]])=="A1",
  all(nchar(names(NodeParents(B)))==0)
)

## Add a node
NodeParents(B) <- anodes[3:1]
stopifnot(
  length(NodeParents(B))==3,
  NodeName(NodeParents(B)[[3]])=="A1",
  all(nchar(names(NodeParents(B)))==0)
)

##Name the inputs
NodeInputNames(B) <- paste("Input",1:3,sep="")
stopifnot(
  names(NodeParents(B))[2]=="Input2"
)

## Detach the parent
NodeParents(B)$Input2 <- list(NULL)
stopifnot(
  length(NodeParents(B))==3,
  NodeKind(NodeParents(B)$Input2) == "Stub"
)

## Remove all parents
NodeParents(B) <- list()
stopifnot(
  length(NodeParents(B))==0
)

DeleteNetwork(abnet)
}
\keyword{ interface }
\keyword{ graphs }
