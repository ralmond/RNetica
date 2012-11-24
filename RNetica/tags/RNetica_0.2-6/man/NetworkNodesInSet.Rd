\name{NetworkNodesInSet}
\alias{NetworkNodesInSet}
\title{
  Returns a list of node labeled with the given node set in a Netica
  Network. 
}
\description{
  A node set is a character label associated with a node which provides
  information about its role in the models.  This function returns a
  list of all nodes labeled with a particular node set.
}
\usage{
NetworkNodesInSet(net, setname)
}
\arguments{
  \item{net}{
    An active \code{\link{NeticaBN}} object representing the network.
  }
  \item{setname}{
    A character scalar giving the node set to look for.
  }
}
\details{
  Netica node sets are a collection of string labels that can be
  associated with various nodes in a network using the function
  \code{\link{NodeSets}()}.  Node sets do not have any meaning to
  Netica:  node set membership only affect the way the node is displayed
  (see \code{\link{NetworkNodeSetColor}()}).  One purpose of node sets
  is to label a set of nodes that play a similar role in the model.  For
  example, \code{"ReportingVariable"} or \code{"Observable"}.

  The expression \code{NetworkNodesInSet(net,setname)} searches through
  the network for all nodes labeled with the given setname.  It returns
  a list of such nodes.  This value cannot be set directly, only
  indirectly through the use of \code{NodeSets}, or through the use of
  system primitives.

  Note that it is acceptable to use the system built-ins.  For examble
  searching for \code{":TableIncomplete"} will return a collection of
  nodes for which the conditional probability table has not yet been
  set.

}
\value{
  A list of nodes which are associated with the named node set.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLurl/Manual/index.html}:
  \nref{GetAllNodesets_bn}, \nref{IsNodeInNodeset_bn}

}
\author{
  Russell Almond
}
\seealso{
  \code{\link{NeticaBN}}, \code{\link{NodeSets}()},
  \code{\link{NetworkSetPriority}()}, 
  \code{\link{NetworkNodesInSet}()}, \code{\link{NetworkNodeSetColor}()}
}
\examples{

nsnet <- CreateNetwork("NodeSetExample")

Ability <- NewContinuousNode(nsnet,"Ability")

X1 <- NewDiscreteNode(nsnet,"Item1",c("Right","Wrong"))

EssayScore <- NewDiscreteNode(nsnet,"EssayScore",paste("level",5:0,sep="_"))

Value <- NewContinuousNode(nsnet,"Value")
NodeKind(Value) <- "Utility"
Placement <- NewDiscreteNode(nsnet,"Placement",
     c("Advanced","Regular","Remedial"))
NodeKind(Placement) <- "Decision"

NodeSets(Ability) <- "ReportingVariable"
NodeSets(X1) <- "Observable"
NodeSets(EssayScore) <- c("ReportingVariable","Observable")

## setequal doesn't deal well with arbitrary objects, so
## just use the names.
nodeseteq <- function(x,y) {
  setequal(as.character(x),as.character(y))
}

stopifnot(
  nodeseteq(NetworkNodesInSet(nsnet,"ReportingVariable"),
           list(Ability,EssayScore)),
  nodeseteq(NetworkNodesInSet(nsnet,"Observable"),
           list(X1,EssayScore)),
  nodeseteq(NetworkNodesInSet(nsnet,"Observables"),
           list()),
  nodeseteq(NetworkNodesInSet(nsnet,":Nature"),
           list(Ability,EssayScore,X1)),
  nodeseteq(NetworkNodesInSet(nsnet,":Decision"),
           list(Placement)),
  nodeseteq(NetworkNodesInSet(nsnet,":Utility"),
           list(Value))
)

DeleteNetwork(nsnet)


}
\keyword{ interface }
\keyword{ attribute }% __ONLY ONE__ keyword per line
