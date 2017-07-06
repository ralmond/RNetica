\name{NetworkNodesInSet}
\alias{NetworkNodesInSet}
\alias{NetworkNodesInSet<-}
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
NetworkNodesInSet(net, setname) <- value
}
\arguments{
  \item{net}{
    An active \code{\link{NeticaBN}} object representing the network.
  }
  \item{setname}{
    A character scalar giving the node set to look for.
  }
  \item{value}{
    A list of active \code{\link{NeticaNode}} objects which should be in
    the node set.
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

  The expression \code{NetworkNodesInSet(net,setname) <-value} make sure
  that \code{setname} is in the node sets of all nodes that are in
  \code{value} and that it is not in the node sets of any node that is
  not in \code{value}.

  Note that it is acceptable to use the system built-ins in the getter
  method (but not the setter).  For example
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
\note{
  The \code{\link[base]{c}()} function strips attributes by default.
  This means that the obvious expression for adding a node to a node
  set:

  \code{NetworkNodesInSet(Net,Set) <-
    c(NetworkNodesInSet(Net,Set),NewNode)}

  will give an error message (claiming that a string with the name of
  \code{NewNode} is not a proper node).  The problem is with an
  ill-considered side effect of the \code{c()} function.  You can work
  around this by using the expression:

  \code{NetworkNodesInSet(Net,Set) <-
    c(NetworkNodesInSet(Net,Set),list(NewNode))}
  

}
\seealso{
  \code{\link{NeticaBN}}, \code{\link{NodeSets}()},
  \code{\link{NetworkSetPriority}()}, 
  \code{\link{NetworkNodesInSet}()}, \code{\link{NetworkNodeSetColor}()}
}
\examples{

nsnet <- CreateNetwork("NodeSetExample")

Ability <- NewContinuousNode(nsnet,"Ability")

XX <- NewDiscreteNode(nsnet,paste("Item",1:5,sep=""),c("Right","Wrong"))
X1 <- XX[[1]]

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
           c(list(Ability,EssayScore),XX)),
  nodeseteq(NetworkNodesInSet(nsnet,":Decision"),
           list(Placement)),
  nodeseteq(NetworkNodesInSet(nsnet,":Utility"),
           list(Value))
)

NetworkNodesInSet(nsnet,"TestSet") <- XX[1:3]
stopifnot(
  is.element("TestSet",NodeSets(XX[[1]])),
  is.element("TestSet",NodeSets(XX[[2]])),
  is.element("TestSet",NodeSets(XX[[3]])),
  !is.element("TestSet",NodeSets(XX[[4]])),
  !is.element("TestSet",NodeSets(XX[[5]]))
)
NetworkNodesInSet(nsnet,"TestSet") <- XX[2:4]
stopifnot(
  !is.element("TestSet",NodeSets(XX[[1]])),
  is.element("TestSet",NodeSets(XX[[2]])),
  is.element("TestSet",NodeSets(XX[[3]])),
  is.element("TestSet",NodeSets(XX[[4]])),
  !is.element("TestSet",NodeSets(XX[[5]]))
)



DeleteNetwork(nsnet)


}
\keyword{ interface }
\keyword{ attribute }
