\name{NodeSets}
\alias{NodeSets}
\alias{NodeSets<-}
\alias{AddNodeToSets}
\alias{RemoveNodeFromSets}
\title{
  Lists or changes the node sets associated with a Netica node.
}
\description{
  A node set is a character label associated with a node which provides
  information about its role in the models.  This function returns or
  sets the labels associated with a node.
}
\usage{
NodeSets(node, incSystem = FALSE)
NodeSets(node) <- value
AddNodeToSets(node,sets)
RemoveNodeFromSets(node,sets)
}
\arguments{
  \item{node}{
    An active \code{\link{NeticaNode}} object.
  }
  \item{incSystem}{
    A logical flag.  If \code{TRUE} then built-in Netica node sets are
    returned as well as the user defined ones.
  }
  \item{value}{
    A character vector containing the names of the node sets that
    \code{node} should be associated with.  These names must follow the
    \code{\link{is.IDname}()} rules.
  }
  \item{sets}{
    A character vector containing the names of the node sets that
    \code{node} should (or should not) be associated with.  These names
    must follow the \code{\link{is.IDname}()} rules.
  }
}
\details{
  Netica node sets are a collection of string labels that can be
  associated with various nodes in a network.  Node sets do not have any
  meaning to Netica:  node set membership only affect the way the node
  is displayed (see \code{\link{NetworkNodeSetColor}()}).  One 
  purpose of node sets is to label a set of nodes that play a similar
  role in the model.  For example, \code{"ReportingVariable"} or
  \code{"Observable"}.

  The expression \code{NodeSet(node)} returns the node sets currently
  associated with \code{node}.  If \code{incSystem=TRUE}, then the
  internal Netica system node sets will be included as well.  These
  begin with a colon (\sQuote{:}).

  The expression \code{NodeSet(node)<-value} removes any node sets
  previously associated with \code{node} and adds node to the node sets
  named in \code{value}.  The elements of \code{value} need not 
  correspond to existing node sets, new node sets will be created for
  new values.  (Warning:  this implies that if the name of the node set
  is spelled incorrectly in one of the calls, this will create a new
  node set.  For example, \code{"Observable"} and \code{"Observables"}
  would be two distinct node sets.)  Setting the node set associated
  with a node only affects user-defined node sets, the Netica system
  node sets cannot be set using \code{NodeSet}.

  The functions \code{AddNodeToSets} and \code{RemoveNodeFromSets}
  operate on the current node set in the expected way.
  
}
\value{
  A character vector giving the names of the node sets \code{node} is
  associated with.  The setter form, \code{AddNodeToSets} and
  \code{RemoveNodeFromSets} return \code{node}.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{AddNodeToNodeset_bn}, \nref{RemoveNodeFromNodeset_bn},
  \nref{IsNodeInNodeset_bn}
}
\author{
  Russell Almond
}
\seealso{
  \code{\link{NeticaNode}}, \code{\link{NodeKind}()},
  \code{\link{NetworkNodeSets}()}, \code{\link{NetworkSetPriority}()}, 
  \code{\link{NetworkNodesInSet}()}, \code{\link{NetworkNodeSetColor}()}, 
  \code{\link{is.IDname}()}
}
\examples{

nsnet <- CreateNetwork("NodeSetExample")

Ability <- NewContinuousNode(nsnet,"Ability")

EssayScore <- NewDiscreteNode(nsnet,"EssayScore",paste("level",5:0,sep="_"))

Value <- NewContinuousNode(nsnet,"Value")
NodeKind(Value) <- "Utility"
Placement <- NewDiscreteNode(nsnet,"Placement",
     c("Advanced","Regular","Remedial"))
NodeKind(Placement) <- "Decision"

stopifnot(
  length(NodeSets(Ability)) == 0, ## Nothing set yet
  setequal(NodeSets(Ability,TRUE),
           c(":Continuous", ":Nature", ":TableIncomplete",
             ":Parentless", ":Childless", ":Node")),
  !is.na(match(":Utility",NodeSets(Value,TRUE))),
  !is.na(match(":Decision",NodeSets(Placement,TRUE)))
)

NodeSets(Ability) <- "ReportingVariable"
stopifnot(
  NodeSets(Ability) == "ReportingVariable"
)
NodeSets(EssayScore) <- "Observable"
stopifnot(
  NodeSets(EssayScore) == "Observable"
)
## Make EssayScore a reporting variable, too
NodeSets(EssayScore) <- c("ReportingVariable",NodeSets(EssayScore))
stopifnot(
  setequal(NodeSets(EssayScore),c("Observable","ReportingVariable"))
)

## Clear out the node set
NodeSets(Ability) <- character()
stopifnot(
  length(NodeSets(Ability)) == 0
)

NodeSets(Ability) <- c("Set1","Set2")
AddNodeToSets(Ability,c("Set2","Set3"))
stopifnot(
  length(NodeSets(Ability)) == 3,
  setequal(NodeSets(Ability),c("Set1","Set2","Set3"))
)
RemoveNodeFromSets(Ability,c("Set1","Set4"))
stopifnot(
  length(NodeSets(Ability)) == 2,
  setequal(NodeSets(Ability),c("Set2","Set3"))
)


DeleteNetwork(nsnet)

}
\keyword{ interface }
\keyword{ attributes }% __ONLY ONE__ keyword per line
