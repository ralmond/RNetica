\name{NetworkNodeSets}
\alias{NetworkNodeSets}
\title{
  Returns a list of node sets associated with a Netica network.
}
\description{
  A node set is a character label associated with a node which provides
  information about its role in the models.  This function returns the
  complete list of node sets associated with any node in the network.
}
\usage{
NetworkNodeSets(net, incSystem = FALSE)
}
\arguments{
  \item{net}{
    An active \code{\link{NeticaBN}} object representing the network.
  }
  \item{incSystem}{
    A logical flag.  If \code{TRUE} then built-in Netica node sets are
    returned as well as the user defined ones.
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

  The expression \code{NetworkNodeSets(node)} returns the node sets that
  are currently associated with any \code{node}.  If
  \code{incSystem=TRUE}, then the internal Netica system node sets will
  be included as well.  These begin with a colon (\sQuote{:}).  This
  value cannot be set directly, only indirectly through the use of
  \code{NodeSets}.
}
\value{
  A character vector giving the node sets used by the network.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLurl/Manual/index.html}:
  \nref{GetAllNodesets_bn}
}
\author{
  Russell Almond
}
\note{
  Node sets cannot be destroyed, only created.  An empty node set has no
  effect. 
}
\seealso{
  \code{\link{NeticaNode}}, \code{\link{NodeSets}()},
  \code{\link{NetworkSetPriority}()}, 
  \code{\link{NetworkNodesInSet}()}, \code{\link{NetworkNodeSetColor}()}
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
  length(NetworkNodeSets(nsnet)) == 0, ## Nothing set yet
  length(NetworkNodeSets(nsnet,TRUE)) == 22 ## Number of system states
)

NodeSets(Ability) <- "ReportingVariable"
stopifnot(
  NetworkNodeSets(nsnet) == "ReportingVariable"
)
NodeSets(EssayScore) <- "Observable"
stopifnot(
  setequal(NetworkNodeSets(nsnet),c("Observable","ReportingVariable"))
)
## Changing spelling of name adds new set, doesn't delete the old one.
NodeSets(EssayScore) <- "Observables"
stopifnot(
  setequal(NetworkNodeSets(nsnet),
           c("Observables", "Observable","ReportingVariable"))
)
## Nor does deletion
NodeSets(Ability) <- character()
stopifnot(
  setequal(NetworkNodeSets(nsnet),
           c("Observables", "Observable","ReportingVariable"))
)

DeleteNetwork(nsnet)
}
\keyword{ attributes }
\keyword{ interface }% __ONLY ONE__ keyword per line
