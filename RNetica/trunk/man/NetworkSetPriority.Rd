\name{NetworkSetPriority}
\alias{NetworkSetPriority}
\title{
  Changes the priority order of the node sets.
}
\description{
  Netica sets the visual appearance (i.e., colour, see
  \code{\link{NetworkNodeSetColor}()}) of a node according to highest
  priority set to which the node belongs.  This function changes the
  order of priority.
}
\usage{
NetworkSetPriority(net, setlist)
}
\arguments{
  \item{net}{
    An active \code{\link{NeticaBN}} object representing the network.
  }
  \item{setlist}{
    A character vector containing a subset of the node set names.  The
    first ones in the sequence will have the highest priority.
  }
}
\details{
  Netica determines the visual style of a node by stepping through the
  node sets to which the node belongs in priority order.  Each node set
  can either have a colour set, or a flag set to indicate that the next
  node in order or priority should be used to determine the appearance
  of the node (see \code{\link{NetworkNodeSetColor}()}).

  This function switches the priority of the node sets names in the
  second argument.  The node sets note mentioned in \code{setlist} are
  not affected.
  
}
\value{
  Returns the \code{net} argument invisibly.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLurl/Manual/index.html}:
  \nref{ReorderNodesets_bn}, \nref{SetNodesetColor_bn}
}
\author{
  Russell Almond
}
\note{
  The priority of the Netica internal node sets (the ones beginning with
  \sQuote{:}) are set by Netica and cannot be changed.  They all have
  lower priority than the user-defined node sets.
}
\seealso{
  \code{\link{NeticaNode}}, \code{\link{NodeSets}()},
  \code{\link{NetworkNodeSets}()}, 
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

NodeSets(EssayScore) <- c("ReportingVariable","Observable")

NetworkSetPriority(nsnet,c("Observable","ReportingVariable"))
## Now EssayScore should be coloured like an observable.
stopifnot( NodeSets(EssayScore) == c("Observable","ReportingVariable"))

NetworkSetPriority(nsnet,c("ReportingVariable","Observable"))
## Now EssayScore should be coloured like a Reporting Variable
stopifnot( NodeSets(EssayScore) == c("ReportingVariable","Observable"))

DeleteNetwork(nsnet)

}
\keyword{ interface }
\keyword{ attribute }% __ONLY ONE__ keyword per line
