\name{NodeLevels}
\alias{NodeLevels}
\alias{NodeLevels<-}
\title{
  Accesses the levels associated with a Netica node.
}
\description{
  The levels associate a numeric value with the levels of a discrete
  \code{\link{NeticaNode}}, or cut a discrete node into a number
  ordered categories.  This function fetches or retrieves the levels
  for \code{node}.  See description for more details.
}
\usage{
NodeLevels(node)
NodeLevels(node) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{node}{
    A \code{NeticaNode} whose levels are to be accessed.
  }
  \item{value}{
    A numeric vector of values.  For discrete nodes, \code{values}
    should have length \code{\link{NodeNumStates}(node)}.  For
    continuous nodes, it can be of any length (except 1) should be in
    either increasing or decreasing order.
  }
}
\details{
  The behavior of the levels depends on whether the node is discrete
  (\code{\link{is.discrete}(node)==TRUE}) or continuous
  (\code{\link{is.discrete}(node)==TRUE}).

  \bold{Discrete.}  For discrete nodes, the levels are associated with
  the states and provide a numeric summary of the states.  In
  particular, if \code{NodeLevels} are set, then it is meaningful to
  calculate an expected value for the node.  The vector returned by
  \code{NodeLevels()} is named with the names of the states, making
  the association clear.  When setting the \code{NodeLevels}, it
  should have length equal to the number of states
  (\code{NodeNumStates(node)}).

  Note that the first time the \code{NodeLevels()} are set, the entire
  vector must be set.  After that point individual values may be
  changed. 

  \bold{Continuous.} For a continuous node, the levels are used to
  split the continuous range into intervals (similar in spirit to the
  function \code{\link[base]{cut}()}).  The levels represent the
  endpoints of the intervals and should be in either
  increasing or decreasing order.  The values \code{Inf} and
  \code{-Inf} are acceptable for the endpoints of the interval.  There
  should be one more level than the desired number of states.
  
  The states of a continuous node are defined by the node levels, and it
  is not meaningful to try to set \code{NodeStates()},
  \code{\link{NodeStateTitles}()} or \code{\link{NodeStateComments}()}.

  Setting \code{NodeLevels(node)<-NULL} for a continuous node will clear
  the levels and the states.
  
}
\value{
  For discrete nodes, a numeric vector of length \code{NodeNumStates()},
  with names equal to the state names.  If levels have not be set, NAs
  will be returned.

  For continuous nodes, a numeric vector of length
  \code{NodeNumStates()+1} with no names, or \code{character(0)}.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{SetNodeLevels_bn()}, \nref{GetNodeLevels_bn},
  \nref{GetNodeNumberStates_bn}, \nref{GetNodeStateName_bn},
  \nref{SetNodeStateNames_bn}
}
\author{
  Russell Almond
}
\note{
  The overloading of node levels is a "feature" of the Netica API.  It
  is not great design, but it probably will be maintained for backwards
  compatibility. 
}
\seealso{
  \code{\link{NewDiscreteNode}()}, \code{\link{NeticaNode}},
  \code{\link{NodeName}()}, \code{\link{is.discrete}()},
  \code{\link{is.active}()}, \code{\link{NodeStateTitles}()},
  \code{\link{NodeStates}()}, \code{\link{NodeStateComments}()},
}
\examples{
lnet <- CreateNetwork("LeveledNet")

## Discrete Node
vnode <- NewDiscreteNode(lnet,"volt_switch",c("Off","Reverse","Forwards"))
stopifnot(
  length(NodeLevels(vnode))==3,
  names(NodeLevels(vnode)) == NodeStates(vnode),
  all(is.na(NodeLevels(vnode)))
)

\dontrun{
## Don't run this until the levels for vnode have been set,
## it will generate an error.
NodeLevels(vnode)[2] <- 0
}

NodeLevels(vnode) <- 1:3
stopifnot(
  length(NodeLevels(vnode))==3,
  names(NodeLevels(vnode)) == NodeStates(vnode),
  NodeLevels(vnode)[2]==2
)

NodeLevels(vnode)["Reverse"] <- -2

## Continuous Node
wnode <- NewContinuousNode(lnet,"Weight")
stopifnot(
 length(NodeLevels(wnode))==0,
 NodeNumStates(wnode)==0
)

NodeLevels(wnode) <- c(0, 0.1, 10, Inf)
stopifnot(
 length(NodeStates(wnode))==3,
 NodeNumStates(wnode)==3
)
NodeStates(wnode) <- c("Low","Medium","High")
stopifnot(
 NodeStates(wnode)[3] == "High",
 is.null(names(NodeLevels(wnode)))
)
## Change number of states
NodeLevels(wnode) <- c(0, 0.1, 10, 100, Inf)
stopifnot(
 length(NodeStates(wnode))==4,
 NodeNumStates(wnode)==4,
 all(nchar(NodeStates(wnode))==0)
)
## Clear levels
NodeLevels(wnode) <- c()
stopifnot(
 NodeNumStates(wnode)==0,
 length(NodeStates(wnode))==0
)

DeleteNetwork(lnet)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ interface }
\keyword{ attribute }% __ONLY ONE__ keyword per line
