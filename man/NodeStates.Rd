\name{NodeStates}
\alias{NodeStates}
\alias{NodeStates<-}
\alias{NodeNumStates}
\title{ Accessor for states of a Netica node.
}
\description{
  This function returns a list associated with a Netica node.  The
  function \code{NodeNumStates()} returns the number of states,
  \code{NodeStates} returns or manipulates them.
}
\usage{
NodeStates(node)
NodeNumStates(node)
NodeStates(node) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{node}{
    An active \code{\link{NeticaNode}} object whose states are to be accessed.
  }
  \item{value}{
    A character vector of length \code{NodeNumStates(node)} giving the
    names of the states.  State names must conform to the
    \code{\link{IDname}} rules.
  }
}
\details{
  States behave slightly differently for discrete and continuous nodes
  (see \code{is.discrete()}.  For discrete nodes, the random variable
  represented by the node can take on one of the values represented by
  \code{NodeStates(node)}.

  The number of states for a discrete node is determined when the node
  is created (through a call to \code{\link{NewDiscreteNode}()}).  The
  number of states may not be changed, but they can be renamed.

  The states are important when building conditional probability
  tables (CPTs).  In particular, the state names are used to label the columns
  of the CPT.  Thus, state names can be used to address arrays in the
  same way that \code{\link[base]{dimnames}} can.  In particular, the
  state names can be used to index the vectors returned by
  \code{\link{NodeStates}()}, \code{\link{NodeStateTitles}()},
  \code{\link{NodeStateTitles}()}, and \code{\link{NodeLevels}()} (for
  discrete nodes).

  
}
\value{
  The function \code{NodeNumStates()} returns an integer giving the
  number of states.

  The function \code{NodeStates()} returns a character vector of length
  \code{NodeNumStates(node)} whose values and names are both set to the
  state names.  The setter version of this function invisibly returns the
  \code{node} object.
}
\references{
  \link{http://norsys.com/onLineAPI/Manual/index.html}:
  \code{GetNodeNumberStates_bn()}, \code{GetNodeStateName_bn()},
  \code{SetNodeStateNames_bn()},  \code{GetNodeLevels_bn()} 
  \code{SetNodeLevels_bn()} 
}
\author{
  Russell Almond
}
\seealso{
  \code{\link{NewDiscreteNode}()}, \code{\link{NeticaNode}},
  \code{\link{NodeName}()}, \code{\link{is.discrete}()},
  \code{\link{is.active}()}, \code{\link{NodeStateTitles}()},
  \code{\link{NodeLevels}()}, \code{\link{NodeStateComments}()},
}
\examples{
anet <- CreateNetwork("Annette")

nodel2 <- NewDiscreteNode(anet,"TwoLevelNode")
stopifnot(
  NodeNumStates(nodel2)==2,
  NodeStates(nodel2)==c("Yes","No")
)

NodeStates(nodel2) <- c("True","False")
stopifnot(
  NodeStates(nodel2)==c("True","False")
)


nodel3 <- NewDiscreteNode(anet,"ThreeLevelNode",c("High","Med","Low"))
stopifnot(
  NodeNumStates(nodel3)==3,
  NodeStates(nodel3)==c("High","Med","Low"),
  NodeStates(nodel3)[2]=="Med"
)

NodeStates(nodel3)[2] <- "Median"
stopifnot(
  NodeStates(nodel3)[2]=="Median"
)

NodeStates(nodel3)["Median"] <- "Medium"
stopifnot(
  NodeStates(nodel3)[2]=="Medium"
)

DeleteNetwork(anet)

}
\keyword{ interface }
\keyword{ model }% __ONLY ONE__ keyword per line
