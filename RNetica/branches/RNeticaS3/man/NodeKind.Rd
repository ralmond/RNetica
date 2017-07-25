\name{NodeKind}
\alias{NodeKind}
\alias{NodeKind<-}
\title{
  Gets or changes the kind of a node in a Netica network.
}
\description{
  Netica supports nodes of four different kinds:  \code{"Nature"},
  \code{"Decision"}, \code{"Utility"}, and \code{"Constant"}.  A fifth
  kind, \code{"Stub"} is used for a reference to a node when an edge has
  been detached from a node.  The function \code{NodeKind()} returns the
  current kind.
}
\usage{
NodeKind(node)
NodeKind(node) <- value
}
\arguments{
  \item{node}{
    A \code{\link{NeticaNode}} object whose kind is to be determined or
    manipulated. 
  }
  \item{value}{
    A character string with one of the values: \code{"Nature"},
  \code{"Decision"}, \code{"Utility"}, or \code{"Constant"}.  Actually,
  only the first letter is matched, so this could be one of N, D, U or
  C. 
  }
}
\details{
  A \code{"Nature"} node (the default when the node is created) is a
  random variable whose value can be predicted using the network.  Pure
  Bayesian networks use only \code{"Nature"} nodes.

  A \code{"Decision"} node is one whose value will be chosen by some
  decision maker.  A \code{"Utility"} node is one whose value the
  decision maker is trying to optimize.  A influence diagram contains
  decision nodes and utilities in addition to nature nodes.  The goal is
  implicitly to find a setting of the decision nodes that maximizes the
  expected utility.

  A \code{"Constant"} node is a parameter used for building a
  conditional probability table.  Its value is nominally fixed, but it
  can be changed to perform sensitivity analysis.

  A \code{"Stub"} is a reference to a node created by removing a parent
  node from another node without changing the table.  It is assumed that
  a real node will later be attached in that location.  This kind can
  only be set internally to Netica; the expression
  \code{NodeKind(node) <- "Stub"} will generate an error.
}
\value{
  A character vector of length one containing one of the values:
  \code{"Nature"}, \code{"Decision"}, \code{"Utility"},
  \code{"Constant"}, or \code{"Stub"}.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GetNodeKind_bn}, \nref{SetNodeKind_bn}
}
\author{
  Russell Almond
}
\note{
  Internal to Netica, \code{"Stub"}s are called
  \code{DISCONNECTED_NODE}s.  I changed the name to make them start with
  a unique letter.
}
\seealso{
  \code{\link{NeticaNode}}, \code{\link{is.discrete}()},
  \code{\link{NodeParents}()} 
}
\examples{
knet <- CreateNetwork("kNet")

skills <- NewContinuousNode(knet,paste("SkillAtTime",1:2,sep=""))

reward <- NewContinuousNode(knet,"RewardForSkill")
NodeKind(reward) <- "Utility"

placement <-
  NewDiscreteNode(knet,"Placement",c("Tier1","Tier2","Tier3"))
NodeKind(placement) <- "Decision"

instructionCost <- NewContinuousNode(knet,"CostOfInstruction")
NodeKind(instructionCost) <- "U"

pretest <- NewDiscreteNode(knet,"PretestDecision",c("yes","no"))
NodeKind(pretest) <- "D"

pretestScore <- NewContinuousNode(knet,"PretestScore")
NodeKind(pretestScore) <- "Nature"

pretestCost <- NewContinuousNode(knet,"PretestCost")
NodeKind(pretestCost) <- "u"

pretestR <- NewContinuousNode(knet,"PretestReliability")
NodeKind(pretestR) <- "Constant"

stopifnot(
 NodeKind(skills[[1]]) == "Nature",
 NodeKind(skills[[2]]) == "Nature",
 NodeKind(reward) == "Utility",
 NodeKind(placement) == "Decision",
 NodeKind(instructionCost) == "Utility",
 NodeKind(pretest) == "Decision",
 NodeKind(pretestScore) == "Nature",
 NodeKind(pretestCost) == "Utility",
 NodeKind(pretestR) == "Constant"
 )

## To make stub node, need links
AddLink(skills[[1]],pretestScore)
NodeInputNames(pretestScore) <- "SkillTested"
## Detach node
NodeParents(pretestScore)$SkillTested <- list(NULL)
stopifnot(
  NodeKind(NodeParents(pretestScore)$SkillTested) == "Stub"
)

DeleteNetwork(knet)
}
\keyword{ interface }
\keyword{ attribute }% __ONLY ONE__ keyword per line
