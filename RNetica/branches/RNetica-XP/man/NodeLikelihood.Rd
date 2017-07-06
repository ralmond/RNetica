\name{NodeLikelihood}
\alias{NodeLikelihood}
\alias{NodeLikelihood<-}
\title{
  Returns or sets the virtual evidence associated with a Netica node.
}
\description{
  The findings associated with a node can be expressed as the
  probability of the evidence occurring in each of the states of the
  node.  This is the \emph{likelihood} associated with the node.  This
  function retrieves or sets the likelihood.
}
\usage{
NodeLikelihood(node)
NodeLikelihood(node) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{node}{
    An active \code{\link{NeticaNode}} whose evidence is to be
    queried or set.
  }
  \item{value}{
    A numeric vector of length \code{\link{NodeNumStates}(node)}
    representing the new likelihood for the node.  All values must be
    between zero and one and there must be at least one positive value,
    but the sum does not need to equal 1.
  }
}
\details{
  This function retrieves or sets virtual evidence associated with each
  node.  Suppose that some set of evidence \eqn{e} is observed.  The
  each of the values in the likelihood represents the conditional
  probability \eqn{Pr(e|node==state)}.  Note that the likelihood can be
  thought of as the message that a new node \code{child} which was a
  child of \code{node} with no other parents would pass to \code{node}
  if its value was set.

  As the likelihood values are conditional probabilities, they do not
  need to add to 1, although they are still restricted to the range
  [0,1].  Also, at least one value must be non-zero (or else the
  evidence represents an impossible case) or Netica will generate an
  error.

  Entering findings through \code{\link{NodeFinding}(node,state)} sets
  a special likelihood.  In this case, the likelihood value
  corresponding to \code{state} will be one, and all others will be zero.
  Similarly, the expression \code{\link{EnterNegativeFinding}(node,statelist)} sets a
  special likelihood with 0's corresponding to the states in
  \code{statelist} and 1's elsewhere.

  Setting the likelihood calls \code{\link{RetractNodeFinding}()},
  clearing any previous finding, negative finding or likelihood.
  
}
\value{
  The function \code{NodeLikelihood(node)} returns a vector of
  likelihoods of length \code{\link{NodeNumStates}(node)}.  The names
  of the result are the state names.

  The expression \code{NodeLikelihood(node)<-value} returns the modified
  node invisibly.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GetNodeLikelihood_bn}, \nref{EnterNodeLikelihood_bn}
}
\author{
  Russell Almond
}
\note{
  If \code{\link{SetNetworkAutoUpdate}()} has been set to \code{TRUE},
  then setting the likelihood could take some time as each finding is
  individually propagated.  Consider wrapping multiple calls setting
  \code{NodeLikelihood()} in \code{WithoutAutoUpdate(net, ...)}.

  Unlike the Netica function \code{EnterNodeLikelihood_bn()} the function
  \code{"NodFinding<-"} internally calls \code{RetractFindings}.  So
  there is no need to do this manually.
}
\section{Warning}{

  The documentation for the Netica function \nref{MostProbableConfig_bn}
  states that likelihood findings are not
  properly taken into account in \code{\link{MostProbableConfig}()}.
  Some quick tests indicate that it is doing something sensible, but
  more extensive testing and/or clarification is needed.

  The documentation for the Netica function
  \nref{FindingsProbability_bn} also provides a warning about likelihood
  evidence.  The function \code{\link{FindingsProbability}(net)} still
  gives a result, but it is the normalization constant for the network,
  and not necessarily a probability.

}
\seealso{
  \code{\link{NeticaBN}}, \code{\link{NodeBeliefs}()},
  \code{\link{EnterNegativeFinding}()},
  \code{\link{RetractNodeFinding}()}, \code{\link{NodeFinding}()}
  \code{\link{JointProbability}()},
  \code{\link{MostProbableConfig}()}, \code{\link{FindingsProbability}()}}
\examples{
irt5 <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","IRT5.dne",
                           sep=.Platform$file.sep))

irt5.theta <- NetworkFindNode(irt5,"Theta")
irt5.x <- NetworkFindNode(irt5,paste("Item",1:5,sep="_"))

CompileNetwork(irt5) ## Ready to enter findings

## Simple finding
NodeFinding(irt5.x[[1]])<-"Wrong"
stopifnot(
  NodeLikelihood(irt5.x[[1]]) == c(0,1)
)

## Negative finding
EnterNegativeFinding(irt5.theta,c("neg1","neg2")) ## Rule out negatives.
stopifnot(
  NodeLikelihood(irt5.x[[1]]) == c(0,1),
  NodeLikelihood(irt5.theta) == c(1,1,1,0,0),
  NodeFinding(irt5.theta) == "@NEGATIVE FINDINGS"
)

## Clearing Findings
RetractNodeFinding(irt5.theta)
stopifnot(
  NodeLikelihood(irt5.theta) == c(1,1,1,1,1)
)

##Virtual findings for X3.  Assume judge has said right, but judge has
##  80\% accuracy rate.
NodeLikelihood(irt5.x[[3]]) <- c(.8,.2)
stopifnot(
  sum(abs(NodeLikelihood(irt5.x[[3]]) - c(.8,.2))) < 1e-6,
  NodeFinding(irt5.x[[3]]) == "@LIKELIHOOD"
)

## Add in virtual likelihood from a second judge
NodeLikelihood(irt5.x[[3]]) <- NodeLikelihood(irt5.x[[3]]) * c(.75,.25)
stopifnot(
  sum(abs(NodeLikelihood(irt5.x[[3]]) - c(.6,.05))) < 1e-6
)


DeleteNetwork(irt5)

}
\keyword{ interface }
\keyword{ manip }% __ONLY ONE__ keyword per line
