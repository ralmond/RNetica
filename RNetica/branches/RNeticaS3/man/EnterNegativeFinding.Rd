\name{EnterNegativeFinding}
\alias{EnterNegativeFinding}
\title{
  Sets findings for a Netica node to a list of ruled out values.
}
\description{
  This is conceptually equivalent to setting
  \code{\link{NodeFinding}{node}<-not(eliminatedVals)} (although this will not
  work as \code{NodeFinding} does not accept set values).  It
  essentially eliminates any of the \code{eliminatedVals} as possible
  values (assigns them zero probability).
}
\usage{
EnterNegativeFinding(node, eliminatedVals)
}
\arguments{
  \item{node}{
    An active \code{\link{NeticaNode}} whose value was observed or
    hypothesized. 
}
  \item{eliminatedVals}{
    A character or integer vector indicating the values to be ruled out.
    Character values should be one of the values in
    \code{\link{NodeStates}(node)}.  Integer values should be between 1
    and \code{\link{NodeNumStates}(node)} inclusive. 
  }
}
\details{
  This function essentially asserts that \eqn{Pr(node \in eliminatedVals)
    = 0}.  Thus, it rules out the values in the \code{eliminatedVals}
  set.  Note that the length of this set should be less than the number
  of states, or all possibilities will have been eliminated.

  Note calling \code{EngerNegativeFining(node, ...)} clears any previous
  findings (including virtual findings set through
  \code{\link{NodeLikelihood}()} or simple finding set through
  \code{\link{NodeFinding}(node)<-value}).  The function
  \code{\link{RetractNodeFinding}(node)} will clear the current finding
  without setting it to a new value.

}
\value{
  This function returns \code{node} invisibly.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{EnterFindingNot_bn}
}
\author{
  Russell Almond
}
\note{
  If \code{\link{SetNetworkAutoUpdate}()} has been set to \code{TRUE},
  then this function could take some time as each finding is
  individually propagated.  Consider wrapping multiple calls setting
  \code{NodeFinding()} in \code{WithoutAutoUpdate(net, ...)}.

  Unlike the Netica function \code{EnterFindingNot_bn()} the function
  \code{EnterNegativeFinding()} internally calls \code{RetractFindings}.  So
  there is no need to do this manually.  Also, the internal Netica
  function multiplies multiple calls to \code{EnterFindingNod_bn()} add
  to the list of negative findings, while in the R version takes the
  entire list.
}
\seealso{
  \code{\link{NeticaBN}}, \code{\link{NodeBeliefs}()},
  \code{\link{NodeFinding}()},
  \code{\link{RetractNodeFinding}()}, \code{\link{NodeLikelihood}()}
}
\examples{
irt5 <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","IRT5.dne",
                           sep=.Platform$file.sep))

irt5.theta <- NetworkFindNode(irt5,"Theta")
irt5.x <- NetworkFindNode(irt5,paste("Item",1:5,sep="_"))

CompileNetwork(irt5) ## Ready to enter findings

## Calculated new expected beliefs
renormed <- NodeProbs(irt5.theta)
renormed[c("neg1","neg2")] <- 0
renormed <- renormed/sum(renormed)

## Negative finding
EnterNegativeFinding(irt5.theta,c("neg1","neg2")) ## Rule out negatives.
stopifnot(
  NodeFinding(irt5.theta) == "@NEGATIVE FINDINGS",
  sum(abs(NodeLikelihood(irt5.theta) - c(1,1,1,0,0))) < 1e-6,
  sum(abs(NodeBeliefs(irt5.theta) - renormed)) < 1.e-6
)


DeleteNetwork(irt5)

}
\keyword{ interface }
\keyword{ manip }% __ONLY ONE__ keyword per line
