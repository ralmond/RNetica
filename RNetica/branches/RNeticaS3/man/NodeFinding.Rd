\name{NodeFinding}
\alias{NodeFinding}
\alias{NodeFinding<-}
\title{
  Returns of sets the observed value associated with a Netica node.
}
\description{
  A finding is an observed variable in a Bayesian network.  The
  expression \code{NodeFinding(node) <- value} indicates that the
  observed value of \code{node} should be set to \code{value}.  The
  function \code{NodeFinding(node)} returns the current value.
}
\usage{
NodeFinding(node)
NodeFinding(node) <- value
}
\arguments{
  \item{node}{
    An active \code{\link{NeticaNode}} whose value was observed or
    hypothesized. 
  }
  \item{value}{
    A character or integer scalar indicating the value which was
    observed or hypothesized.  If a character, it should be one of the
    values in \code{\link{NodeStates}(node)}.  If an integer it should
    be a value between 1 and \code{\link{NodeNumStates}(node)}
    inclusive. 
  }
}
\details{
  Setting \code{NodeFinding(node) <- value} essentially asserts that
  \eqn{Pr(node=value) = 1}.  The value may be either expressed as a
  character name of one of the states, or an integer giving the index
  into the state table.

  Note that setting \code{NodeFinding(node) <- value} clears any previous
  findings (including virtual findings set through
  \code{\link{NodeLikelihood}()} or
  \code{\link{EnterNegativeFinding}()}), that may have been set.  The
  function \code{\link{RetractNodeFinding}(node)} will clear the current
  finding without setting it to a new value.
  
  The function \code{NodeFinding(node)} returns the currently set
  finding, if there is one.  It can also return one of the three special
  values:
  \enumerate{
    \item  \code{"@NEGATIVE FINDINGS"} --- Negative findings have been
    entered using \code{\link{EnterNegativeFinding}()}. 
    \item  \code{"@LIKELIHOOD"} --- Uncertain evidence which provides a
    likelihood of various states of the node were entered using
    \code{\link{NodeLikelihood}(node)} 
    \item  \code{"@NO FINDING"} --- No findings, including negative
    findings or likelihood findings were entered. 
  }

}
\value{
  The expression \code{NodeFinding(node)<-value} returns the modified
  node invisibly.

  The function \code{NodeFinding(node)} returns a string which is either
  the currently set finding or one of the special values \code{"@NO
  FINDING"}, \code{"@LIKELIHOOD"}, or \code{"@NEGATIVE FINDINGS"}.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GetNodeFinding_bn}, \nref{EnterFinding_bn}
}
\author{
  Russell Almond
}
\note{
  If \code{\link{SetNetworkAutoUpdate}()} has been set to \code{TRUE},
  then this function could take some time as each finding is
  individually propagated.  Consider wrapping multiple calls setting
  \code{NodeFinding()} in \code{WithoutAutoUpdate(net, ...)}.

  Unlike the Netica function \code{EnterFinding_bn()} the function
  \code{"NodFinding<-"} internally calls \code{RetractFindings}.  So
  there is no need to do this manually.
}
\seealso{
  \code{\link{NeticaBN}}, \code{\link{NodeBeliefs}()},
  \code{\link{EnterNegativeFinding}()}, \code{\link{EnterFindings}()},
  \code{\link{RetractNodeFinding}()}, \code{\link{NodeLikelihood}()},
  \code{\link{EnterGaussianFinding}()}, \code{\link{EnterIntervalFinding}()},
  \code{\link{JointProbability}()},\code{\link{NodeValue}()},
  \code{\link{MostProbableConfig}()},
  \code{\link{FindingsProbability}()}
}
\examples{
irt5 <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","IRT5.dne",
                           sep=.Platform$file.sep))

irt5.theta <- NetworkFindNode(irt5,"Theta")
irt5.x <- NetworkFindNode(irt5,paste("Item",1:5,sep="_"))

CompileNetwork(irt5) ## Ready to enter findings

stopifnot (
  ## irt5 is parent node, so marginal beliefs and conditional
  ## probability table should be the same.
  sum(abs(NodeBeliefs(irt5.theta) - NodeProbs(irt5.theta))) < 1e-6
)
## Marginal probability for Node 5
irt5.x5.init <- NodeBeliefs(irt5.x[[5]])

SetNetworkAutoUpdate(irt5,TRUE) ## Automatic updating
NodeFinding(irt5.x[[1]]) <- "Right"
stopifnot( 
  IsBeliefUpdated(irt5.x[[5]])
)
irt5.x5.time1 <- NodeBeliefs(irt5.x[[5]])
stopifnot (
  sum(abs(irt5.x5.init-irt5.x5.time1)) > 1e-6
)

SetNetworkAutoUpdate(irt5,FALSE) ## Automatic updating
NodeFinding(irt5.x[[2]]) <- 2 ## Wrong
stopifnot( 
  !IsBeliefUpdated(irt5.x[[5]]),
  NodeFinding(irt5.x[[2]]) == "Wrong"
)
irt5.x5.time2 <- NodeBeliefs(irt5.x[[5]])
stopifnot (
  sum(abs(irt5.x5.time2-irt5.x5.time1)) > 1e-6,
  IsBeliefUpdated(irt5.x[[5]]) ## Now we have updated it.
)

## Negative finding
EnterNegativeFinding(irt5.theta,c("neg1","neg2")) ## Rule out negatives.
stopifnot(
  NodeFinding(irt5.theta) == "@NEGATIVE FINDINGS"
)

## Clearing Findings
RetractNodeFinding(irt5.theta)
stopifnot(
  NodeFinding(irt5.theta) == "@NO FINDING"
)

##Virtual findings for X3.  Assume judge has said right, but judge has
##  80\% accuracy rate.
NodeLikelihood(irt5.x[[3]]) <- c(.8,.2)
stopifnot(
  NodeFinding(irt5.x[[3]]) == "@LIKELIHOOD"
)

DeleteNetwork(irt5)

}
\keyword{ interface }
\keyword{ manip }% __ONLY ONE__ keyword per line
