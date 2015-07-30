\name{EnterIntervalFinding}
\alias{EnterIntervalFinding}
\title{Enter finding of value within an interval
}
\description{
  Sets the finding associate with node to an interval.
}
\usage{
EnterIntervalFinding(node, low, high, retractFirst = TRUE)
}
\arguments{
  \item{node}{
    An active \code{\link{NeticaNode}} object that references the node.
}
  \item{low}{
    Lower bound of interval.
}
  \item{high}{
    Upper bound of interval.
}
  \item{retractFirst}{
    A logical value.  If true, any previous findings will be retracted
    first. 
}
}
\details{
  The \code{node} must a continuous node that has
  been discretized using \code{\link{NodeLevels}(node)}.  The
  probabilities for each state are calculated based on a uniform
  distribution with the given \code{low} and \code{high} endpoints.

}
\value{
  Return the \code{node} argument invisibly.

}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{EnterIntervalFinding_bn}
}
\author{
  Russell Almond
}

\section{Warning }{

  This function is not behaving at all like what I expected.  In
  particular, I expect that it would behave like a normal likelihood,
  but instead it seems to be behaving as if I typed the expression
  \code{\link{NodeValue}(node)<-low}.  I've queried Norsys about
  this.  Use with caution until I get a clarification.

}

\seealso{
  \code{\link{EnterNegativeFinding}()}, \code{\link{EnterFindings}()},
  \code{\link{RetractNodeFinding}()}, \code{\link{NodeLikelihood}()},
  \code{\link{NodeFinding}()}, \code{\link{EnterGaussianFinding}()},
  \code{\link{NodeValue}()}
}
\examples{
cirt5 <- CreateNetwork("ContinuousIRT5")

theta <- NewContinuousNode(cirt5,"Theta")
NodeLevels(theta) <- c(-5,-2.5,-1.5,-0.5,0.5,1.5,2.5,5)
theta[] <- rep(1/NodeNumStates(theta),NodeNumStates(theta))

CompileNetwork(cirt5) ## Ready to enter findings

EnterIntervalFinding(theta,-1,1)
NodeBeliefs(theta)

## I expect the middle three values to be non-negative, but that is not
## what I get!

DeleteNetwork(cirt5)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ interface }
\keyword{ manip }% __ONLY ONE__ keyword per line
