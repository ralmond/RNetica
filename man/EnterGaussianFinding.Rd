\name{EnterGaussianFinding}
\alias{EnterGaussianFinding}
\title{Enter a numeric finding with uncertainty
}
\description{
  This function a likelihood for a node that follows a Gaussian
  distribution with a given mean and standard deviation.  This is
  entered as virtual evidence.  
}
\usage{
EnterGaussianFinding(node, mean, sem, retractFirst = TRUE)
}
\arguments{
  \item{node}{
    An active \code{\linkS4class{NeticaNode}} object that references the node.
    Node should be continuous, or have numeric value ranges assigned to
    it using \code{\link{NodeLevels}(\var{node})}.
  }
  \item{mean}{
    A numeric scalar giving the observed value (mean of the normal).
}
  \item{sem}{
    A nonnegative numeric scalar giving the standard error of
    measurement for the observed finding (standard deviation of the
    normal).  
}
  \item{retractFirst}{
    A logical value.  If true, any previous findings will be retracted
    first. 
}
}
\details{
  The \code{node} must a continuous node that has
  been discretized using \code{\link{NodeLevels}(node)}.  The
  probabilities for each state are calculated based on a Gaussian
  distribution with the given \code{mean} and \code{sem} (SD).  
}
\value{
  Return the \code{node} argument invisibly.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{EnterGaussianFinding_bn}, 
}
\author{
  Russell Almond
}
\section{Warning }{

  The Netica function EnterGaussianFinding_bn is not behaving at all
  like what I expected.  In particular, I expect that it would behave
  like a normal likelihood, 
  but instead it seems to be behaving as if I typed the expression
  \code{\link{NodeValue}(node)<-mean}.  I've queried Norsys about
  this.  

  Meanwhile, I've worked around by calling \code{\link{NodeLikelihood}}
  instead of the internal Netica function.

}
\seealso{
  \code{\link{EnterNegativeFinding}()}, \code{\link{EnterFindings}()},
  \code{\link{RetractNodeFinding}()}, \code{\link{NodeLikelihood}()},
  \code{\link{NodeFinding}()}, \code{\link{EnterIntervalFinding}()},
  \code{\link{NodeValue}()}
}
\examples{
sess <- NeticaSession()
startSession(sess)

cirt5 <- CreateNetwork("ContinuousIRT5", session=sess)

theta <- NewContinuousNode(cirt5,"Theta")
NodeLevels(theta) <- c(-5,-2.5,-1.5,-0.5,0.5,1.5,2.5,5)
NodeProbs(theta) <- rep(1/NodeNumStates(theta),NodeNumStates(theta))

CompileNetwork(cirt5) ## Ready to enter findings

EnterGaussianFinding(theta,0,1)
NodeBeliefs(theta)

stopifnot(all(abs(NodeBeliefs(theta) -
              diff(pnorm(NodeLevels(theta),0,1))) < .0001))

DeleteNetwork(cirt5)
stopSession(sess)

}
\keyword{ interface }
\keyword{ manip }
