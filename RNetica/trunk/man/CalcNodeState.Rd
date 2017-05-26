\name{CalcNodeState}
\alias{CalcNodeState}
\alias{CalcNodeValue}
\title{Calculates the state of a node based on logical functions or formulae
}
\description{
  The expression \code{CalcNodeState(node)} will return the state of
  \code{node} if it is known deterministically, and \code{NA} if the exact
  value is not known.  The expression \code{CalcNodeValue(node)} will
  return the numeric value of the node (e.g., the value set with
  \code{\link{NodeLevels}(node)}.  
}
\usage{
CalcNodeState(node)
CalcNodeValue(node)
}
\arguments{
  \item{node}{
    An active \code{\link{NeticaNode}} object that references the node.
}
}
\details{
  According to the Netica manual, the way that the value of \code{node}
  could be known absolutely is if it was set directly a call to 
  \code{\link{NodeFinding}(node)} or \code{\link{NodeValue}(node)}, or
  if the value can be calculated exactly through logical conditional
  probability tables (i.e., ones with just 0's and 1's) or formula (see
  \code{\link{NodeEquation}()}.

  The expression \code{\link{CalcNodeState}(node)} is appropriate when
  \code{node} is discrete, or has been discretized through a call to
  \code{\link{NodeLevels}(node)}.  Otherwise it will generate an error.

  The expression \code{\link{CalcNodeValue}(node)} is appropriate when
  \code{node} is continuous, or the states have been assigned numeric
  values through a call to \code{\link{NodeLevels}(node)}.  Otherwise it
  will generate an error. 
}
\value{

  The expression \code{\link{CalcNodeState}(node)} will return a
  character scalar giving the name of the current state of \code{node}
  if it can be determined, otherwise it will return \code{NA}.

  The expression \code{\link{CalcNodeValue}(node)} will return a
  numeric scalar giving the name of the current value of \code{node}
  if it can be determined, otherwise it will return \code{NA}.
  
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{CalcNodeState_bn}, \nref{CalcNodeValue_bn}
}
\author{
  Russell Almond
}
\section{Warning }{

  This function is not behaving at all like what I expected.  In
  particular, it is returning \code{NA} in many cases where I expect it
  to produce a value.  I've queried Norsys about this, but use with caution
  until I get a clarification.

}

\seealso{
  \code{\link{NodeFinding}()}, \code{\link{NodeLevels}()},
  \code{\link{NodeValue}()}, \code{\link{IsNodeDeterministic}()},
  \code{\link{NodeEquation}()},\code{\link{is.continuous}()},
  \code{\link{NodeExpectedValue}()}

}
\examples{

lights <- CreateNetwork("lights")
switchs <- NewDiscreteNode(lights,paste("Switch",1:2,sep=""),c("Up","Down"))
bulb <- NewDiscreteNode(lights,"Bulb",c("On","Off"))

## Set up a two-way switch (Xor) network
AddLink(switchs[[1]],bulb)
AddLink(switchs[[2]],bulb)
## This sets up a logical table, so that the light is on iff
## both switches are in the same orientation.
bulb[] <-"Off"
bulb[Switch1="Up",Switch2="Up"]<-"On"
bulb[Switch1="Down",Switch2="Down"]<-"On"
switchs[[1]][] <- .5
switchs[[2]][] <- .5


CompileNetwork(lights)

## Bulb is a deterministic node.
stopifnot(IsNodeDeterministic(bulb))

## value of node is unknown, returns NA
stopifnot(is.na(CalcNodeState(bulb)))

NodeFinding(switchs[[1]]) <- "Up"
NodeFinding(switchs[[2]]) <- "Up"

stopifnot(CalcNodeState(switchs[[1]])=="Up")

stopifnot(CalcNodeState(bulb)=="On")

NodeLevels(bulb) <-c(1,0)
NodeLevels(switchs[[1]]) <-c(1,0)
NodeLevels(switchs[[2]]) <-c(1,0)

## I expect both of these to return 1, but they return NA
CalcNodeValue(bulb)
CalcNodeValue(switchs[[1]])

DeleteNetwork(lights)


}
\keyword{ interface }
\keyword{ manip }
