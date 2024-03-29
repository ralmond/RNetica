\name{NodeExpectedUtils}
\alias{NodeExpectedUtils}
\title{Calculates expected utility for each value of a decision node
}
\description{

  Calculates the expected utility for a decision node.  That is for each
  state of the decision node it calculates the expected utility if that
  state is chosen.

}
\usage{
NodeExpectedUtils(node)
}
\arguments{
  \item{node}{
    An active \code{\linkS4class{NeticaNode}} object that references the node.
    This should be a decision node, that is \code{\link{NodeKind}(\var{node})}
    should equal \code{"Decision"}.
  }
}
\details{

  This solves a decision problem.  In an influence diagram (decision
  net), one decision node is considered a predecessor if its value is
  known at the time when a decision is made.  The compilation process
  for a decision net will fill in predecessor relationships when they
  are implied by paths through nature nodes.  Decision networks are
  typically \dQuote{solved} by working backwards in time from the last
  decision to the first.

  The expression \code{NodeExpectedUtils(\var{node})} will only return a
  meaningful result if either, \var{node} represents the first
  sequential decision, or all prior decisions have been made (and their
  values are known).
  
}
\value{

  This should return a named numeric vector of length
  \code{\link{NodeNumStates}(\var{node})} with each element corresponding to
  one of the states of \code{node}.

}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GetNodeExpectedUtils_bn}

  For more about decision nets:
  \url{http://www.norsys.com/WebHelp/NETICA/X_Decision_Problems.htm}

}
\author{
  Russell Almond
}
\section{Warning}{

  This function is currently returning an internal Netica error.  Do not
  use until I get clarification from Norsys.

}
\seealso{
  \code{\link{NodeKind}()}, \code{\link{NodeValue}()},
  \code{\link{NodeExpectedValue}()}
}
\examples{
sess <- NeticaSession()
startSession(sess)

## Read the RTI network from the library.
rti <- ReadNetworks(system.file("sampleNets","CostOfTesting.dne",
                                package="RNetica"),
                    session=sess)
## The two decision nodes
Test <- NetworkFindNode(rti,"Test")
Instruction <- NetworkFindNode(rti,"Instruction")

## Network must be compiled before analysis:
CompileNetwork(rti)


NodeExpectedUtils(Test)
\dontrun{
## This produces an error because Test is not set.
NodeExpectedUtils(Instruction)
}
NodeFinding(Test) <- "Yes"
NodeExpectedUtils(Instruction)


DeleteNetwork(rti)
stopSession(sess)

}
\keyword{ interface }
\keyword{ manip }
