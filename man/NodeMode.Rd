\name{NodeMode}
\alias{NodeMode}
\title{Returns the most likely state of a node.}
\description{

  From a compiled network, returns the most likely state, 
  that is `which.max(NodeBeliefs(node))`, only this returns the state
  label.
  
}
\usage{
NodeMode(node)
}
\arguments{
  \item{node}{A \code{\linkS4class{NeticaNode}} object in a
  compiled network.}
}
\value{
  A character scalar giving the name of the most likely state.
}
\author{Russell G. Almond}
\note{
  If there are multiple modes, this function with return the first one.
}
\seealso{
  \code{\link{NodeBeliefs}}, \code{\link{NodeStates}}
}
\examples{
  
SetupDefaultSession()
irt5 <- local_RNetica_net("IRT5.dne")
theta <- NetworkFindNode(irt5,"Theta")

CompileNetwork(irt5)
NodeBeliefs(theta)
NodeMode(theta)

withr::deferred_run()  # Stops Netica.
 
}
\keyword{ interface }
\keyword{ manip }
