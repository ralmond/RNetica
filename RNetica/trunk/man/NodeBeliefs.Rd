\name{NodeBeliefs}
\alias{NodeBeliefs}
\alias{IsBeliefUpdated}
\title{
  Returns the current marginal probability distribution associated with
  a node in a Netica network.
}
\description{
  After a network is compiled, marginal probabilities are available at
  each of the nodes.  Entering findings changes these to probabilities
  associated with the conditions represented by the findings.  This
  function returns the marginal probabilities for the variable
  \code{node} conditioned on the findings.

  The function \code{IsBeliefUpdated(node)} checks to see whether the value
  of findings have been propagated to \code{node} yet.
}
\usage{
NodeBeliefs(node)
IsBeliefUpdated(node)
}
\arguments{
  \item{node}{
    An active \code{\link{NeticaNode}} representing the variable whose
    marginal distribution is to be determined.
}
}
\details{
  The function \code{NodeBeliefs()} is not available until the network
  has been compiled (\code{\link{CompileNetwork}()}).  Asking for
  the marginal values before the network is compiled will throw an
  error.

  When findings are entered, the marginal probabilities (or beliefs)
  associated with \code{node} will change.  The process of propagating
  the findings from an evidence node to a query node is known as
  updating.  Depending on the size and topology of the network, the
  updating process might take some time.  To speed up operations, the
  \code{AutoUpdate} flag on the network can be cleared using
  \code{\link{SetNetworkAutoUpdate}()}.

  If the \code{AutoUpdate} flag is not set for the network, then calling
  \code{NodeBeliefs(node)} could trigger an update cycle and hence take
  some time.  The function \code{IsBeliefUpdated(code)} tests to see
  whether the marginal probability for \code{node} currently
  incorporates all of the findings.  It returns true if it does and
  false if not.
}
\value{
  The function \code{NodeBeliefs(node)} returns a vector of
  probabilities of length \code{\link{NodeNumStates}(node)}.  The names
  of the result are the state names.

  The function \code{IsBeliefUpdated(node)} returns \code{TRUE} if
  calling \code{NodeBeliefs(node)} will not result in probabilities
  being updated.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GetNodeBeliefs_bn}, \nref{IsBeliefUpdated_bn}
}
\author{
  Russell Almond
}
\note{
  I tend to avoid the term "belief" because I've spent so much time
  writing about Dempster--Shafer models (belief functions).
}
\seealso{
  \code{\link{NeticaBN}}, \code{\link{NodeProbs}()},
  \code{\link{NodeFinding}()}, \code{\link{JointProbability}()},
  \code{\link{MostProbableConfig}()}, \code{\link{FindingsProbability}()}
  \code{\link{NodeExpectedValue}()}, \code{\link{NodeValue}()},
  \code{\link{CalcNodeValue}()},
}
\examples{
irt5 <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","IRT5.dne",
                           sep=.Platform$file.sep))

irt5.theta <- NetworkFindNode(irt5,"Theta")
irt5.x <- NetworkFindNode(irt5,paste("Item",1:5,sep="_"))

\dontrun{
NodeBeliefs(irt5.theta) ## This call will produce an errors because irt5
                        ## is not compiled
}
stopifnot(
  !IsBeliefUpdated(irt5.theta)
)
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
NodeFinding(irt5.x[[2]]) <- "Right"
stopifnot( 
  !IsBeliefUpdated(irt5.x[[5]])
)
irt5.x5.time2 <- NodeBeliefs(irt5.x[[5]])
stopifnot (
  sum(abs(irt5.x5.time2-irt5.x5.time1)) > 1e-6,
  IsBeliefUpdated(irt5.x[[5]]) ## Now we have updated it.
)

DeleteNetwork(irt5)

}
\keyword{ interface }
\keyword{ manip }% __ONLY ONE__ keyword per line
