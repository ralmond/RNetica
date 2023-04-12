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
  \var{node} conditioned on the findings.

  The function \code{IsBeliefUpdated(\var{node})} checks to see whether
  the value of findings have been propagated to \var{node} yet.
}
\usage{
NodeBeliefs(node)
IsBeliefUpdated(node)
}
\arguments{
  \item{node}{
    An active \code{\linkS4class{NeticaNode}} representing the variable whose
    marginal distribution is to be determined.
}
}
\details{
  The function \code{NodeBeliefs()} is not available until the network
  has been compiled (\code{\link{CompileNetwork}()}).  Asking for
  the marginal values before the network is compiled will throw an
  error.

  When findings are entered, the marginal probabilities (or beliefs)
  associated with \var{node} will change.  The process of propagating
  the findings from an evidence node to a query node is known as
  updating.  Depending on the size and topology of the network, the
  updating process might take some time.  To speed up operations, the
  \var{AutoUpdate} flag on the network can be cleared using
  \code{\link{SetNetworkAutoUpdate}()}.

  If the \var{AutoUpdate} flag is not set for the network, then calling
  \code{NodeBeliefs(node)} could trigger an update cycle and hence take
  some time.  The function \code{IsBeliefUpdated(code)} tests to see
  whether the marginal probability for \var{node} currently
  incorporates all of the findings.  It returns true if it does and
  false if not.
}
\value{

  The function \code{NodeBeliefs(\var{node})} returns a vector of
  probabilities of length \code{\link{NodeNumStates}(\var{node})}.  The names
  of the result are the state names.

  The function \code{IsBeliefUpdated(\var{node})} returns \code{TRUE} if
  calling \code{NodeBeliefs(\var{node})} will not result in probabilities
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
  writing about Dempster--Shafer models (belief functions).  Netica uses
  it to mean the marginal probability for a node given all of the
  entered evidence and conditional probability tables of all of the nodes.

}
\seealso{
  \code{\linkS4class{NeticaNode}}, \code{\linkS4class{NeticaBN}},
  \code{\link{NodeProbs}()}, 
  \code{\link{NodeFinding}()}, \code{\link{JointProbability}()},
  \code{\link{MostProbableConfig}()}, \code{\link{FindingsProbability}()}
  \code{\link{NodeExpectedValue}()}, \code{\link{NodeValue}()},
  \code{\link{CalcNodeValue}()},
}
\examples{
sess <- NeticaSession()
startSession(sess)

irt5 <- ReadNetworks(system.file("sampleNets","IRT5.dne",
                                 package="RNetica"),
                     session=sess)

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
stopSession(sess)

}
\keyword{ interface }
\keyword{ manip }
