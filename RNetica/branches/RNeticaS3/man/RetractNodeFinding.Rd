\name{RetractNodeFinding}
\alias{RetractNodeFinding}
\alias{RetractNetFindings}
\title{
  Clears any findings for a Netica node or network.
}
\description{
  The function \code{RetractNodeFinding(node)} clears any findings or
  virtual findings set with \code{\link{NodeFinding}()},
  \code{\link{EnterNegativeFinding}()} or
  \code{\link{NodeLikelihood}()} and associated with \code{node}.  The
  function \code{RetractNetFindings(net)} clears any findings associated
  with any node in the network.
}
\usage{
RetractNodeFinding(node)
RetractNetFindings(net)
}
\arguments{
  \item{node}{
    An active \code{\link{NeticaNode}} whose findings are to be
    retracted. 
  }
  \item{net}{
    An active \code{\link{NeticaBN}} whose findings are to be
    retracted. 
  }
}
\details{
  This is an undo function for \code{\link{NodeFinding}()},
  \code{\link{EnterNegativeFinding}()} or
  \code{\link{NodeLikelihood}()}.  In particular, it allows for entering
  hypothesized findings for various calculations.
}
\value{
  Returns its argument invisibly.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{RetractNetFindings_bn}, \nref{RetractNodeFindings_bn}
}
\author{
  Russell Almond
}
\note{
  If \code{\link{SetNetworkAutoUpdate}()} has been set to \code{TRUE},
  then this function could take some time as each finding is
  individually propagated.  Consider wrapping multiple calls setting
  \code{NodeFinding()} in \code{WithoutAutoUpdate(net, ...)}.

  The Netica functions for setting node findings require the programmer
  to call \code{RetractNodeFindings_bn()} before setting values to clear
  out old findings.  The RNetica functions do this internally, so the
  user does not need to worry about this.
}
\seealso{
  \code{\link{NeticaBN}}, \code{\link{NodeBeliefs}()},
  \code{\link{EnterNegativeFinding}()},
  \code{\link{EnterGaussianFinding}()}, \code{\link{EnterIntervalFinding}()},
  \code{\link{NodeFinding}()}, \code{\link{NodeLikelihood}()}
}
\examples{
irt5 <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","IRT5.dne",
                           sep=.Platform$file.sep))

irt5.theta <- NetworkFindNode(irt5,"Theta")
irt5.x <- NetworkFindNode(irt5,paste("Item",1:5,sep="_"))

CompileNetwork(irt5) ## Ready to enter findings

stopifnot(NodeFinding(irt5.x[[1]]) == "@NO FINDING")

NodeFinding(irt5.x[[1]]) <- "Right"
stopifnot(NodeFinding(irt5.x[[1]]) == "Right")

RetractNodeFinding(irt5.x[[1]]) 
stopifnot(NodeFinding(irt5.x[[1]]) == "@NO FINDING")

NodeFinding(irt5.x[[1]]) <- "Wrong"
NodeFinding(irt5.x[[2]]) <- 1
NodeFinding(irt5.x[[3]]) <- 2
stopifnot(
  NodeFinding(irt5.x[[1]]) == "Wrong",
  NodeFinding(irt5.x[[2]]) == "Right",
  NodeFinding(irt5.x[[3]]) == "Wrong",
  NodeFinding(irt5.x[[4]]) == "@NO FINDING",
  NodeFinding(irt5.x[[5]]) == "@NO FINDING"
)

RetractNetFindings(irt5)
stopifnot(
  NodeFinding(irt5.x[[1]]) == "@NO FINDING",
  NodeFinding(irt5.x[[2]]) == "@NO FINDING",
  NodeFinding(irt5.x[[3]]) == "@NO FINDING",
  NodeFinding(irt5.x[[4]]) == "@NO FINDING",
  NodeFinding(irt5.x[[5]]) == "@NO FINDING"
)

DeleteNetwork(irt5)

}
\keyword{ interface }
\keyword{ manip }% __ONLY ONE__ keyword per line
