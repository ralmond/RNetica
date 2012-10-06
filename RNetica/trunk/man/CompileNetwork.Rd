\name{CompileNetwork}
\alias{CompileNetwork}
\alias{UncompileNetwork}
\title{
  Builds the junction tree for a Netica Network
}
\description{
  Before Netica performs inference in a network, it needs to compile the
  network.  This process consists of building a junction tree and
  conditional probability tables for the nodes of that tree.  The
  function \code{CompileNetwork()} compiles the network and
  \code{UncompileNetwork()} undoes the compilation and frees the
  associated memory.
}
\usage{
CompileNetwork(net)
UncompileNetwork(net)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{
    An active \code{\link{NeticaBN}} which will be compiled.
  }
}
\details{
  Usually Bayesian network projects operate in two phases.  In the
  construction phase, new nodes are added to the network, new connections
  made and conditional probabilty tables are set.

  In the inference phase, findings are added to nodes and other nodes
  are queried about their current conditional probability tables.

  The functions \code{CompileNetowrk()} and \code{UncompileNetwork()}
  move the networks between the two phases.
}
\value{
  The \code{NeticaBN} object \code{net} is returned invisibly.
}
\references{
  \link{http://norsys.com/onLineAPI/Manual/index.html}:
  \code{CompileNet_bn()}, \code{UncompileNet_bn()}, 

}
\author{
  Russell Almond
}
\note{
  I don't currently know of a good way to test a node to see if it is
  compiled. However, I think calling \code{CompileNetwork()} twice on
  the same network is harmless.
}
\seealso{
  \code{\link{NeticaBN}}, \code{\link{HasNodeTable}()},
  \code{\link{NodeFinding}()}, \code{\link{NodeBeliefs}()}
}
\examples{

irt5 <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","IRT5.dne",
                           sep=.Platform$file.sep))

CompileNetwork(irt5) ## Ready to enter findings

UncompileNetwork(irt5) ## Ready to add more nodes

DeleteNetwork(irt5)

}
\keyword{ interface }
\keyword{ model }% __ONLY ONE__ keyword per line
