\name{NetworkFootprint}
\alias{NetworkFootprint}
\title{
  Returns a list of names of unconnected edges.
}
\description{
  When a link is detached through setting a \code{\link{NodeParents}()}
  to \code{NULL}, or through copying a node but not its parent to a new
  network, this leaves a \emph{stub node}, an unsatisfied connection.
  This function runs through the set of nodes in a network and lists the
  names of all unsatisfied connections.
}
\usage{
NetworkFootprint(net)
}
\arguments{
  \item{net}{
    An active \code{\link{NeticaBN}} to be examined.
  }
}
\details{

  Stub nodes -- unsatisfied links or connections -- can happen in two
  ways.  Either by setting one of the values of
  \code{\link{NodeParents}(node)} to \code{NULL}, or by copying a node
  (using \code{\link{CopyNodes}()}) without copying its parents.  (This
  can also be done in the Netica GUI by detaching the link from the
  parent end).  This this case Netica names the
  \code{\link{NodeInputNames}()} according to the name of the old node.

  The function \code{NetworkFootprint(net)} search all of the nodes in
  \code{net} to find stub nodes, and reports the
  \code{NodeInputNames()} of the stub nodes.  This function provides a
  test for unsatisfied connections, and should be of assistance when
  joining two networks together.  The function
  \code{\link{AdjoinNetwork}(sm,em)} joins two networks together and
  attempts to resolve the unsatisfied connections in \code{em}.

  One particular application of the footprint is in the EM--SM algorithm
  (Almond et al, 1999; Almond and Mislevy, 1999).  Here it is assumed
  that nodes in the footprint of an evidence model will be joined.
  Making a clique node \code{\link{MakeCliqueNode}()} ensures that joint
  information from the evidence model will find a good home in the
  system model network.
}
\value{
  A character vector giving the input names of the stub nodes in
  \code{net}.  Duplicate values are removed.
}
\references{
  Almond, R. G. & Mislevy, R. J. (1999) Graphical models and computerized
adaptive testing.  \emph{Applied Psychological Measurement}, 23, 223-238.

Almond, R., Herskovits, E., Mislevy, R. J., & Steinberg,
L. S. (1999). Transfer of information between system and evidence
models. In Artificial Intelligence and Statistics 99, Proceedings
(pp. 181--186). Morgan-Kaufman

}
\author{
  Russell Almond
}
\seealso{
  \code{\link{NeticaNode}}, \code{\link{NodeParents}()},
  \code{\link{MakeCliqueNode}()}, \code{\link{NodeInputNames}()},
  \code{\link{CopyNodes}()},\code{\link{AdjoinNetwork}()}
}
\examples{

## System/Student model
EMSMSystem <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","System.dne",
                           sep=.Platform$file.sep))

CompileNetwork(EMSMSystem)
JunctionTreeReport(EMSMSystem)

## Evidence model for Task 1a
EMTask1a <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","EMTask1a.dne",
                           sep=.Platform$file.sep))
NetworkFootprint(EMTask1a)
## The corresponding clique is not in system model, so force it in.
MakeCliqueNode(NetworkFindNode(EMSMSystem, NetworkFootprint(EMTask1a)))
CompileNetwork(EMSMSystem)
JunctionTreeReport(EMSMSystem)



## Evidence model for Task 2a
EMTask2a <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","EMTask2a.dne",
                           sep=.Platform$file.sep))
NetworkFootprint(EMTask2a)
## This is already a clique, so nothing to do.

DeleteNetwork(list(EMSMSystem,EMTask1a,EMTask2a))

}
\keyword{ interface}
\keyword{ manip }% __ONLY ONE__ keyword per line
