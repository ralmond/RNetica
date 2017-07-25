\name{EliminationOrder}
\alias{EliminationOrder}
\alias{EliminationOrder<-}
\title{
  Retrieves or sets the elimination order used in compiling a Netica network.
}
\description{
  The compilation process involves eliminating the nodes in the network
  one-by-one, different orders will produce junction trees of different
  sizes.  The function \code{EliminationOrder(net)} returns the current
  elimination order associated with a network.  The expression
  \code{EliminationOrder(net) <- value} sets the elimination order.
}
\usage{
EliminationOrder(net)
EliminationOrder(net) <- value
}
\arguments{
  \item{net}{
    An active \code{\link{NeticaBN}}/
  }
  \item{value}{
    Either \code{NULL} (to clear the elimination order) or a list of
    every node in \code{net} with no duplicates.
  }
}
\details{

  Large cycles create problems for propagating probabilities in Bayesian
  networks. A solution to this problem is to fill-in chords (short cuts)
  in the cycles and then transform the network to a tree shape with the
  nodes of the tree representing cliques of the graph.  This is commonly
  called a junction tree (although a junction tree additionally has
  nodes separating the cliques, called \emph{sepsets} in Netica).

  Finding the optimal pattern of fill-ins is an NP hard problem.  A
  common way of approaching it is to eliminate the nodes from the
  network one-by-one and connect the neighbours of the eliminated node
  (if they were not already connected).  In this case, the sequence of
  eliminated nodes will determine which edges are filled in, and hence
  the size of the final junction tree.  Finding an optimal eliminator
  order is also NP hard, but simple heuristics (like the greedy
  algorithm) tend to do reasonably well in practice.  (See Almond, 1995,
  for a complete description of the algorithm and heuristics solutions).

  When Netica compiles a network (\code{\link{CompileNetwork}(net)}), it
  picks an elimination order, unless one has already been set.  Unless
  the network has a particular difficult structure, then the Netica
  defaults should work pretty well.  The function
  \code{\link{JunctionTreeReport}(net)} gives a report about the
  existing tree.

  If the analyst has some clue about the structure of the network and
  wants to manually select the elimination order, this can be set
  through the form \code{EliminationOrder(net)<-nodelist}.  Here
  \code{nodelist} should be a complete list of all of the nodes in
  \code{net} with no duplication.  Alternatively, it can be set to
  \code{NULL}.

  Setting the elimination order does not affect an already compiled
  network, it is only is applied when the network is next compiled.
}
\value{
  A list of all of the nodes in the network in elimination order if the
  elimination order is currently set, otherwise \code{NULL}.

  The setter form returns \code{net} invisibly.
}
\references{

  Almond, R.G. (1995) \emph{Graphical Belief Modeling}.  Chapman and Hall.

\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GetNetElimOrder_bn}, \nref{SetNetElimOrder_bn}, 
}
\author{
  Russell Almond
}
\note{
  The Netica documentation does not specify the heuristics for selecting
  the elimination order if no order is specified.  I suspect it is some
  variation on the greedy algorithm, which works well in many cases.
}
\seealso{
  \code{\link{NeticaBN}}, \code{\link{NetworkAllNodes}()},
  \code{\link{CompileNetwork}()}, \code{\link{JunctionTreeReport}()}
}
\examples{

EMSMMotif <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","EMSMMotif.dne",
                           sep=.Platform$file.sep))

## Should be null before we do anything.
stopifnot(
 is.null(EliminationOrder(EMSMMotif))
)

CompileNetwork(EMSMMotif)
## Now should have an elimination order.
stopifnot(
 length(EliminationOrder(EMSMMotif)) ==
 length(NetworkAllNodes(EMSMMotif)),
 NetworkCompiledSize(EMSMMotif) == 84
)
JunctionTreeReport(EMSMMotif)

## EMSMMotif is partitioned into observable and proficiency variables.
## Tell Netica to eliminate observable variables first.
EliminationOrder(EMSMMotif) <- c(NetworkNodesInSet(EMSMMotif,"Observable"),
                                 NetworkNodesInSet(EMSMMotif,"Proficiency"))
UncompileNetwork(EMSMMotif)
CompileNetwork(EMSMMotif)
stopifnot(
 length(EliminationOrder(EMSMMotif)) ==
 length(NetworkAllNodes(EMSMMotif)),
 NetworkCompiledSize(EMSMMotif) == 84
)
JunctionTreeReport(EMSMMotif)

## Clear elimination order.
EliminationOrder(EMSMMotif) <- NULL
stopifnot(
 is.null(EliminationOrder(EMSMMotif))
)

DeleteNetwork(EMSMMotif)

}

\keyword{ interface }
\keyword{ utility }% __ONLY ONE__ keyword per line
