\name{CopyNodes}
\alias{CopyNodes}
\title{
  Copies or duplicates nodes in a Netica network.
}
\description{
  This function either copies nodes from one net to another or
  duplicates nodes within the same network. 
}
\usage{
CopyNodes(nodes, newnamelist = NULL, newnet = NULL, options = character(0))
}
\arguments{
  \item{nodes}{
    A list of active \code{\link{NeticaNode}} objects all from the same
    network. 
  }
  \item{newnamelist}{
    If supplied, this should be character vector with the same length as
    \code{nodes} giving the new names for the nodes.
  }
  \item{newnet}{
    If supplied, it should be an active \code{\link{NeticaBN}} which is
    the destination for the new nodes.  If this argument is \code{NULL}
    the nodes will be duplicated within the original network.
  }
  \item{options}{
    A character vector of options, with each element being one of the
    options.  Currently, the only supported options are
    \code{"no_tables"} (do not copy the conditional probability tables
    for the nodes) and \code{"no_links"} (do not duplicate the links,
    which implies do not copy tables).
  }
}
\details{
  The nodes in the first argument will be copied into a new network as
  specified by \code{newnet}.  If \code{newnet} is not specified or if
  it the same as the network from which \code{nodes} come, then the
  nodes will be duplicated instead of copied.

  If the nodes are duplicated, then will be given new names.  The
  default Netica behavior for new names is to append a number to the end
  of the node name, or to increment an existing number.  If
  \code{newnamelist} is supplied, these names will be used instead of
  the add a number convention.  Supplying \code{newnamelist} will change
  the names of the nodes when copying from one network to another.

  When nodes are copied links going into the node are copied as well.
  Thus if there is a link \code{ A -> B} in the network and \code{B} is
  copied into the same network, then there will a link \code{ A -> B1}
  to the new node.  If \code{B} is copied into a new network, the link
  will be there but not attached, as if \code{\link{NodeParents}(B1)[A]
  <- NULL} had been called.
  
  The argument \code{options} allows control over what is copied.  The
  currently supported options are:
  \itemize{
  
  \item \code{"no_tables"} --- The conditional probability tables of
  the nodes (see \code{\link{NodeProbs}()}) will not be copied, and new
  tables will need to be set in the new network.  

  \item \code{"no_links"} --- The links going into the \code{nodes} will
  not be copied.  Note that the \code{"no_links"} option implies the
  \code{"no_tables"} option, so both do not need to be specified.
  }

}
  \value{
    A list containing the new nodes (or just the new node, if there is
    only one).
  }
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{CopyNodes_bn}
}
\author{
  Russell Almond
}
\note{

  There may be some information that is not copied.  For example, the
  \code{\link{NodeSets}()} information is not copied.
  
}
\seealso{
  \code{\link{CopyNetworks}()}, \code{\link{NeticaNode}},
  \code{\link{NeticaBN}()}, \code{\link{NodeProbs}()},
  \code{\link{NodeParents}()}, \code{\link{AbsorbNodes}()},
  \code{\link{DeleteNodes}()}

}
\examples{
System <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","System.dne",
                           sep=.Platform$file.sep))

EMTask1a <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","EMTask1a.dne",
                           sep=.Platform$file.sep))

student1 <- CopyNetworks(System, "Student1")
student1.sysnodes <- NetworkAllNodes(student1)

student1.t1anodes <- CopyNodes(NetworkAllNodes(EMTask1a),newnet=student1)

## Copied, new nodes have the same names as the old nodes.
stopifnot(
  setequal(names(NetworkAllNodes(EMTask1a)),
           names(student1.t1anodes))
)

## The nodes in the evidence model have stub connections to the nodes in
## the system model.  Need to link them up.
stopifnot(
  any(sapply(NodeParents(student1.t1anodes[[1]]),NodeKind) == "Stub"),
  any(sapply(NodeParents(student1.t1anodes[[2]]),NodeKind) == "Stub")
)


student1.allnodes <- NetworkAllNodes(student1)
for (node in student1.t1anodes) {
  stubs <- sapply(NodeParents(node),NodeKind) == "Stub"
  NodeParents(node)[stubs] <- student1.allnodes[NodeInputNames(node)[stubs]]
}
stopifnot(
  sapply(NodeParents(student1.t1anodes[[1]]),NodeKind) != "Stub",
  sapply(NodeParents(student1.t1anodes[[2]]),NodeKind) !="Stub"
)

## Duplicate these nodes.
student1.t1xnodes <- CopyNodes(student1.t1anodes)

## Autonaming increments the numbers.  
stopifnot(
  setequal(names(student1.t1xnodes),c("Obs1a3","Obs1a4"))
)

## Duplicate and rename.
student1.t1cnodes <- CopyNodes(student1.t1anodes,c("Obs1c1","Obs1c2"))

stopifnot(
  setequal(names(student1.t1cnodes),c("Obs1c1","Obs1c2"))
)
## Duplicated nodes have real not stub connections.
stopifnot(
  sapply(NodeParents(student1.t1cnodes[[1]]),NodeKind) != "Stub",
  sapply(NodeParents(student1.t1cnodes[[2]]),NodeKind) !="Stub"
)


DeleteNetwork(list(System,student1,EMTask1a))


}
\keyword{ interface }
\keyword{ manip }% __ONLY ONE__ keyword per line
