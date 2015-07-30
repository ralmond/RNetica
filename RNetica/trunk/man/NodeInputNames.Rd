\name{NodeInputNames}
\alias{NodeInputNames}
\alias{NodeInputNames<-}
\title{
  Associates names with incoming edges on a Netica node.
}
\description{
  The function \code{NodeInputNames()} can be used to set or retrieve
  names for each of the parents of \code{node}.  This facilitates
  operations such as copying and reconnecting the nodes.
}
\usage{
NodeInputNames(node)
NodeInputNames(node) <- value
}
\arguments{
  \item{node}{
    A \code{\link{NeticaNode}} object whose parent link names will be
    retrieved or set.
  }
  \item{value}{
    A character vector of length \code{length(\link{NodeParents}(node)}
    giving the new names.  Names must conform to the
    \code{\link{IDname}} convention.
    }
}
\details{
  When a parent node is detached from a child, Netica names the link
  with the name of the old node.  For example, suppose that the
  following commands were executed \code{AddLink(A,C); AddLink(B,C)}.
  Then if the node \code{B} is detached, via
  \code{\link{NodeParents}(C)[2]<-list(NULL)}, Netica will replace \code{B}
  with a stub node, and name the link \code{"B"}.  The command
  \code{NodeParents(C)$B <- D} would then attach the node \code{D} where
  the old node was attached.

  Rather than relying on the automatic naming scheme, the node names can
  be directly set using \code{NodeInputNames(node)<-newvals}.  Netica
  will not rename a detached link if there already exists a name for
  that link.  Explicitly naming the links rather than relying on
  Netica's naming scheme is probably good practice.  If node input names
  are set, then they will be used names for the return value of
  \code{NodeParents()} 

  The getter form \code{NodeInputNames()} returns the currently set
  names of the input links.  If an input link whose name has not been
  set either directly or via inserting a \code{NULL} in
  \code{NodeParents()} has a name of \code{""}.
}
\value{
  The function \code{NodeInputNames()} returns a character vector of
  the same length as \code{GetNodeParents()} giving the current names of
  the links.  If a link has not yet been named, the corresponding entry
  of the vector will be the empty string.

  The setter function returns the node object invisibly.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GetNodeInputNames_bn}, \nref{SetNodeInputNames_bn},
  \nref{SwitchNodeParent_bn}
}
\author{
  Russell Almond
}
\note{
  To detach a parent, you must use \code{list(NULL)} on the left hand
  side of \code{NodeParents(node)[i] <- list(NULL)} and not
  \code{NULL}.  
}
\seealso{
  \code{\link{NeticaNode}}, \code{\link{AddLink}()},
  \code{\link{NodeParents}()}
}
\examples{
abnet <- CreateNetwork("AB")

anodes <- NewDiscreteNode(abnet, paste("A",1:3,sep=""))
B <- NewDiscreteNode(abnet,"B")

NodeParents(B) <- anodes
stopifnot(
  all(NodeInputNames(B)=="")
)

NodeParents(B)[2] <- list(NULL)
stopifnot(
  NodeInputNames(B)==c("","A2","")
)

## Now can use A2 as name
D <- NewDiscreteNode(abnet,"D")
NodeParents(B)$A2 <- D
## But name doesn't change
stopifnot(
  NodeInputNames(B)==c("","A2","")
)

##Name the inputs
NodeInputNames(B) <- paste("Input",1:3,sep="")
stopifnot(
  names(NodeParents(B))[2]=="Input2"
)

## Now detaching nodes doesn't change input names.
NodeParents(B)[1] <- list(NULL)
stopifnot(
  NodeKind(NodeParents(B)[[1]])=="Stub",
  NodeInputNames(B)[1]=="Input1"
)

DeleteNetwork(abnet)
}
\keyword{ interface }
\keyword{ attribute }% __ONLY ONE__ keyword per line
