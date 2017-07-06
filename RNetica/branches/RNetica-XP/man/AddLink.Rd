\name{AddLink}
\alias{AddLink}
\alias{DeleteLink}
\title{
  Adds or removes a link between two nodes in a Netica network.
}
\description{
  Add link adds an edge from \code{Parent} to \code{Child}.  Delete Link
  removes that edge.  This states that the distribution of \code{child}
  will be specified conditional on the value of \code{parent}.
  Consequently, adding or removing edges with affect the
  conditional probability tables associated with the \code{Child} node
  (see \code{\link{NodeProbs}()}.)
}
\usage{
AddLink(parent, child)
DeleteLink(parent, child)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{parent}{
    A \code{\link{NeticaNode}} representing an independent variable to
    be added to the conditioning side of the relationship.  The nodes
    \code{parent} and \code{child} must both be in the same network.
  }
  \item{child}{
    A \code{\link{NeticaNode}} representing dependent variable to be
    added to the conditioning side of the relationship.
  }
}
\details{
  After adding a link \code{parent --> child}, it may be the case that
  \code{parent} is in \code{\link{NodeParents}(child)} and \code{child}
  is a member of \code{\link{NodeChildren}(parent)}.  If \code{child}
  already has other parents, then the new parent will be added to the
  end of the list.  The order of the parents can be set by setting
  \code{NodeParents(child)}.  

  In general, the Bayesian network must always be an acyclic directed
  graph.  Therefore, if \code{parent} is a descendant of \code{child}
  (that is if \code{\link{is.NodeRelated}(child)}, "descendant",
  \code{child} is \code{TRUE}), then Netica will generate an error.

  The function \code{DeleteLink()} removes the relationship, and the
  \code{parent} and \code{child} nodes should no longer be in each
  other parent and child lists.  The parent list of the child node is
  shortened (a stub node for later reconnection is not created as when
  \code{NodeParents(child)[i] <- list(NULL)}).  

}
\value{
  The function \code{AddLinK} invisibly returns the index of the new
  parent in the parent list.

  The function \code{DeleteLink} invisibly returns the child node.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{AddLink_bn}, \nref{DeleteLink_bn}
}
\author{
  Russell Almond
}
\note{
  The Netica API specifies the first argument to \code{DeleteLink_bn()}
  as an index into the parent list. RNetica maps from the node to the
  index. 
}
\seealso{
  \code{\link{NeticaNode}}, \code{\link{NodeParents}()},
  \code{\link{NodeChildren}()}, \code{\link{is.NodeRelated}()}
}
\examples{

abnet <- CreateNetwork("AABB")
A <- NewDiscreteNode(abnet, "A")
B <- NewDiscreteNode(abnet, "B")

AddLink(A,B)

stopifnot(
  match(A,NodeParents(B),nomatch=0)>0,
  match(B,NodeChildren(A),nomatch=0)>0
)

DeleteLink(A,B)

stopifnot(
  match(A,NodeParents(B),nomatch=0)==0,
  match(B,NodeChildren(A),nomatch=0)==0
)

DeleteNetwork(abnet)
}
\keyword{ interface}
\keyword{ graphs }
\keyword{ manip }% __ONLY ONE__ keyword per line
