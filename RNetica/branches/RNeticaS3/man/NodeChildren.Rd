\name{NodeChildren}
\alias{NodeChildren}
\title{
  Returns a list of the children of a node in a Netica network.
}
\description{
  The children of a node \code{parent} are the nodes which are directly
  connected to \code{parent} with an edge oriented from
  \code{parent}. The function \code{NodeChildren(parent)} returns a list of the
  children of \code{parent}
}
\usage{
NodeChildren(parent)
}
\arguments{
  \item{parent}{
    A \code{\link{NeticaNode}} whose children are to be found.
  }
}
\details{
  The function \code{NodeChildren(parent)} only returns the immediate
  descendants of \code{parent}.  A list of all descendants can be found
  using the function
  \code{\link{GetRelatedNodes}(parent,"decendents")}.

  The function \code{link{NodeParents}()} returns the opposite end of the
  link, however, unlike \code{NodeParents()}, \code{NodeChildren()}
  cannot be directly set.
}
\value{
  A list (possibly empty) of \code{NeticaNode} objects which are the
  children of \code{parent}.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GetNodeChildren_bn}
}
\author{
  Russell Almond
}
\seealso{
  \code{\link{NeticaNode}}, \code{\link{AddLink}()},
  \code{\link{NodeParents}()}, \code{\link{GetRelatedNodes}()}
}
\examples{

chnet <- CreateNetwork("ChildcareCenter")

mom <- NewContinuousNode(chnet,"Mother")
stopifnot(
  length(NodeChildren(mom))==0
)

daughters <- NewDiscreteNode(chnet,paste("Daughter",1:3,sep=""))
sapply(daughters, function(d) AddLink(mom,d))

stopifnot(
  length(NodeChildren(mom))==3,
  all(match(daughters,NodeChildren(mom),nomatch=0))>0
)
  
DeleteNetwork(chnet)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ interface }
\keyword{ graphs }% __ONLY ONE__ keyword per line
