\name{NodeChildren}
\alias{NodeChildren}
\title{

  Returns a list of the children of a node in a Netica network.

}
\description{

  The children of a node \var{parent} are the nodes which are directly
  connected to \var{parent} with an edge oriented from
  \var{parent}. The function \code{NodeChildren(\var{parent})} returns a
  list of the children of \var{parent}
}
\usage{
NodeChildren(parent)
}
\arguments{
  \item{parent}{
    A \code{\linkS4class{NeticaNode}} whose children are to be found.
  }
}
\details{
  The function \code{NodeChildren(\var{parent})} only returns the immediate
  descendants of \code{parent}.  A list of all descendants can be found
  using the function
  \code{\link{GetRelatedNodes}(\var{parent},"decendents")}.

  The function \code{link{NodeParents}()} returns the opposite end of the
  link, however, unlike \code{NodeParents()}, \code{NodeChildren()}
  cannot be directly set.
}
\value{

  A list (possibly empty) of \code{\linkS4class{NeticaNode}} objects
  which are the children of \var{parent}.

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
  \code{\linkS4class{NeticaNode}}, \code{\link{AddLink}()},
  \code{\link{NodeParents}()}, \code{\link{GetRelatedNodes}()}
}
\examples{
sess <- NeticaSession()
startSession(sess)

chnet <- CreateNetwork("ChildcareCenter", session=sess)
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
stopSession(sess)

}
\keyword{ interface }
\keyword{ graphs }
