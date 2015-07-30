\name{ReverseLink}
\alias{ReverseLink}
\title{
  Reverses a link in a Netica network.
}
\description{
  This reverses the link between \code{parent} and \code{child} so that
  it now points from \code{child} to \code{parent}.  If \code{child} has
  additional parents, they are connected to \code{parent} and the
  conditional probability tables are adjusted so that the joint
  probability distribution across all nodes in the network remains the same.
}
\usage{
ReverseLink(parent, child)
}
\arguments{
  \item{parent}{
    An active \code{\link{NeticaNode}} which is currently a parent of
    \code{child} and which will be the child after the transformation.
  }
  \item{child}{
    An active \code{\link{NeticaNode}} which is currently a child of
    \code{parent} and which will be the parent after the transformation.
  }
}
\details{
  This is not just a simple reversal of a single edge, but rather the
  influence diagram operation of \emph{arc reversal}.  Netica will add
  additional links to enforce any conditional probability relationship.
  For example, Consider a net where \code{A} and \code{B} are both
  parents of \code{C}, but \code{A} is not directly connected to
  \code{C}.  After reversing the arc between \code{B} and \code{C},
  \code{A} will also become a parent of \code{B} to maintain the joint
  distribution. 
}
\value{
  Returns \code{NULL} if successful and \code{NA} if there was a problem.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{ReverseLink_bn}

  Shachter, R. D. (1986) "Evaluating Influence Diagrams."
  \emph{Operations Research}, \bold{34}, 871--82.
}
\author{
  Russell Almond
}
\seealso{
  \code{\link{NeticaNode}}, \code{\link{AddLink}()},
  \code{\link{NodeChildren}()}, \code{\link{NodeParents}()},
  \code{\link{AbsorbNodes}()}, \code{\link{is.NodeRelated}()}
}
\examples{
abcnet <- CreateNetwork("ABC")

A <- NewDiscreteNode(abcnet,"A")
B <- NewDiscreteNode(abcnet,"B")
C <- NewDiscreteNode(abcnet,"C")

AddLink(A,C)
AddLink(B,C)
stopifnot(
  is.NodeRelated(A,C,"parent"),
  is.NodeRelated(C,B,"child"),
  !is.NodeRelated(A,B,"parent")
)

ReverseLink(B,C)
stopifnot(
  is.NodeRelated(A,C,"parent"),
  is.NodeRelated(C,B,"parent"),
  is.NodeRelated(A,B,"parent")
)

DeleteNetwork(abcnet)

}
\keyword{ interface }
\keyword{ graphs }% __ONLY ONE__ keyword per line
