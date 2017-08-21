\name{ReverseLink}
\alias{ReverseLink}
\title{
  Reverses a link in a Netica network.
}
\description{

  This reverses the link between \var{parent} and \var{child} so that
  it now points from \var{child} to \var{parent}.  If \var{child} has
  additional parents, they are connected to \var{parent} and the
  conditional probability tables are adjusted so that the joint
  probability distribution across all nodes in the network remains the same.
}
\usage{
ReverseLink(parent, child)
}
\arguments{
  \item{parent}{
    An active \code{\linkS4class{NeticaNode}} which is currently a parent of
    \var{child} and which will be the child after the transformation.
  }
  \item{child}{
    An active \code{\linkS4class{NeticaNode}} which is currently a child of
    \var{parent} and which will be the parent after the transformation.
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
  \code{\linkS4class{NeticaNode}}, \code{\link{AddLink}()},
  \code{\link{NodeChildren}()}, \code{\link{NodeParents}()},
  \code{\link{AbsorbNodes}()}, \code{\link{is.NodeRelated}()}
}
\examples{
sess <- NeticaSession()
startSession(sess)

abcnet <- CreateNetwork("ABC", session=sess)

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
stopSession(sess)

}
\keyword{ interface }
\keyword{ graphs }
