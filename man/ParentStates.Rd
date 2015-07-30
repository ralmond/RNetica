\name{ParentStates}
\alias{ParentStates}
\alias{ParentNames}
\title{
  Returns a list of the names of the states of the parents of a Netica node.
}
\description{
  This function returns a list each of whose elements is a character
  vector giving the states of the parent variables (i.e., the result of
  calling \code{\link{NodeStates})} on each of the elements of
  \code{\link{NodeParents}(node)}).   The names of this list are the
  names assigned to the edges through \code{\link{NodeInputNames}(node)},
  or the names of the parent variables if edge names were not supplied.
}
\usage{
ParentStates(node)
ParentNames(node)
}
\arguments{
  \item{node}{
    An active \code{\link{NeticaNode}} object whose parent states are to
    be determined.
}
}
\value{
  For \code{ParentStates(node)}, named list where each element
  corresponds to the states of a parent variable.  If \code{node} has no
  parents, it returns a list of length 0.

  The function \code{ParentName(node)} returns
  \code{names(ParentNames(node)}, only is faster.
}
\author{
  Russell Almond
}
\note{
  This is a slightly more sophisticated version of
  \code{lapply(NodeParents(node), NodeStates)}.  It does minimal
  checking so that it can be fast.
}
\seealso{
  \code{\link{NodeStates}()}, \code{\link{NodeParents}()},
  \code{\link{NodeInputNames}()}
}
\examples{
abc1 <- CreateNetwork("ABC1")
A <- NewDiscreteNode(abc1,"A",c("A1","A2","A3","A4"))
B <- NewDiscreteNode(abc1,"B",c("B1","B2","B3"))
C <- NewDiscreteNode(abc1,"C",c("C1","C2"))

stopifnot(
  length(ParentStates(A)) == 0
)

AddLink(A,B)

Bpars <- ParentStates(B)
stopifnot(
  length(Bpars) == 1,
  names(Bpars) == "A",
  Bpars$A==NodeStates(A)
)

AddLink(A,C)
AddLink(B,C)

NodeInputNames(C) <- c("A_type","B_type")

Cpars <- ParentStates(C)
stopifnot(
  length(Cpars) == 2,
  names(Cpars) == c("A_type","B_type"),
  Cpars[[1]]==NodeStates(A),
  Cpars$B_type==NodeStates(B)
)

DeleteNetwork(abc1)

}
\keyword{ interface }
\keyword{ utility }% __ONLY ONE__ keyword per line
