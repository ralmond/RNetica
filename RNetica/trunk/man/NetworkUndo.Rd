\name{NetworkUndo}
\alias{NetworkUndo}
\alias{NetworkRedo}
\title{
  Undoes (redoes) a Netica operation on a network.
}
\description{
  Netica maintains an internal queue of reversible operations on a
  network.  The \code{NetworkUndo()} rolls them back off the stack.
  The \code{NetworkRedo()}.
}
\usage{
NetworkUndo(net)
NetworkRedo(net)
}
\arguments{
  \item{net}{
    A \code{\link{NeticaBN}} object on which an action took place.
}
}
\details{
  The details of which operations are undoable is not clearly
  documented in Netica.  Some obvious things, like adding nodes, do not
  appear to work.
}
\value{
  Returns an invisible integer which is the return code from the
  underlying network function.  Its value is not documented, other than
  it will be negative if the undo/redo stack is empty.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{UndoNetLastOper_bn}, \nref{RedoNetOper_bn}
}
\author{
  Russell Almond
}
\seealso{
  \code{\link{NeticaBN}}, \code{\link{CreateNetwork}}
}
\examples{
\dontrun{
activeNet <- CreateNetwork("undoRedoTest")

NewContinuousNode(activeNet,"Node1")
NewContinuousNode(activeNet,"Node2")
NewContinuousNode(activeNet,"Node3")

## These tests don't actually work, I'm not sure
## what constitutes an undoable action in Netica.
print(NetworkUndo(activeNet))
stopifnot(length(NetworkAllNodes(activeNet))==2)

print(NetworkUndo(activeNet))
stopifnot(length(NetworkAllNodes(activeNet))==1)

print(NetworkRedo(activeNet))
stopifnot(length(NetworkAllNodes(activeNet))==2)

DeleteNetwork(activeNet)
}
}
\keyword{ interface }
\keyword{ utilities }% __ONLY ONE__ keyword per line
