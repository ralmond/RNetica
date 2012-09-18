\name{NetworkUndo}
\alias{NetworkUndo}
\alias{NetworkRedo}
\title{
  Undoes (redoes) a Netica operation on a network.
}
\description{
  Netica mantians an internal queue of reversable operations on a
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
  documented in Netica.
}
\value{
  Returns an invisible integer which is the return code from the
  underlying network function.  Its value is not documented, other than
  it will be negative if the undo/redo stack is empty.
}
\references{
    \link{http://norsys.com/onLineAPI/Manual/index.html}
}
\author{
  Russell Almond
}
\seealso{
  \code{\link{NeticaBN}}, \code{\link{CreateNetwork}}
}
\examples{
activeNet <- CreateNetwork("undoRedoTest")

## This test doesn't work, because setting the comment/title
## is not undoable.  Need to add a test later.
NetworkTitle(activeNet) <- "State 1"
NetworkTitle(activeNet) <- "State 2"
NetworkTitle(activeNet) <- "State 3"

print(NetworkUndo(activeNet))
##stopifnot(NetworkTitle(activeNet)=="State2")

print(NetworkUndo(activeNet))
##stopifnot(NetworkTitle(activeNet)=="State1")

print(NetworkRedo(activeNet))
##stopifnot(NetworkTitle(activeNet)=="State2")

}
\keyword{ interface }
\keyword{ utility }% __ONLY ONE__ keyword per line
