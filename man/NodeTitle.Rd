\name{NodeTitle}
\alias{NodeTitle}
\alias{NodeTitle<-}
\alias{NodeDescription}
\alias{NodeDescription<-}
\title{
  Gets the title or Description associated with a Netica node.
}
\description{
  The title is a longer name for a node which is not subject to the
  Netica \code{\link{IDname}} restrictions.  The description is a free form
  text associated with a node.
}
\usage{
NodeTitle(node)
NodeTitle(node) <- value
NodeDescription(node)
NodeDescription(node) <- value
}
\arguments{
  \item{node}{
    A \code{\link{NeticaNode}} object.
  }
  \item{value}{
    A character object giving the new title or description.
  }
}
\details{
  The title is meant to be a human readable alternative to the name,
  which is not limited to the \code{\link{IDname}} restrictions.  The
  title also affects how the node is displayed in the Netica GUI.

  The description is any text the user chooses to attach to the node.  If
  \code{value} has length greater than 1, the vector is collapsed into a
  long string with newlines separating the components.
}
\value{
  A character vector of length 1 providing the title or description.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GetNodeTitle_bn}, \nref{SetNodeTitle_bn},
  \nref{GetNodeComments_bn}, \nref{SetNodeComments_bn}
}
\author{
  Russell Almond
}
\note{
  Node descriptions are called "Descriptions" in the Netica GUI, but
  "Comments" in the API.
}
\seealso{
  \code{\link{NeticaNode}}, \code{\link{NodeName}()} 
}
\examples{
net2 <- CreateNetwork("secondNet")

firstNode <- NewDiscreteNode(net2,"firstNode")

NodeTitle(firstNode) <- "My First Bayesian Network Node"
stopifnot(NodeTitle(firstNode)=="My First Bayesian Network Node")

now <- date()
NodeDescription(firstNode)<-c("Node created on",now)
stopifnot(NodeDescription(firstNode) ==
  paste(c("Node created on",now),collapse="\n"))

## Print here escapes the newline, so is harder to read
cat(NodeDescription(firstNode),"\n")

DeleteNetwork(net2)
}
\keyword{ interface }
\keyword{ attribute }% __ONLY ONE__ keyword per line
