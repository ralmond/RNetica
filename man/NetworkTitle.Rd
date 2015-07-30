\name{NetworkTitle}
\alias{NetworkTitle}
\alias{NetworkTitle<-}
\alias{NetworkComment}
\alias{NetworkComment<-}
\title{
  Gets the title or comments associated with a Netica network.
}
\description{
  The title is a longer name for a network which is not subject to the
  Netica \code{\link{IDname}} restrictions.  The comment is a free form
  text associated with a network.
}
\usage{
NetworkTitle(net)
NetworkTitle(net) <- value
NetworkComment(net)
NetworkComment(net) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{
    A \code{\link{NeticaBN}} object.
  }
  \item{value}{
    A character object giving the new title or comment.
  }
}
\details{
  The title is meant to be a human readable alternative to the name,
  which is not limited to the \code{\link{IDname}} restrictions.  The
  title also affects how the network is displayed in the Netica GUI.

  The comment is any text the user chooses to attach to the network.  If
  \code{value} has length greater than 1, the vector is collapsed into a
  long string with newlines separating the components.
}
\value{
  A character vector of length 1 providing the title or comment.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GetNetTitle_bn}, \nref{SetNetTitle_bn},
  \nref{GetNetComments_bn}, \nref{SetNetComments_bn}
}
\author{
  Russell Almond
}
\seealso{
  \code{\link{NeticaBN}}, \code{\link{NetworkName}()} 

}
\examples{
firstNet <- CreateNetwork("firstNet")

NetworkTitle(firstNet) <- "My First Bayesian Network"
stopifnot(NetworkTitle(firstNet)=="My First Bayesian Network")

now <- date()
NetworkComment(firstNet)<-c("Network created on",now)
## Print here escapes the newline, so is harder to read
cat(NetworkComment(firstNet),"\n")
stopifnot(NetworkComment(firstNet) ==
  paste(c("Network created on",now),collapse="\n"))


DeleteNetwork(firstNet)
}
\keyword{ interface }
\keyword{ attribute }% __ONLY ONE__ keyword per line
