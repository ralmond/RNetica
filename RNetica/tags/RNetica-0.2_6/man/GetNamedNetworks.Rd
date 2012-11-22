\name{GetNamedNetworks}
\alias{GetNamedNetworks}
\title{
  Finds a Netica network (if it exists) for the name.
}
\description{
  This searches through the currently open Netica networks and returns a
  \code{\link{NeticaBN}} object pointing to the networks with the given
  names.  If no network with the name is found \code{NULL} is returned
  instead, so this provides a way to check whether a network exists.
}
\usage{
GetNamedNetworks(namelist)
}
\arguments{
  \item{namelist}{
    A character vector giving the name or names of the networks to be found.
}
}
\details{
  \code{GetNamedNetworks()} searches the list of network names looking for
  a network with the appropriate name.  If it is found, a handle to that
  network is returned as a \code{NeticaBN} object.  If not, \code{NULL}
  is returned.
  
}
\value{
  If \code{namelist} is of length 1, then a single \code{NeticaBN}
  object or \code{NULL} will be returned.

  If \code{namelist} is of length greater than 1, then a list of the
  same length as \code{namelist} is returned.  Each element is a
  \code{NeticaBN} related to the corresponding name or \code{NULL} if
  the name does not refer to a network.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:  \nref{GetNthNet_bn}
}
\author{
  Russell Almond
}
\note{
  This function does a linear search through all networks, so it could
  be pretty slow if there are a large number of networks open.
}
\seealso{
  \code{\link{CreateNetwork}()}, \code{\link{GetNthNetwork}()}
}
\examples{

net1 <- CreateNetwork("myNet")
## Fetch the network we just created by name.
net2 <- GetNamedNetworks("myNet")
stopifnot(is(net2,"NeticaBN"))
stopifnot(as.character(net1)==as.character(net2))
stopifnot(net1==net2)

## No network named "fish", this should return NULL
fish <- GetNamedNetworks("fish")
stopifnot(all(sapply(fish,is.null)))

DeleteNetwork(net2)

}
\keyword{ interface }
\keyword{ utilities }% __ONLY ONE__ keyword per line
