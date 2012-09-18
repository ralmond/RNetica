\name{NetworkName}
\alias{NetworkName}
\alias{NetworkName<-}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Gets or Sets the name of the network.
}
\description{
  Gets or sets the name of the network.
}
\usage{
NetworkName(net)
NetworkName(net) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{
    A \code{\link{NeticaBN}} object which links to the network.
}
  \item{value}{
    A character scalar containing the new name.
}
}
\details{
  Network names must conform to the \code{\link{IDname}} rules for
  Netica identifiers.  Trying to set the network to a name that does not
  conform to the rules will produce an error, as will trying to set the
  network name to a name that corresponds to another different network.
}
\value{
  The name of the network as a character vector of length 1.

  The setter method returns the modified object.
}
\references{
    \link{http://norsys.com/onLineAPI/Manual/index.html}
}
\author{
  Russell Almond
}
\note{
  Netica BN objects are internally implemented as character vectors
  giving the name of the network.  If a network is renamed, then it is
  possible that R will hold onto an old reference that still using the
  old name.  In this case, \code{NetworkName(net)} will give the correct
  name, and \code{GetNamedNets(NetworkName(net))} will return a
  reference to a corrected object.
}
\seealso{
  \code{\link{CreateNetwork}()}, \code{\link{NeticaBN}},
  \code{\link{GetNamedNetworks}()}, \code{\link{NetworkTitle}()} 
}
\examples{
StartNetica()

net <- CreateNetwork("myNet")
netcached <- net

stopifnot(NetworkName(net)=="myNet")

NetworkName(net)<-"SomethingElse"
stopifnot(as.character(net)=="SomethingElse")

##Warning, the following expression is true!
as.character(netcached) != NetworkName(netcached)
## But this one holds
stopifnot(NetworkName(net)==NetworkName(netcached))
## And this one
stopifnot(net==netcached)

## This fixes the problem
NetworkName(netcached) <- NetworkName(netcached)
stopifnot(as.character(netcached) != NetworkName(netcached))

StopNetica()
}
\keyword{ interface }
\keyword{ utility }% __ONLY ONE__ keyword per line
