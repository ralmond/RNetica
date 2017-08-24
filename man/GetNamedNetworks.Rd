\name{GetNamedNetworks}
\alias{GetNamedNetworks}
\alias{CheckNamedNetworks}
\title{
  Finds a Netica network (if it exists) for the name.
}
\description{
  This searches through the currently open Netica networks in the
  \code{\linkS4class{NeticaSession}} and returns a
  \code{\linkS4class{NeticaBN}} object pointing to the networks with the given
  names.  If no network with the name is found \code{NULL} is returned
  instead, so this provides a way to check whether a network exists.

  \code{CheckNamedNetworks} checks the internal Netica list of networks,
  not the networks cached in the \code{\linkS4class{NeticaSession}}
  object, so it can be used to check for inconsistencies.
}
\usage{
GetNamedNetworks(namelist, session=getDefaultSession())
CheckNamedNetworks(namelist, session=getDefaultSession())
}
\arguments{
  \item{namelist}{
    A character vector giving the name or names of the networks to be found.
}
  \item{session}{An object of type \code{\linkS4class{NeticaSession}}
    which defines the reference to the Netica workspace.}
}
\details{
  \code{GetNamedNetworks()} searches the list of network names looking for
  a network with the appropriate name.  If it is found, a handle to that
  network is returned as a \code{\linkS4class{NeticaBN}} object.  If
  not, \code{NULL} is returned.  Note that if a network of the specified
  name existed, it could return an inactive
  \code{\linkS4class{NeticaBN}} object corresponding to the deleted
  network, so it is probably good to check the result with
  \code{\link{is.active}}.

  There are two ways that RNetica can check for a network of a given
  name.  The first the network cache maintained by the
  \code{\link{NeticaSession}} object (\code{session$nets}).  The
  function \code{GetNamedNetworks} just checks the cache, so it should
  be relatively fast.  The function \code{CheckNamedNetworks} iterates
  through all of the networks in Netica's internal memory, so it should
  be slower, but should also spot problems with RNetica and Netica
  getting out of sync.
  
}
\value{
  If \code{namelist} is of length 1, then a single
  \code{\linkS4class{NeticaBN}} 
  object or \code{NULL} will be returned.

  If \code{namelist} is of length greater than 1, then a list of the
  same length as \code{namelist} is returned.  Each element is a
  \code{\link{NeticaBN}} related to the corresponding name or \code{NULL} if
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

  Each \code{\linkS4class{NeticaBN}} is given a name when it is
  created.  When the network is created, either through a call to
  \code{\link{CreateNetwork}} or \code{\link{ReadNetworks}}, the
  \code{\link{NeticaSession}} object updates its cache of the network
  names in its \code{nets} field. The \code{nets} field of the session
  object is an \code{\link[base]{environment}} which associate the
  network's name with a \code{\linkS4class{NeticaBN}} object.
  Internally, functions that return a \code{\linkS4class{NeticaBN}}
  object (primarily \code{\link{NodeNet}}), search the network cache in
  the session object for the network with the corresponding name.

  \code{GetNamedNetworks} uses the cache (which is hashed) and so should
  be fairly fast.

  \code{CheckNamedNetworks} does a linear search through all networks,
  so it could be pretty slow if there are a large number of networks
  open.  It should raise an error if the cache and the internal Netica
  specs are out of sync.
}
\seealso{
  \code{\link{CreateNetwork}()}, \code{\link{GetNthNetwork}()}
}
\examples{
sess <- NeticaSession()
startSession(sess)

net1 <- CreateNetwork("myNet", session=sess)
## Fetch the network we just created by name.
net2 <- GetNamedNetworks("myNet", session=sess)
stopifnot(is(net2,"NeticaBN"))
stopifnot(NetworkName(net1)==NetworkName(net2))
stopifnot(net1==net2)

net3 <- CheckNamedNetworks("myNet", session=sess)
stopifnot(net1==net3)

## No network named "fish", this should return NULL
fish <- GetNamedNetworks("fish", session=sess)
stopifnot(all(sapply(fish,is.null)))
fish <- CheckNamedNetworks("fish", session=sess)
stopifnot(all(sapply(fish,is.null)))

DeleteNetwork(net1)
net1a <- GetNamedNetworks("myNet", session=sess)
stopifnot(NetworkName(net1a)=="myNet",!is.active(net1a))

net1b <- CheckNamedNetworks("myNet", session=sess)
stopifnot(is.null(net1b))

stopSession(sess)

}
\keyword{ interface }
\keyword{ utilities }
