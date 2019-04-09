\name{NeticaBN}
\alias{NeticaBN}
\alias{is.NeticaBN}
\title{
An object referencing a Bayesian network in Netica.
}
\description{
  This file is now obsolete:  See \code{\linkS4class{NeticaBN}} for the
  new class description.

  This object is returned by various RNetica functions which create or
  find network objects, and contain handles to the Bayesian network.  A
  \code{NeticaBN} object represents an active network.  The function
  \code{is.active()} tests whether the network is still loaded into
  Netica's memory.

}
\usage{
is.NeticaBN(x)
}
\arguments{
  \item{x}{The object to print or test
  }
}
\details{
  This is an object of class \code{NeticaBN}.  It consists of a name,
  and an invisible handle to a Netica network.  The function
  \code{\link{is.active}()} tests the state of that handle and returns
  \code{FALSE} if the network is no longer in active memory (usually
  because of a call to \code{DeleteNetwork()}).  The printed
  representation depends on whether or not it is active (inactive nodes
  print as \code{"<Deleted Network: Name >"}).

  For active networks, the equality test tests to see if both object
  point to the same object in Netica memory.  Not that the name of the
  network is embedded in the object implementation and may get out of
  sync with the network, so the printed representations may be unequal
  even if it points to the same network.  For inactive networks, the
  objects are compared using the cached names.

}
\value{
  For \code{toString()} a string.  The function \code{print()} is
  usually called for its side effects.

  The function \code{is.NeticaBN()} returns a logical scalar depending
  on whether or not its argument is a \code{NeticaBN}.

  The function \code{Ops.NeticaBN()} returns a logical value depending on
  whether the objects are equal.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIurl/index.html}:
  \nref{GetNetUserData_bn}, \nref{SetNetUserData_bn} (these are used
  to maintain the back pointers to the R object).
}
\author{
  Russell Almond
}
\note{

  \code{NeticaBN} objects are all rendered inactive when
  \code{StopNetica()} is called, therefore they do not persist across R
  sessions.  Generally speaking, the network should be saved, using
  \code{\link{WriteNetworks}()} and then reloaded in the new session
  using \code{\link{ReadNetworks}()}.  When a network is saved or loaded
  the \code{"Filename"} attribute is set, to provide a mechanism for
  storing the filename across R sessions.
}
\seealso{
  \code{\link{CreateNetwork}()},\code{\link{DeleteNetwork}()},
  \code{\link{GetNamedNetworks}()},\code{\link{NetworkName}()},
  \code{\link{is.active}()}, \code{\link{NetworkAllNodes}()},
  \code{\link{WriteNetworks}()}, \code{\link{GetNetworkFileName}()},
  \code{\link{cc}()}
}
\examples{
\dontrun{
net1 <- CreateNetwork("aNet")
stopifnot(is.NeticaBN(net1))
stopifnot(is.active(net1))
stopifnot(net1$Name=="aNet")

net2 <- GetNamedNetworks("aNet")
stopifnot(net2$Name=="aNet")
stopifnot(net1==net2)

NetworkName(net1) <- "Unused"
stopifnot(net1==net2)

netd <- DeleteNetwork(net1)
stopifnot(!is.active(net1))
stopifnot(!is.active(net2))
stopifnot(netd$Name=="Unused")
stopifnot(netd == net1)
## Warning:  The following expression used to not be true (RNetica <0.5)
## but now is.
net1 = net2

}
}
\keyword{ classes }
\keyword{ graphs }
\keyword{ interface }
