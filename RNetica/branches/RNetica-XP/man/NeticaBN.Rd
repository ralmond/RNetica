\name{NeticaBN}
\alias{NeticaBN}
\alias{toString.NeticaBN}
\alias{print.NeticaBN}
\alias{is.NeticaBN}
\alias{Ops.NeticaBN}
\title{
An object referencing a Bayesian network in Netica.
}
\description{
  This object is returned by various RNetica functions which create or
  find network objects, and contain handles to the Bayesian network.  A
  \code{NeticaBN} object represents an active network.  The function
  \code{is.active()} tests whether the network is still loaded into
  Netica's memory.

}
\usage{
is.NeticaBN(x)
\method{toString}{NeticaBN}(x, ...)
\method{print}{NeticaBN}(x, ...)
\method{Ops}{NeticaBN}(e1, e2)
e1 == e2
e1 != e2

}
\arguments{
  \item{x}{The object to print or test
  }
  \item{\dots}{Other arguments to \code{\link[base]{print}()} or
    \code{\link[base]{toString}()}
  }
  \item{e1}{A \code{NeticaBN} object to test.
  }
  \item{e2}{A \code{NeticaBN} object to test.
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
  Internally, the \code{NeticaBN} objects are
  character strings with extra attributes.  So \code{as.character(net)}
  will return the name of the network.  Because of this, the default
  \code{\link[base]{c}()} function will strip off the essential
  attributes returning them to strings.  Use the \code{\link{cc}()}
  function instead to avoid this problem.

  Note that if a \code{NeticaBN} object is stored in an R object, and
  the network is subsequently renamed (with a call to the set method of
  \code{NetworkName}), the old object may persist with the wrong name.
  This may result in a situation where the printed names of the objects
  are different but \code{net1==net2} returns true.  This can be fixed
  with the code \code{NetworkName(net) <- NetworkName(net)}.

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

net1 <- CreateNetwork("aNet")
stopifnot(is.NeticaBN(net1))
stopifnot(is.active(net1))
stopifnot(as.character(net1)=="aNet")

net2 <- GetNamedNetworks("aNet")
stopifnot(as.character(net2)=="aNet")
stopifnot(net1==net2)

NetworkName(net1) <- "Unused"
stopifnot(net1==net2)
## Warning:  The following expression is true!
as.character(net1) != as.character(net2)

netd <- DeleteNetwork(net1)
stopifnot(!is.active(net1))
stopifnot(!is.active(net2))
stopifnot(as.character(netd)=="Unused")
stopifnot(netd == net1)
## Warning:  The following expression is true!
net1 != net2

}
\keyword{ classes }
\keyword{ graphs }
\keyword{ interface }% __ONLY ONE__ keyword per line
