\name{NeticaBN}
\alias{NeticaBN}
\alias{NeticaBN.toString}
\alias{NeticaBN.print}
\alias{is.NeticaBN}
\alias{is.active}
\alias{Ops.NeticaBN}

\title{
An object referencing a Bayesian network.
}
\description{
  This object is returned by various RNetica functions which create or
  find network objects, and contain handles to the Bayesian network.  A
  \code{NeticaBN} object represents an active network.  The function
  \code{is.active()} tests whether the network is still loaded into
  Netica's memory.

}
\usage{
NeticaBN.toString(x, ...)
NeticaBN.print(x, ...)
is.NeticaBN(x)
is.active(bn)
Ops.NeticaBN(e1, e2)
e1 == e2
e1 != e2

}
\arguments{
  \item{x}{The object to print or test
  }
  \item{\dots}{Other arguments to \code{\link[base]{print}()} or
    \code{\link[base]{toString}()}
  }
  \item{bn}{A \code{NeticaBN} object to test.
  }
  \item{e1}{A \code{NeticaBN} object to test.
  }
  \item{e2}{A \code{NeticaBN} object to test.
  }
}
\details{
  This is an argument of class \code{NeticaBN}.  It consists of a name,
  and an invisible handle to a Netica network.  The function
  \code{is.active()} test the state of that handle and returns
  \code{FALSE} if the network is no longer in active memory (usually
  because of a call to \code{DeleteNetwork()}.

  For active networks, the equality test tests to see if both object
  point to the same object in Netica memory.  Not that the name of the
  network is embedded in the object implementation and may get out of
  sync with the network, so the printed representations may be unequal
  even if it points to the same neetwork.  For inactive networks, the
  objects are compared using the cached names.

  

}
\value{
  For \code{toString()} a string.  The function \code{print()} is
  usually called for its side effects.

  The function \code{is.NeticaBN()} returns a logical scalar depending
  on whether or not its argument is a \code{NeticaBN}.

  The function \code{is.active()} returns \code{TRUE} if the argument
  still points to a network loaded in Netica's memory, and \code{FALSE}
  if that network has been deleted.  It returns \code{NA} if the
  argument is not a \code{NeticaBN}.

}
\author{
  Russell Almond
}
\note{
  Internally, the \code{NeticaBN} and \code{DeletedNeticaBN} objects are
  character strings with extra attributes.  So \code{as.character(net)}
  will return the name of the network.

  Note that if a \code{NeticaBN} object is stored in an R object, and
  the network is subsquently renamed (with a call to the set method of
  \code{NetworkName}), the old object may persist with the wrong name.
  This may result in a situation where the printed names of the objects
  are different but \code{net1==net2} returns true.  This can be fixed
  with the code \code{NetworkName(net) <- NetworkName(net)}.

}
\seealso{
  \code{\link{CreateNetwok}()},\code{\link{DeleteNetwork}()},
  \code{\link{GetNamedNetworks}()},\code{\link{NetworkName}()}
}
\examples{

net1 <- CreateNetwork("Untitled")
stopifnot(is.NeticaBN(net1))
stopifnot(is.active(net1))
stopifnot(as.character(net1)=="Untitled")

net2 <- GetNamedNetworks("Untitled")
stopifnot(as.character(net2)=="Untitled")
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
\keyword{ interface }% __ONLY ONE__ keyword per line
