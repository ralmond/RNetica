\name{NeticaBN}
\alias{NeticaBN}
\alias{DeletedNeticaBN}
\alias{NeticaBN.toString}
\alias{NeticaBN.print}
\alias{DeletedNeticaBN.toString}
\alias{DeletedNeticaBN.print}

\title{
An object referencing a Bayesian network.
}
\description{
  This object is returned by various RNetica functions which create or
  find network objects, and contain handles to the Bayesian network.  A
  \code{NeticaBN} object represents an active network, and a
  \code{DeletedBN} object represents a stale network handle that should
  not be used.
}
\usage{
NeticaBN.toString(x, ...)
NeticaBN.print(x, ...)

DeletedNeticaBN.toString(x, ...)
DeletedNeticaBN.print(x, ...)
}
\arguments{
  \item{x}{The object to print
}
  \item{\dots}{Other arguments to \code{\link[base]{print}()} or
    \code{\link[base]{toString}()} 
}
}
\value{
  For \code{toString()} a string.  The function \code{print()} is
  usually called for its side effects.
}
\author{
  Russell Almond
}
\section{Implementation Notes}{
  Internally, the \code{NeticaBN} and \code{DeletedNeticaBN} objects are
  character strings with extra attributes.  So \code{as.character(net)}
  will return the name of the network.

  The current version of RNetica uses the name of the network as the
  object as the handle.  This is because it is generally a real pain to
  ensure that no R objects hang onto stale pointer references.  Thus,
  generally one can substitute the string in any place where a NeticaBN
  object is called for.

  The safer, but less efficient method is to call
  \code{\link{GetNamedNets}()} every time to force the string into a
  Bayesian network.  This will also check that a network of that name
  exists (returning \code{NULL} if it does not).

}

\seealso{
  \code{\link{CreateNetwok}()},\code{\link{DeleteNetwork}()},
  \code{\link{GetNamedNets}()}
}
\examples{
StartNetica()

net1 <- CreateNetwork("Untitled")
stopifnot(is(net1,"NeticaBN"))
stopifnot(as.character(net1)=="Untitled")

net2 <- GetNamedNets("Untitled")
stopifnot(is(net2,"NeticaBN"))
stopifnot(as.character(net2)=="Untitled")

netd <- DeleteNetwork(net2)
stopifnot(is(netn,"DeletedNeticaBN"))
stopifnot(as.character(net2)=="Untitled")

StopNetica()
}
\keyword{ classes }
\keyword{ print }% __ONLY ONE__ keyword per line
