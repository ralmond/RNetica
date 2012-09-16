\name{NeticaBN}
\alias{NeticaBN}
\alias{NeticaBN.toString}
\alias{NeticaBN.print}
\alias{is.NeticaBN}
\alias{is.active}

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

}
\arguments{
  \item{x}{The object to print or test
}
  \item{\dots}{Other arguments to \code{\link[base]{print}()} or
    \code{\link[base]{toString}()}
  }
  \item{bn}{A \code{NeticaBN} object to test.}
}
\details {
  This is an argument of class \code{NeticaBN}.  It consists of a name,
  and an invisible handle to a Netica network.  The function
  \code{is.active()} test the state of that handle and returns
  \code{FALSE} if the network is no longer in active memory (usually
  because of a call to to 

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
  the network is subsquently renamed (with a call to \code{RenameNets}),
  the old object may persist with the wrong name.

}
\seealso{
  \code{\link{CreateNetwok}()},\code{\link{DeleteNetwork}()},
  \code{\link{GetNamedNets}()},\code{\link{RenameNets}()}
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
