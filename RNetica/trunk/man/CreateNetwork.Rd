\name{CreateNetwork}
\alias{CreateNetwork}
\alias{DeleteNetwork}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Creates (destroys) a new Netica network.
}
\description{
  \code{CreateNetwork()} makes a new empty network in Netica.
  \code{DeleteNetwork()} frees the memory associated with the named
  network inside of Netica.
}
\usage{
CreateNetwork(names)
DeleteNetwork(nets)
}
\arguments{
  \item{names}{
    A character vector giving the name or names of the network to be created.
}
  \item{nets}{
    A list of \code{\link{NeticaBN}} objects to be destroyed or a
    character vector containing their names.
}
}
\details{
  The \code{CreateNetwork} method creates a new network for each of the
  named networks.  It returns a \code{NeticaBN} object, or a list of
  such objects if the argument \code{names} has length greater than 1.

  The \code{DeleteNetwork} method frees the Netica memory associated
  with each net in its argument.  Note that the network will not be
  available for use after it is deleted.  It returns a
  \code{\link{DeleteNeticaBN}} object, or a list of such objects if its
  argument has length greater than one.  If no network with the given
  name is found, it issues a warning and returns \code{NULL} for the
  corresponding result.

  Note that \code{nets} can either be a list of \code{NeticaBN} objects
  or a list of network names.  Internally, \code{DeleteNetwork()}
  searches for the networks by name.

  These functions wrap the Netica API functions \code{NewNet_bn()} and
  \code{DeleteNet_bn()}. 
}
\value{
  For \code{CreateNetwork()}, a single \code{NeticaBN} object if
  \code{length(names)==1}, and a list of such objects if
  \code{length(names)>1}.

  For \code{DeleteNetwork()}, a single \code{DeletedNeticaBN} object if
  \code{length(names)==1}, and a list of such objects if
  \code{length(names)>1}.  If the specified network does not exist, the
  corresponding element will be \code{NULL}.
  
}
\references{
  \link{http://norsys.com/onLineAPI/Manual/index.html}
}
\author{
  Russell Almond
}
\section{Implementation Note}{
  Currently, the \code{NeticaBN} object uses the name of the networks as
  the pointer into the network.  Thus either a character vector of names
  or a list of \code{NeticaBN} objects is mostly equivalent.

  Future versions may actually use pointers to the Netica objects.
}
\seealso{
  \code{\link{CopyNets}()}
}
\examples{
StartNetica()

net1 <- CreateNetwork("EmptyNet")
stopifnot(is(net1,"NeticaBN"))
stopifnot(as.character(net1)=="EmptyNet")

netd <- DeleteNetwork(net1)
stopifnot(is(netd,"DeletedNeticaBN"))
stopifnot(as.character(netd)=="EmptyNet")

StopNetica()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ interface }
\keyword{ utility }
\keyword{ model }% __ONLY ONE__ keyword per line
