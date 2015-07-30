\name{CreateNetwork}
\alias{CreateNetwork}
\alias{DeleteNetwork}
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
    A list of \code{\link{NeticaBN}} objects to be destroyed.
}
}
\details{

  The \code{CreateNetwork} method creates a new network for each of the
  names.  Names must follow the \code{\link{IDname}} rules.  It returns
  a \code{NeticaBN} object, or a list of such objects if the argument
  \code{names} has length greater than 1.

  The \code{DeleteNetwork} method frees the Netica memory associated
  with each net in its argument.  Note that the network will not be
  available for use after it is deleted.  It returns the
  \code{\link{NeticaBN}} objects, but modified so that they are no
  longer active.

  The function \code{link{is.active}()}, checks to see if the network
  associated with a \code{NeticaBN} object still corresponds to a
  network loaded into Netica's memory.

  These functions wrap the Netica API functions \code{NewNet_bn()} and
  \code{DeleteNet_bn()}. 
}
\value{
  A single \code{NeticaBN} object if the length of the argument is 1,
  and a list of such objects if the argument has length greater than 1.
  For \code{DeleteNets()} if a specified network does not exist, the
  corresponding element in the return list will be \code{NULL}.
  
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{NewNet_bn()}, \nref{DeleteNet_bn()}
}
\author{
  Russell Almond
}
\note{
  The function \code{DeleteNetwork()} implicitly deletes any nodes
  associated with the network.  Therefore, any nodes associated with
  this network will become inactive (see \code{\link{is.active}()}).
}
\section{Implementation Note}{
  Currently, the \code{NeticaBN} object uses the name of the networks as
  the pointer into the network.  Thus either a character vector of names
  or a list of \code{NeticaBN} objects is mostly equivalent.

  Future versions may actually use pointers to the Netica objects.
}
\seealso{
  \code{\link{CopyNetworks}()}, \code{\link{is.active}()}
}
\examples{

net1 <- CreateNetwork("EmptyNet")
stopifnot(is(net1,"NeticaBN"))
stopifnot(as.character(net1)=="EmptyNet")
stopifnot(is.active(net1))

netd <- DeleteNetwork(net1)
stopifnot(!is.active(netd))
stopifnot(!is.active(net1))
stopifnot(as.character(netd)=="EmptyNet")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ interface }
\keyword{ utilities }
