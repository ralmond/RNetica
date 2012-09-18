\name{WriteNetworks}
\alias{WriteNetworks}
\alias{ReadNetworks}
\alias{GetNetworkFileName}
\title{
  Reads or writes a Netica network from a file.
}
\description{

  This function writes a Netica network to a \code{.neta} or \code{.dne}
  file or reads a network written by such a file.  This allows networks
  created with RNetica to be shared with other Netica users.

}
\usage{
WriteNetworks(nets, paths)
ReadNetworks(paths)
GetNetworkFileName(net)
}
\arguments{
  \item{nets}{
    A single \code{\link{NeticaBN}} object or a list of such objects.
}
  \item{net}{
    A single \code{\link{NeticaBN}} object.
}
  \item{paths}{
    A character vector of pathnames to \code{.neta} files.  For
    \code{ReadNetworks()}, the pathnames must exist. For
    \code{WriteNetworks()}, the \code{length(paths)} must equal
    \code{length(nets)} .
}
}
\details{
  This method invokes the native Netica open and save functions to read
  and write networks to \code{.neta} or \code{.dne} files.  The
  \code{.neta} format is binary and more compact, while the \code{.dne}
  format is ascii and may be safer in some circumstances (such as when
  used with a source control system).  Netica figures out which format
  to use based on the extension of the file, elments of \code{paths}
  should end with \code{.neta} or \code{.dne}.

  The function \code{GetNetworkFileName()} returns the name of the last
  file this network was saved to or read from.  It cannot be set other
  than through the \code{WriteNetworks()} or \code{ReadNetworks()}
  functions. 
}
\value{
  Both \code{ReadNetworks()} and \code{WriteNetworks()} return a list of
  \code{NeticaBN} objects corresponding to the new networks.  In the
  case of a problem with one of the networks, the corresponding entry
  will be set to \code{NULL}.  If the return list has length 1, a single
  \code{NeticaBN} object will be returned instead of a list.
}
\references{
    \link{http://norsys.com/onLineAPI/Manual/index.html}
}
\author{
  Russell Almond
}
\note{
  The demonstration version of Netica is limited to the size of the
  networks it will write (the limit is somewhere around 10 nodes).  If
  you are running across errors saving large networks, you need to
  purchase a Netica API license from Norsys (\link{http://norsys.com/}).
  
  \code{ReadNetworks()} and \code{WriteNetworks()} are vectorized, and
  can take either scalars or vectors as arguments (thus, a whole
  collection of networks can be read or written at once).  When the
  argument is a scalar, a scalar is returned.  This is probably the 80\% case,
  but may produce unexpected behavior in certain coding circumstances.
}
\seealso{
  \code{\link{NeticaBN}}, \code{\link{CreateNetwork}()}
}
\examples{
StartNetica()

peanut <- CreateNetwork("peanut")
NetworkTitle(peanut) <- "The Peanut Network"
peanutFile <- tempfile("peanut",fileext=".dne")
WriteNetworks(peanut,peanutFile)
stopifnot(GetNetworkFileName(peanut)==peanutFile)

pecan <- CreateNetwork("pecan")
NetworkTitle(pecan) <- "The Pecan Network"
pecanFile <- tempfile("pecan",fileext=".dne")
almond <- CreateNetwork("almond")
NetworkTitle(almond) <- "The Almond Network"
almondFile <- tempfile("almond",fileext=".neta")
WriteNetworks(list(pecan,almond),c(pecanFile,almondFile))

DeleteNetwork(peanut)
DeleteNetwork(pecan)
DeleteNetwork(almond)
stopifnot(!is.active(almond))

peanut <- ReadNetworks(peanutFile)
stopifnot(is.active(peanut))
stopifnot(NetworkTitle(peanut)=="The Peanut Network")

nets <- ReadNetworks(c(pecanFile,almondFile))
stopifnot(length(nets)==2)
stopifnot(all(sapply(nets,is.active)))
stopifnot(NetworkTitle(nets[[1]])=="The Pecan Network")
almond <- GetNamedNetworks("almond")
stopifnot(is.NeticaBN(almond),is.active(almond))

DeleteNetwork(peanut)
DeleteNetwork(nets[[1]])
DeleteNetwork(almond)

StopNetica()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
