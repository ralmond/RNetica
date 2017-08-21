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
ReadNetworks(paths, session)
GetNetworkFileName(net, internal=FALSE)
}
\arguments{
  \item{nets}{
    A single \code{\linkS4class{NeticaBN}} object or a list of such objects.
  }
  \item{net}{
    A single \code{\linkS4class{NeticaBN}} object.
  }
  \item{paths}{
    A character vector of pathnames to \code{.neta} files.  For
    \code{ReadNetworks()}, the pathnames must exist. For
    \code{WriteNetworks()}, the \code{length(\var{paths})} must equal
    \code{length(\var{nets})}.  For \code{WriteNetworks()} if \code{paths} are
    missing, then \code{GetNetworkFileName()} will be called to try and
    determine any path associated with the node.
  }
  \item{session}{An object of type \code{\linkS4class{NeticaSession}}
    which defines the reference to the Netica workspace.  Read networks
    will be created in this session.}
  \item{internal}{A logical scalar.  If true, the actual Netica object
    will be consulted, if false, a cached value in the R object will be
    used.}
}
\details{

  This method invokes the native Netica open and save functions to read
  and write networks to \code{.neta} or \code{.dne} files.  The
  \code{.neta} format is binary and more compact, while the \code{.dne}
  format is ASCII and may be safer in some circumstances (such as when
  used with a source control system).  Netica figures out which format
  to use based on the extension of the file, elements of \code{paths}
  should end with \code{.neta} or \code{.dne}.

  The function \code{GetNetworkFileName()} returns the name of the last
  file this network was saved to or read from.  It cannot be set other
  than through the \code{WriteNetworks()} or \code{ReadNetworks()}
  functions.  Note that the filename is saved in both the R object and
  the Netica object.  If \code{internal=TRUE}, then the Netica object
  will be consulted.  This will raise an error if the \var{net} is not
  currently active.

  To facilitate saving and restoring files across R sessions, both
  \code{ReadNetworks()} and \code{WriteNetworks()} attach a
  \code{"Filename"} attribute to the object, which records the file just
  read or written.  \code{GetNetworkFileName(\var{net},internal=TRUE)}
  will not work after quitting and restarting Netica, but
  \code{net$PathnameName} should contain the same pathname.  If
  \code{ReadNetworks()} is passed a \code{NeticaBN} object (or a list of
  such objects), it will attempt to read from the file referenced by the
  \code{"Filename"} attribute.  Thus, calling \code{\var{net} <-
  WriteNetworks(\var{net},\var{path})} right before shutting down R and
  \code{\var{net} <- ReadNetworks(\var{net},\var{session})} right after
  the call to \code{\link{startSession}()}
  should restore the network.
  
}
\value{

  Both \code{ReadNetworks()} and \code{WriteNetworks()} return a list of
  \code{\linkS4class{NeticaBN}} objects corresponding to the new
  networks.  In the case of a problem with one of the networks, the
  corresponding entry will be set to \code{NULL}.  If the return list
  has length 1, a single \code{NeticaBN} object will be returned instead
  of a list. 

  A \code{"PathnameName"} field is added to the
  \code{\linkS4class{NeticaBN}} object 
  that is returned.  This can be used to restore \code{NeticaBN} objects
  after an R session is restarted.

}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{WriteNet_bn}, \nref{ReadNet_bn} 
}
\author{
  Russell Almond
}
\note{

  The demonstration version of Netica is limited to the size of the
  networks it will write (the limit is somewhere around 10 nodes).  If
  you are running across errors saving large networks, you need to
  purchase a Netica API license from Norsys (\url{http://norsys.com/}).
  
  \code{ReadNetworks()} and \code{WriteNetworks()} are vectorized, and
  can take either scalars or vectors as arguments (thus, a whole
  collection of networks can be read or written at once).  When the
  argument is a scalar, a scalar is returned.  This is probably the 80\% case,
  but may produce unexpected behavior in certain coding circumstances.

  Netica does not seem to expand the character '~' to your home
  directory (unlike R which follows the Unix convention in this
  regard). 

}
\seealso{
  \code{\linkS4class{NeticaBN}}, \code{\link{CreateNetwork}()},
  \code{\link{NetworkFindNode}()} (for recreating links to nodes after
  restoring a net)
}
\examples{
sess <- NeticaSession()
startSession(sess)

peanut <- CreateNetwork("peanut", session=sess)
NetworkTitle(peanut) <- "The Peanut Network"
peanutFile <- tempfile("peanut",fileext=".dne")
WriteNetworks(peanut,peanutFile)
stopifnot(GetNetworkFileName(peanut)==peanutFile)

pecan <- CreateNetwork("pecan", session=sess)
NetworkTitle(pecan) <- "The Pecan Network"
pecanFile <- tempfile("pecan",fileext=".dne")
almond <- CreateNetwork("almond", session=sess)
NetworkTitle(almond) <- "The Almond Network"
almondFile <- tempfile("almond",fileext=".neta")
WriteNetworks(list(pecan,almond),c(pecanFile,almondFile))

DeleteNetwork(peanut)
DeleteNetwork(pecan)
DeleteNetwork(almond)
stopifnot(!is.active(almond))

peanut <- ReadNetworks(peanutFile, session=sess)
stopifnot(is.active(peanut))
stopifnot(NetworkTitle(peanut)=="The Peanut Network")

nets <- ReadNetworks(c(pecanFile,almondFile), session=sess)
stopifnot(length(nets)==2)
stopifnot(all(sapply(nets,is.active)))
stopifnot(NetworkTitle(nets[[1]])=="The Pecan Network")
almond <- GetNamedNetworks("almond", session=sess)
stopifnot(is.NeticaBN(almond),is.active(almond))

DeleteNetwork(peanut)
DeleteNetwork(nets[[1]])
DeleteNetwork(almond)
stopSession(sess)

\dontrun{
## Safe way to preserve node and network objects across R sessions.
tnet <- WriteNetworks(tnet,"Tnet.neta")
q(save="yes")
# R
library(RNetica)
sess <- startSession(getDefaultSession())
tnet <- ReadNetworks(tnet, session=sess)
nodes <- NetworkFindNodes(tnet,tnet$listNodes())
}

}
\keyword{ interface }
\keyword{ IO }
