\name{NetworkName}
\alias{NetworkName}
\alias{NetworkName<-}
\title{
  Gets or Sets the name of a Netica network.
}
\description{

  Gets or sets the name of the network. Names must conform to the
  \code{\link{IDname}} rules.

}
\usage{
NetworkName(net, internal=FALSE)
NetworkName(net) <- value
}
\arguments{
  \item{net}{
    A \code{\linkS4class{NeticaBN}} object which links to the network.
  }
  \item{internal}{A logical scalar.  If true, the actual Netica object
    will be consulted, if false, a cached value in the R object will be
    used.}
  \item{value}{
    A character scalar containing the new name.
  }
}
\details{
  Network names must conform to the \code{\link{IDname}} rules for
  Netica identifiers.  Trying to set the network to a name that does not
  conform to the rules will produce an error, as will trying to set the
  network name to a name that corresponds to another different network.

  The \code{\link{NetworkTitle}()} function provides another way to name
  a network which is not subject to the \code{IDname} restrictions.

  Note that the name of the network is stored in two places:  in the
  \code{Name} field of the \code{\linkS4class{NeticaBN}} object
  (\code{\var{net}$Name}), and internally in the Netica object.  These
  should be the same; however, may not be.  The \code{internal} field is
  used to force a check of the internal Netica object rather than the
  field in the R object.
}
\value{
  The name of the network as a character vector of length 1.

  The setter method returns the modified object.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
    \nref{GetNetName_bn}, \nref{SetNetName_bn}
}
\author{
  Russell Almond
}
\note{

  This paragraph is obsolete as of RNetica version 0.5, it describes the
  previous versions only.

  \code{NeticaBN} objects are internally implemented as character vectors
  giving the name of the network.  If a network is renamed, then it is
  possible that R will hold onto an old reference that still using the
  old name.  In this case, \code{NetworkName(net)} will give the correct
  name, and \code{GetNamedNets(NetworkName(net))} will return a
  reference to a corrected object.

  Starting with RNetica 0.5, \code{\linkS4class{NeticaBN}} objects are
  cached in the \code{\linkS4class{NeticaSession}} object.  The setter
  method for \code{NetworkName} updates the cache as well.

  In versions of RNetica less than 0.5, trying to set the name of a node
  to a name that was already used would generate a warning instead of an
  error.  It now generates an error.

}
\seealso{
  \code{\link{CreateNetwork}()}, \code{\linkS4class{NeticaBN}},
  \code{\link{GetNamedNetworks}()}, \code{\link{NetworkTitle}()} 
}
\examples{
sess <- NeticaSession()
startSession(sess)

net <- CreateNetwork("funNet", session=sess)
netcached <- net
stopifnot(!is.null(sess$findNet("funNet")))

stopifnot(NetworkName(net)=="funNet")
stopifnot(NetworkName(net,internal=TRUE)=="funNet")

NetworkName(net)<-"SomethingElse"
stopifnot(net$Name=="SomethingElse")
stopifnot(is.null(sess$findNet("funNet")))
stopifnot(!is.null(sess$findNet("SomethingElse")))

stopifnot(NetworkName(net)==NetworkName(netcached))
stopifnot(NetworkName(net)==NetworkName(netcached,internal=TRUE))

net1 <- CreateNetwork("funnyNet", session=sess)
cat("Next statement should generate an error message.\n")
nn <- try(NetworkName(net1) <- "SomethingElse") 
stopifnot(is(nn,"try-error"))

DeleteNetwork(net)
DeleteNetwork(net1)
stopSession(sess)
}
\keyword{ interface }
\keyword{ attribute }
