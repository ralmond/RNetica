\name{NetworkSession}
\alias{NetworkSession}
\title{Returns the Netica session object associated with a network.}
\description{
  
  This returns the \code{\linkS4class{NeticaSession}} object in which
  the network was created.
}
\usage{
NetworkSession(net)
}
\arguments{
  \item{net}{A \code{\linkS4class{NeticaBN}} object.}
}
\value{
  The (possibly inactive) session object pointing to the Netica
  session in which the network was created.
}
\author{Russell Almond}
\seealso{
  \code{\linkS4class{NeticaSession}},
  \code{\linkS4class{NeticaBN}}
}
\examples{
SetupDefaultSession()

irt5 <- local_RNetica_net("IRT5.dne")

NetworkSession(irt5)

theta <- NetworkFindNode(irt5,"Theta")

NetworkSession(NodeNet(theta))

withr::deferred_run()  # Stops Netica.

}
\keyword{ utilities }
\keyword{ classes }

