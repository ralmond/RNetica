\name{NetworkSetRNG}
\alias{NetworkSetRNG}
\title{Sets a random number generator associates with the network.
}
\description{
  This function creates a new random number generator using the given
  seed and associates it with the network.
}
\usage{
NetworkSetRNG(net, seed=runif(1,0,1000000000))
}
\arguments{
  \item{net}{An active \code{\link{NeticaBN}} whose random number
    generator is to be set.}
  \item{seed}{An unsigned integer to be uses as the seed.}
}
\details{

  Associating a random number generator with a Netica network has two
  effects.  First, if the seed is constant, then subsequent calls to
  \code{GenerateRandomCase} will create a reproducible sequence of
  cases.  Second, as the default random number generator Netica uses is
  threadsafe, the random number generation will be slightly faster.
}
\value{
  Returns the \code{net} argument.
}
\references{
  \newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{NewRandomGenerator_ns}, \nref{SetNetRandomGen_bn}

}
\author{Russell Almond}
\note{
  This function both creates the random number generator (see
  \code{\link{NewNeticaRNG}}) and associates it with the network
  argument.  Following the Netica API, it should be possible to separate
  the two operations, but it unclear what would happen if the RNG object
  was then freed (either manually or by associating it with another
  network and then deleting that other network).  It therefore seemed
  safer to encapsulate the RNG creation process in the C code.
}
\seealso{
  \code{\link{NewNeticaRNG}()}, \code{\link{GenerateRandomCase}()}
}
\examples{

rnet <- CreateNetwork("Random")
NetworkSetRNG(rnet, 1234469767)

DeleteNetwork(rnet)

}
\keyword{ interface }
\keyword{ datagen }
