\name{NeticaRNG}
\alias{NeticaRNG}
\alias{NewNeticaRNG}
\alias{FreeNeticaRNG}
\alias{WithRNG}
\alias{is.NeticaRNG}
\alias{isNeticaRNGActive}
\alias{toString.NeticaRNG}
\alias{print.NeticaRNG}
\title{Creates a Netica Random Number Generator}
\description{
  These functions create and manipulate Netica Random Number
  Generators. Note that the storage for NeticaRNG objects should be
  freed when you are done with them by calling \code{FreeNeticaRNG(rng)}.
}
\usage{
NewNeticaRNG(seed = runif(1, 0, 1e+09))
FreeNeticaRNG(rng)
WithRNG(rng,expr)
is.NeticaRNG(x)
isNeticaRNGActive(rng)
\method{toString}{NeticaRNG}(x,...)
\method{print}{NeticaRNG}(x,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{seed}{An unsigned integer to use as a seed for the random number
    generator. }
  \item{rng}{A NeticaRNG object}
  \item{x}{An arbitrary object}
  \item{expr}{An expression to be executed.}
  \item{...}{Other arguments to \code{toString}.  These are ignored.}
}
\details{

  Netica supports random number generator objects which serve can be
  used to generate random cases (\code{\link{GenerateRandomCase}()}).
  In either case explicitly creating a random number generator is
  optional.  If this is not done, the default random number generator is
  uses, which is slightly slower because it needs to be threadsafe.  As
  RNetica probably adds more overhead than the non-threadsafe RNG, the
  primary use for creating a NeticaRNG is to produce a reproducable
  sequence of random cases.

  Creating a random number generator with \code{rng <-
  NewNeticaRNG(seed)} generates an object in Netica space.  The memory
  for that object should be freed when that is complete.  The expression
  \code{FreeNeticaRNG(rng)} frees this object.  The function
  \code{WithRNG} can be used to execute code in a context where the RNG
  will be freed after after completion or in the case of early
  termination due to an error.  

  When the random number generator is freed, or if the R session or
  Netica session is terminated, the NeticaRNG object will become
  inactive.   The function \code{isNeticaRNGActive(rng)} tests to see if
  the random number generator is active (the Netica version still
  exists).  
  
}
\value{

  The value of \code{NewNetiaRNG(seed)} is an active NeticaRNG object.

  The value of \code{FreeRNG(rng)} is its argument which is now
  inactive.

  The value of \code{WithRNG(rng,expr)} is the result of evaluating
  \code{expr}.

  The values of \code{is.NeticaRNG(x)} and \code{isNeticaRNGActive(rng)}
  are logical scalars.
}
\references{
  \newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{NewRandomGenerator_ns},
  \nref{DeleteRandomGen_ns}
  \url{http://homepage.stat.uiowa.edu/~luke/R/references/weakfinex.html}
}
\author{Russell Almond}
\note{
  There are two other uses of newly created Netica RNG objects in the
  Netica Manual which are not currently supported by RNetica.  One is to
  simply generate uniform random numbers which seems superfluous given
  R's richer random number generation facilities.  The second it to
  associate the RNG with a network.  According to the manual, such RNGs
  no longer need to be deleted when you are done with them.  This seems
  like it could lead to a situation where a single RNG was associated
  with two networks and then the RNG was deleted when the first network
  was deleted.  Therefore the function \code{\link{NetworkSetRNG}()}
  always creates a new RNG.

  Internally, a weak reference system is used to keep a list of Netica
  RNG objects which need to be closed when RNetica is unloaded.
  RNG objects should also be forced closed when garbage collected.
  The weak reference system is somewhat experimental, so well designed
  code should manually close the RNG when the program is through
  with it.
}
\seealso{
  \code{\link{NetworkSetRNG}()}, \code{\link{GenerateRandomCase}()}
}
\examples{

rng <- NewNeticaRNG(123456789)

stopifnot(is.NeticaRNG(rng),
          isNeticaRNGActive(rng))

FreeNeticaRNG(rng)
stopifnot(!isNeticaRNGActive(rng))


}
\keyword{ interface }
\keyword{ datagen }
