\name{dgetFromString}
\alias{dgetFromString}
\alias{dputToString}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Serializes an R object to a string}
\description{
  The function \code{dputToString} converts an R object to a string
  which can then be turned back into an R object using
  \code{dgetFromString}. 
}
\usage{
dgetFromString(str)
dputToString(obj)
}
\arguments{
  \item{str}{A string containing a serialized object}
  \item{obj}{An object to be serialized}
}
\details{
  These functions call the base R functions \code{\link[base]{dget}} and
  \code{\link[base]{dput}} using a string buffer as the connection.
  Thus, they serialize the R object and return a string value which can
  be stored in a \code{\link{NeticaNode}} (see
  \code{\link{NodeUserObj}}) or or \code{\link{NeticaBN}} (see
  \code{\link{NetworkUserObj}}).

  Note that the object must be self-contained.
}
\value{
  The function \code{dputToString} returns a character scalar containing
  the serialized object.  Note:  Sometimes R \dQuote{helpfully} adds
  line breaks, returning a vector of strings.  This can be fixed by
  using \code{paste(dputToString(obj),collapse=" ")}.
  

  The function \code{dgetFromString} returns an arbitrary R object
  depending on what was stored in \code{str}.

}
\author{Russell Almond}
\seealso{
  \code{\link{NodeUserObj}}), \code{\link{NetworkUserObj}}
}
\examples{
x <- sample(1L:10L)

x1 <- dgetFromString(dputToString(x))

stopifnot(all(x==x1))

}
\keyword{ manip }

