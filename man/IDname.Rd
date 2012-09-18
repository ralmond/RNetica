\name{IDname}
\alias{is.IDname}
\alias{IDname}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Tests to see if a string is a valid as a Netica Identifier.
}
\description{

  This function returns a logical vector indicating whether or not each
  element of \code{x} is a valid Netica identifier.

}
\usage{
is.IDname(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    A character vector of possible identifier names.
}
}
\details{

  Netica identifiers (net names, node names and similar) are limited to
  30 characters which must be a valid letter, number of the character
  '_'.  The first chacter must be a letter.

}
\value{
  A logical vector of the same length of \code{x}.
}
\references{
    \link{http://norsys.com/onLineAPI/Manual/index.html}
}
\author{
  Russell Almond
}
\note{
  This is primarily a utility for doing argument checking inside of
  functions that require a Netica IDname.
}
\seealso{
  \code{\link{CreateNets}()}
}
\examples{
stopifnot(is.IDname(c("aFish","Wanda1","feed me","fish_food","1more","US$",
                      "a123456789012345678901234567890"))
          ==c(TRUE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE))
}
\keyword{ interface }
\keyword{ utility }% __ONLY ONE__ keyword per line
