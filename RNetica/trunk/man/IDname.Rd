\name{IDname}
\alias{is.IDname}
\alias{as.IDname}
\alias{IDname}
\title{
  Tests to see if a string is a valid as a Netica Identifier.
}
\description{

  The function \code{is.IDname()} returns a logical vector indicating
  whether or not each element of \code{x} is a valid Netica identifier.
  The function \code{is.IDname()} attempts to massage the input value to
  conform to the IDname rules.

}
\usage{
is.IDname(x)
as.IDname(x,prefix="y")
}
\arguments{
  \item{x}{
    A character vector of possible identifier names.
  }
  \item{prefix}{
    A character scalar that provides an alphabetic prefix for names that
    start with an illegal character.
    }
}
\details{

  Netica identifiers (net names, node names, state names,  and similar)
  are limited to 30 characters which must be a valid letter, number of
  the character '_'.  The first chacter must be a letter.  The function
  \code{is.IDname()} tests to see if a string conforms to these rules,
  and thus is a legal name.

  The function \code{as.IDname()} attempts to coerce its argument into
  the IDname format by applying the following transformations.
  \enumerate{
    \item{The argument is coerced into a character vector.}
    \item{If any value begins with a nonalphabetic character, the
      \code{prefix} argument is prepended to all values.}
    \item{All non-alphanumeric characters are converted to '_'.}
    \item{Each value is truncated to 30 characters in length.}
  }
  Note that these rules guarentee that the result will conform to the
  IDname convention.  They do not guarentee that the resulting values
  will be unique.  In particular, watch out for long names that differ
  only in the last few characters.
}
\value{
  A logical vector of the same length of \code{x}.
}
\references{
  \url{http://norsys.com/onLineAPIManual/index.html}
}
\author{
  Russell Almond
}
\note{
  This is primarily a utility for doing argument checking inside of
  functions that require a Netica IDname.
}
\seealso{
  \code{\link{CreateNetwork}()}, \code{\link{NewDiscreteNode}()},
  \code{\link{NodeStates}()}, \code{\link{NodeName}()},
  \code{\link{NodeInputNames}()},
}
\examples{
stopifnot(
is.IDname(c("aFish","Wanda1","feed me","fish_food","1more","US$",
                      "a123456789012345678901234567890")) ==
          c(TRUE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE)
as.IDname(c("aFish","Wanda1","feed me","fish_food","1more","US$",
                      "a123456789012345678901234567890")) ==
          c("yaFish","yWanda1","yfeed_me","yfish_food","y1more","yUS_",
                      "ya1234567890123456789012345678")
)
}
\keyword{ interface }
\keyword{ utilities }% __ONLY ONE__ keyword per line
