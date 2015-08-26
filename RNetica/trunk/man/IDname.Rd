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
as.IDname(x,prefix="y",maxlen=25)
}
\arguments{
  \item{x}{
    A character vector of possible identifier names.
  }
  \item{prefix}{
    A character scalar that provides an alphabetic prefix for names that
    start with an illegal character.
  }
  \item{maxlen}{
    The maximum number of characters to use in the converted name, which
    should be less than Netica's maximum of 30 characters.
  }
}
\details{

  Netica identifiers (net names, node names, state names,  and similar)
  are limited to 30 characters which must be a valid letter, number or
  the character '_'.  The first character must be a letter.  The function
  \code{is.IDname()} tests to see if a string conforms to these rules,
  and thus is a legal name.

  The function \code{as.IDname()} attempts to coerce its argument into
  the IDname format by applying the following transformations.
  \enumerate{
    \item{The argument is coerced into a character vector.}
    \item{If any value begins with a nonalphabetic character, the
      \code{prefix} argument is prepended to all values.}
    \item{All non-alphanumeric characters are converted to '_'.}
    \item{Each value is truncated to \code{maxlen} characters in length.}
  }

  The truncation works by the following mechanism:
  \enumerate{
    \item{The string is truncated to length \code{maxlen-3}.}
    \item{The UTF 8 values of the remaining characters is summed and the
      result is taken modulo 100 to provide a 2-digit hash code for the
      remaining characters.}
    \item{The hash code is appended to the end of the truncated string
      separated with an \code{_}.
    }
  }
  This should result in strings which are likely, but not guaranteed to
  be unique if the difference between two names is only after the last
  \code{maxlen-3} characters.

  Note that although Netica allows variable names up to 30 characters in
  length, in some cases (particularly when stub variables are created
  after separating an edge from its parent) Netica creates new variable
  names by appending characters onto existing ones.  That is why the
  recommended value for \code{maxlen} is set to 25.
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
          c(TRUE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE),
as.IDname(c("aFish","Wanda1","feed me","fish_food","1more","US$",
                      "a123456789012345678901234567890")) ==
          c("aFish","Wanda1","feed_me","fish_food","y1more","US_",
                      "a123456789012345678901_25")
)
}
\keyword{ interface }
\keyword{ utilities }% __ONLY ONE__ keyword per line
