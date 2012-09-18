\name{NeticaVersion}
\alias{NeticaVersion}
\title{ Fetches the version number of Netica.
}
\description{
The version number of Netica is returned as both an integer and a string.
}
\usage{
NeticaVersion()
}
%- maybe also 'usage' for other objects documented here.
\details{
  This must be called after the call to \code{\link{StartNetica}()}.  
}
\value{
  \item{number }{Netica version number times 100 (to make it an integer).}
  \item{message }{String defining Netica version.}
}
\references{
  \link{http://norsys.com/onLineAPI/Manual/index.html}
}
\author{
  Russell Almond
}
\note{
  RNetica was developed with Netica API 5.04
}

\seealso{
  \code{\link{StartNetica}()}
}
\examples{
print(NeticaVersion()$message)
stopifnot(NeticaVersion$number>409) ## Version 4.09 is a popular one.
}
\keyword{ interface }
\keyword{ environment }% __ONLY ONE__ keyword per line
