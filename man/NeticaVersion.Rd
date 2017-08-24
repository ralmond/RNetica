\name{NeticaVersion}
\alias{NeticaVersion}
\title{ Fetches the version number of Netica.
}
\description{
The version number of Netica is returned as both an integer and a string.
}
\usage{
NeticaVersion(session=getDefaultSession())
}
\arguments{
  \item{session}{An object of type \code{\linkS4class{NeticaSession}}
    which defines the reference to the Netica workspace.}
}
\details{
  This is a synnonym for \code{\var{session}$neticaVersion()} (see
  \code{\linkS4class{NeticaSession}}). 

  This must be called after the call to \code{\link{StartNetica}()}.  
}
\value{
  A list with two elements:
  \item{number }{Netica version number times 100 (to make it an integer).}
  \item{message }{String defining Netica version.}
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GetNeticaVersion_bn} 
}
\author{
  Russell Almond
}
\note{
  RNetica was developed with Netica API 5.04
}

\seealso{
  \code{\linkS4class{NeticaSession}}
  \code{\link{StartNetica}()}
}
\examples{
sess <- NeticaSession()
startSession(sess)
print(sess$neticaVersion()$message)
stopifnot(NeticaVersion(sess)$number > 409) ## Version 4.09 is a popular one.
stopSession(sess)
}
\keyword{ interface }
\keyword{ environment }
