\name{GetNthNetwork}
\alias{GetNthNetwork}
\title{

  Fetch a Netica network by its position in the Netica list.

}
\description{

  Fetches networks according to an internal sequence list of networks
  maintained inside of Netica.  If the number passed is greater than the
  number of currently defined networks, this function will return \code{NULL}

}
\usage{

GetNthNetwork(n, session = getDefaultSession())

}
\arguments{
  \item{n}{A vector of integers greater than 1.
  }
  \item{session}{An object of class \code{\linkS4class{NeticaSession}}
    which provides the link to the Netica environment.  If not supplied,
    then the default value is the value of the function
    \code{\link{getDefaultSession}()} which is usually the value of
    \code{DefaultNeticaSession} in the global environment.}
}
\details{

  The primary use for this function is probably to loop through all open
  networks.  As this function will return \code{NULL} when there are no
  more networks, that can be used to terminate the loop.

  Note that the sequence numbers can change, particularly after functions
  that open and close networks.

  This is a wrapper for the Netica function \code{GetNthNet_bn()}.

  Starting with RNetica 0.5, the session object is a container which
  contains the open networks, so this function is no longer really
  needed. 

}
\value{

  If \code{n} is of length 1, then a single \code{\linkS4class{NeticaBN}}
  object or \code{NULL} will be returned.

  If \code{n} is of length greater than 1, then a list of the
  same length as \code{n} is returned.  Each element is a
  \code{NeticaBN} related or \code{NULL} if the number is greater than
  the number of open networks.

}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GetNthNet_bn} 
}
\author{
  Russell Almond
}
\note{
  The Netica shared library uses a zero-based reference (i.e., the first
  net is 0), but this function subtracts 1 from the argument, so it uses
  a one-based reference system (the first net is 1).
}
\seealso{
  \code{\linkS4class{NeticaSession}},
  \code{\link{CreateNetwork}()}, \code{\link{GetNamedNetworks}()}
}
\examples{
sess <- NeticaSession()
startSession(sess)

foo <- CreateNetwork("foo",sess)
bar <- CreateNetwork("bar",sess)

count <- 1
while (!is.null(net <- GetNthNetwork(count,sess))) {
  cat("Network number ",count," is ",NetworkName(net),".\n")
  count <- count +1
}
cat("Found ",count-1," networks.\n")
stopifnot(count==3L)

stopSession(sess)

}
\keyword{ interface }
\keyword{ utilities }
