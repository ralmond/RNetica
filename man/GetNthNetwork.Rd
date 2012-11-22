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
GetNthNetwork(n)
}
\arguments{
  \item{n}{A vector of integers greater than 1.
}
}
\details{
  The primary use for this function is probably to loop through all open
  networks.  As this function will return \code{NULL} when there are no
  more networks, that can be used to terminate the loop.

  Note that the sequence numbers can change, particularly after functions
  that open and close networks.

  This is a wrapper for the Netica function \code{GetNthNet_bn()}.
}
\value{
  If \code{n} is of length 1, then a single \code{NeticaBN}
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
  \code{\link{CreateNetwork}()}, \code{\link{GetNamedNetworks}()}
}
\examples{

count <- 1
while (!is.null(net <- GetNthNetwork(count))) {
  cat("Network number ",count," is ",net,".\n")
  count <- count +1
}
cat("Found ",count-1," networks.\n")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ interface }
\keyword{ utilities }% __ONLY ONE__ keyword per line
