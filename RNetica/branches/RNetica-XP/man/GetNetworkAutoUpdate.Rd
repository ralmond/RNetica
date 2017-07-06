\name{GetNetworkAutoUpdate}
\alias{GetNetworkAutoUpdate}
\alias{SetNetworkAutoUpdate}
\alias{WithoutAutoUpdate}
\title{
  Turns Netica automatic updating on or off for a network.
}
\description{
  Netica networks can either propagate the effects of new findings
  immediately, or they can delay propagation until the user queries the
  network. These functions toggle the switch that controls the
  autoupdate mechanism
}
\usage{
GetNetworkAutoUpdate(net)
SetNetworkAutoUpdate(net, newautoupdate)
WithoutAutoUpdate(net,expr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{
    A \code{\link{NeticaBN}} object to be  queried or changed.
  }
  \item{newautoupdate}{
    A logical values, \code{TRUE} to turn automatic updating on.
    A value \code{NA} produces an error.
  }
  \item{expr}{
    An R expression to be evaluated with automatic updating turned off.
  }
  
}
\details{
  Automatic updating means that queries operate very quickly, however,
  if a large number of finding are to be entered before the next query,
  they can slow the network down.  These functions provide a mechanism
  for controlling that.

  \code{GetNetworkAutoUpdate()} returns the current status of the
  autoupdate flag.    \code{SetNetworkAutoUpdate()} sets flag, but
  returns its current value (to make it easier to restore).  The
  function \code{WithoutAutoUpdate} provides a mechanism for turning
  updating off while performing a series of operations.
}
\value{
  \code{GetNetworkAutoUpdate()} and \code{SetNetworkAutoUpdate} both
  returns the current autoupdate flag as a logical value.

  \code{WithoutAutoUpdate()} returns the value of executing \code{expr},
  unless executing \code{expr} results in an error in which case it
  returns a \code{try-error}.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{SetNetAutoUpdate_bn}, \nref{GetNetAutoUpdate_bn}
}
\author{
  Russell Almond
}
\note{
  Automatic updating makes a lot of sense when Netica is running under
  the GUI, but not so much when it is running as an API.  It is probably
  easiest to just set this to false all the time.
}

\seealso{
  \code{\link{NeticaBN}}, \code{\link{NodeBeliefs}()},
  \code{\link{NodeFinding}()} 
}
\examples{

autoNet <- CreateNetwork("AutomaticTest")

GetNetworkAutoUpdate(autoNet)

SetNetworkAutoUpdate(autoNet,FALSE)
stopifnot(!GetNetworkAutoUpdate(autoNet))
stopifnot(!SetNetworkAutoUpdate(autoNet,TRUE))
stopifnot(GetNetworkAutoUpdate(autoNet))

result <- TRUE
WithoutAutoUpdate(autoNet, result <<-GetNetworkAutoUpdate(autoNet))
stopifnot(!result)

DeleteNetwork(autoNet)

}
\keyword{ interface }
\keyword{ programming }
\keyword{ attribute }
\keyword{ utilities }% __ONLY ONE__ keyword per line
