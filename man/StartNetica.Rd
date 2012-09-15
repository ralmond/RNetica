\name{StartNetica}
\alias{StartNetica}
\alias{StopNetica}
\alias{LicenseKey}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Starting and stopping the Netica shared library.
}
\description{
  This function creates (or destroys) a Netica environment.  The
  \code{StartNetica} function also allows you to set various parameters
  associated with the Netica environment.
}
\usage{
StartNetica(license = LicenseKey, checking = NULL, maxmem = NULL)
StopNetica()
}
\arguments{
  \item{license}{A string containing a license key from Norsys.  If this
    is \code{null} (the default) the limited student/demonstration
    version of Netica is used rather than the full version.
}
  \item{checking}{A character string containing one of the keywords:
    \code{"NO_CHECK"}, \code{"QUICK_CHECK"}, \code{"REGULAR_CHECK"},
    \code{"COMPLETE_CHECK"}, or \code{"QUERY_CHECK"}, which controls how
    rigorous Netica is about checking errors. A value of \code{NULL}
    uses the Netica default which is \code{"REGULAR_CHECK"}.
}
  \item{maxmem}{An integer containing the maximum amount of memory to be
    used by the Netica shared library in bytes.  If supplied, this
    should be at least 200,000.
}
}
\details{
  The function \code{StartNetica()} calls the Netica functions
  \code{NewNeticaEnviron_ns()} and \code{InitNetica2_bn()} to create and
  set up a Netica environment.

  Netica is commercial software.  The RNetica package downloads and
  installs the demonstration version of Netica which is limited in its
  functionality (particularly in the size of the networks it handles).
  Unlocking the full version of Netica requires a license key which can
  be purchased from Norsys (\link{http://www.Norsys.com/}).  They will
  send a license key which unlocks the full capabilities of the shared
  library.  This can be passed as the first argument to
  \code{StartNetica()}.  The variable \code{RNetica::LicenseKey} is used
  as the default value.  As the package is distributed, this is set to
  \code{NULL}.  If you set the value of this variable, this will be used
  as the default in future calls to \code{StartNetica()}.

  The \code{checking} argument, if supplied, is used to call the Netica
  function \code{ArgumentChecking_ns()}.  See the documentation of that
  function for the meaning of the codes.  The default value,
  \code{"REGULAR_CHECK"} is appropriate for most development situations.

  The \code{maxmem} argument, if supplied, is used to limit the amount
  of memory used by Netica.  This is passed in a call to the Netica
  function \code{LimitMemoryUsage_ns()}.  Netica will complain if this
  value is less than 200,000.  Leaving this as \code{NULL} will not
  place limits on the size of Netica's memory for tables and things.

  The function \code{StopNetica()} calls the Netica function
  \code{CloseNetica_bn()}.  It is mainly used when one wants to stop
  Netica and restart it with other parameters.
}
\value{
  These functions are called for side effects and do not return
  meaningful values.
}
\references{
  \link{http://norsys.com/onLineAPI/Manual/index.html}
}
\author{Russell Almond
}
\section{License}{
  The Netica API is not free-as-in-speech software, the use of the
  Netica shared library makes you subject to the Netica License
  agreement (which can be found in the RNetica folder in your R library.
  If you do not agree to the terms of that license, please uninstall RNetica.

  The Netica API is also not free-as-in-beer software.  The demonstration
  version of the Netica API, however, is.  In order for you to make full
  use of the RNetica API, you must purchase a Netica API license from
  Norsys.

  RNetica itself (the glue layers between R and Netica) is free (in both
  the speech and beer senses) software.  Suggestions for improvements
  and bug fixes are welcome.
}
\section{Implementation Notes}{
Eventually, \code{StartNetica()} and \code{StopNetica()} will be called
  by the \code{.onload()} function for RNetica.  Until that time, users
  will need to call them by hand.

  The Netica environment pointer, which is used by the Netica shared
  library is defined inside of the RNetica shared library, and not
  visible at the R level.
}
\seealso{
  \code{\link{NeticaVersion}()}, \code{\link{CreateNetwork}()}
}
\examples{
StartNetica()
## Get the version of Netica.
print(NeticaVersion())

## Commonly done next step is to create a network.
net1 <- CreateNetwork("myNet")

## Lots of other operations, then when done.
StopNetica()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ interfaces }
\keyword{ environment }% __ONLY ONE__ keyword per line
