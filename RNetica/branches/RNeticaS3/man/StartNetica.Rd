\name{StartNetica}
\alias{StartNetica}
\alias{StopNetica}
\alias{LicenseKey}
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
    is \code{NULL} the limited student/demonstration
    version of Netica is used rather than the full version.  If the
    variable \code{NeticaLicenseKey} is set before \code{RNetica} is
    loaded, then the value of that variable at the time the package is
    loaded will become the default for \code{license}.
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
  be purchased from Norsys (\url{http://www.Norsys.com/}).  They will
  send a license key which unlocks the full capabilities of the shared
  library.  This can be passed as the first argument to
  \code{StartNetica()}.  If the value of the first argument is
  \code{NULL} then the demonstration version is used instead of the
  licensed version (could be useful for testing).

  If you set the value of a variable \code{NeticaLicenseKey}, then when
  \code{RNetica} is loaded, then its value at the time the
  package is loaded is used as the default value for \code{license}. 
  If no value for as \code{NeticaLicenseKey}, the default value for
  \code{license} is set to \code{NULL}, which loads the demo version of
  \code{Netica}. 

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

  The function \code{StartNetica} is called when the package is attached
  (in the \code{.onAttach()} function.  The function \code{StopNetica()}
  is called by \code{.Last.lib()}, Normally, users should not need to
  call these functions, but they may wish to do so if they need to
  restart Netica with different arguments.

}
\value{
  These functions are called for side effects and do not return
  meaningful values.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{NewNeticaEnviron_ns}, \nref{InitNetica2_bn},
  \nref{CloseNetica_bn}, \nref{LimitMemoryUsage_ns},
  \nref{ArgumentChecking_ns} 
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
  Norsys (\url{http://norsys.com/}).

  RNetica itself (the glue layers between R and Netica) is free (in both
  the speech and beer senses) software.  Suggestions for improvements
  and bug fixes are welcome.
}
\section{Implementation Notes}{
  I'm looking into a way to bury the license key into RNetica during
  the installation process.  Probably will happen in a future version.
  Until then, best bet is to save a value for \code{NeticaLicenseKey} in
  the workspace.

  The Netica environment pointer, which is used by the Netica shared
  library is defined inside of the RNetica shared library, and not
  visible at the R level.
}
\seealso{
  \code{\link{NeticaVersion}()}, \code{\link{CreateNetwork}()}
}
\examples{
\dontrun{
## Restart licensed version
StopNetica()
StartNetica("License key from Norsys")                  
## Get the version of Netica.
print(NeticaVersion())

## Commonly done next step is to create a network.
net1 <- CreateNetwork("myNet")

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ interfaces }
\keyword{ environment }% __ONLY ONE__ keyword per line
