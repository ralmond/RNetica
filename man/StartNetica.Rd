\name{StartNetica}
\alias{StartNetica}
\alias{StopNetica}
\alias{startSession}
\alias{stopSession}
\alias{restartSession}
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
startSession(session)
stopSession(session)
restartSession(session)
StartNetica(license = LicenseKey, checking = NULL, maxmem = NULL,
           session=NeticaSession(LicenseKey=license, checking=checking,
                                 maxmem=maxmem))
StopNetica(session=getDefaultSession())
}
\arguments{
  \item{session}{An object of class \code{\linkS4class{NeticaSession}}
    which encapsulates the connection to Netica.}
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
  The generic functions \code{startSession} and \code{stopSession} start
  and stop the connection to Netica.  Note that the session is active
  (\code{\link{is.active}}) if the session has been started, and
  inactive if it was stopped (or not yet started).  The generic function
  \code{restartSession} stops the session and starts it again.

  The functions \code{StartNetica} and \code{StopNetica} are depricated,
  but still included for backwards compatability.  The function
  \code{StartNetica} will now create a new object of class
  \code{\linkS4class{NeticaSession}} passing its arguments to the
  constructor.  It will also set a variable \code{DefaultNeticaSession}
  in the global environment to the new session.  As this is the value
  returned by \code{\link{getDefaultSession}()} this will cause RNetica
  to behave like the previous versions where the session pointer was
  stored internally to the C code.  The function \code{StopNetica}
  operates on the default session if no explicit argument is given.
  
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

  
  Prior to RNetica version 0.5, RNetica looked for a variable
  called \code{NeticaLicenseKey} in the global workspace when 
  \code{RNetica} is loaded.  This then becomes the default value for
  both \code{StartNetica} and \code{\link{getDefaultSession}()}.  
  If no value is set for \code{NeticaLicenseKey}, the default value for
  \code{license} is set to \code{NULL}, which loads the demo version of
  \code{Netica}.

  In version 0.5 and later of RNetica, the recommended way to store the
  license is to create a default session and store it in the variable
  \code{DefaultNeticaSession} in the global environment.  This session
  object then contains the license key.

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

  As of RNetica 0.5, the function \code{StartNetica} is no longer called
  when the package is attached (in the \code{.onAttach()} function).
  Instead, users should start Netica scripts with
  \code{startSession(DefaultNeticaSession)}.  Note that the pathnames of
  recently loaded networks are stored in the session object, so that
  networks can be quickly re-read from a saved session.

}
\value{
  These functions now all return an object of class
  \code{\linkS4class{NeticaSession}}.  Note that \code{StartNetica} sets
  the value of \code{DefaultNeticaSession} in the global environment to
  the value of the \code{session} argument.
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

  Starting with RNetica 0.5, the Netica environment pointer, which is
  used by the Netica shared library is stored inside of the
  \code{NeticaSession} object instead of as a global object in the C
  code.  The function \code{\link{is.active}()} checks to see whether
  that pointer is set (active) or null (inactive).  Note that when a
  session object is saved, the session pointer is not saved and should
  be set to null.

  The pre verison 0.5 method used a variable \code{NeticaLicenseKey} in
  the global environment to store the license key.  The post 0.5 method
  uses a variable \code{DefaultNeticaSession} to store a session object
  instead, but for backwards compatability, it will try to create a
  session using the value of \code{NeticaLicenseKey} if no default
  session exists.


}
\seealso{
  \code{\linkS4class{NeticaSession}}, \code{\link{NeticaSession}()},
  \code{\link{NeticaVersion}()}, \code{\link{CreateNetwork}()}
}
\examples{
\dontrun{
## Recommended way of doing things
DefaultNeticaSession <- NeticaSession(LicenseKey="Code from Norsys")
startSession(DefaultNeticaSession)

## If DefaultNeticaSession was created in a previous R session
## Re-read the networks.
for (netname in DefaultNeticaSession$listNets()) {
  net <- DefaultNeticaSession$findNet(netname)
  ReadNetworks(GetNetworkFileName(net),DefaultNeticaSession)
}

restartSession(DefaultNeticaSession)
stopSession(DefaultNeticaSession)

## Depricated methods
StartNetica("License key from Norsys")                  
StopNetica()
## Get the version of Netica.

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ interfaces }
\keyword{ environment }% __ONLY ONE__ keyword per line
