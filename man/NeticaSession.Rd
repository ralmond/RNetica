\name{NeticaSession}
\alias{NeticaSession}
\alias{getDefaultSession}
\title{Creates a connection between R and Netica}
\description{
  This function creates a \code{\linkS4class{NeticaSession}} object
  which encapsulates the link between R and Netica.  It also contains a
  collection of the networks associated with this Netica session.  
}
\usage{
NeticaSession(LicenseKey=character(),
              SessionName=paste("RNetica Session", date()),
              Checking=character(), maxmem=integer())
getDefaultNeticaSession()
}
\arguments{
  \item{LicenseKey}{If supplied, this should be a character scalar
    providing the license key purchased from Norsys
    \url{http://www.norsys.com/}.  If left as default, RNetica will run
    in a limited mode.}
  \item{SessionName}{A character vector giving an identifier for the
    session.  Only used in printing.}
  \item{Checking}{Object of class \code{character} one of the 
      keywords: 
    \code{"NO_CHECK"}, \code{"QUICK_CHECK"}, \code{"REGULAR_CHECK"},
    \code{"COMPLETE_CHECK"}, or \code{"QUERY_CHECK"}, which controls how
    rigorous Netica is about checking errors. A value of \code{character()}
    uses the Netica default which is \code{"REGULAR_CHECK"}.
  }
  \item{maxmem}{
Object of class \code{numeric} containing an
      integer indicating the maximum amount of memory to be
      used by the Netica shared library in bytes.  If supplied, this
      should be at least 200,000.
    }
}
\details{

  Starting with verison 0.5 of RNetica, in order to start Netica, you
  must first create an object of class
  \code{\linkS4class{NeticaSession}} and then call
  \code{\link{startSession}} on that object.  This object then contains
  a pointer to the Netica environment, and networks are created within
  the NeticaSession object.  

  Netica is commercial software.  The RNetica package downloads and
  installs the demonstration version of Netica which is limited in its
  functionality (particularly in the size of the networks it handles).
  Unlocking the full version of Netica requires a license key which can
  be purchased from Norsys (\url{http://www.Norsys.com/}).  They will
  send a license key which unlocks the full capabilities of the shared
  library.  This should be given as the \code{LicenseKey} argument to
  the constructor.  If no license key is applied, then Netica will run
  in a limited mode which limits the number of networks and nodes in the
  networks.  This is sufficient to run the test cases, and explore the
  capabilities, but for serious model building you will need to purchase
  a license.

  The \code{checking} argument, if supplied, is used to call the Netica
  function \code{ArgumentChecking_ns()}.  See the documentation of that
  function for the meaning of the codes.  The default value,
  \code{"REGULAR_CHECK"} is appropriate for most development situations.

  The \code{maxmem} argument, if supplied, is used to limit the amount
  of memory used by Netica.  This is passed in a call to the Netica
  function \code{LimitMemoryUsage_ns()}.  Netica will complain if this
  value is less than 200,000.  Leaving this as \code{NULL} will not
  place limits on the size of Netica's memory for tables and things.

  Prior to version 0.5, the Netica session pointer was managed inside of
  the c layer of RNetica.  Thus, the session was an implicit argument to
  several functions.  In particular, the functions
  \code{\link{CreateNetwork}}, \code{\link{GetNthNetwork}}, 
  \code{\link{GetNamedNetwork}}, and \code{\link{ReadNetworks}} all now
  have a session argument.  A session argument is
  also needed by some lower level functions which create Netica objects:
  \code{\link{CaseFileDelimiter}}, \code{\link{CaseFileMissingCode}},
  \code{\link{CaseFileStream}}, \code{\link{MemoryCaseStream}},
  \code{\link{OpenCaseStream}} and \code{\link{NewNeticaRNG}}.  For
  backwards compatability, the default for the session argument is now
  the value of \code{getDefaultSession()}.

  In the previous version the session was created an the
  \code{\link{StartNetica()}} function was called when the RNetica
  namespace was attached.  Thus the user did not need to worry about
  starting the Netica session.  To be backwards compatable, the function
  \code{getDefaultSession()} searches for a default NeticaSession object
  and if necessary creates one and starts it.

  The function \code{getDefaultSession()} does the following steps:
  \enumerate{
    \item{It first looks for a variable \code{DefaultNeticaSession} in
      the global environment.  If this exists, it will be used as the
      session.  If it does not exist, and R is running in
      \link[base]{interactive} mode, then the user will be prompted to
      create one.  If running in batch mode, or if the user does not want to
      create the default environment, then \code{getDefaultSession()} will
      raise an error.}
    \item{If creating a new session, it will look for a variable called
      \code{NeticaLicenseKey} in the global environment. If that exists,
      it will be used as the license key when creating a new
      session.  If not, then a new limited session will be created.}
    \item{If a session object was found in step 1, or created in step 2,
      then, if necessary is it activated with a call to
      \code{\link{startSession}}.}
  }

}
\value{
  An object of class \code{\linkS4class{NeticaSession}}.
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
\seealso{
  See \code{\linkS4class{NeticaSession}} for a discription of methods
  that use the Netica Session object.

  \code{\link{startSession}}, \code{\link{stopSession}},
  \code{\link{CreateNetwork}}, \code{\link{GetNthNetwork}},
  \code{\link{GetNamedNetwork}}, and \code{\link{ReadNetworks}}.  It is

  


}
\examples{
\dontrun{
## Create a fully licensed session, and save it as the default
DefaultNeticaSession <- NeticaSession(LicenseKey="License Key from Norsys")

## Create a limited mode session
sess <- NeticaSession()


startSession(sess)
NeticaVersion(sess)
getDefaultSession()
stopSession(sess)
}
}
\keyword{classes}
\keyword{interface}
