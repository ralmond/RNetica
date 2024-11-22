\name{SetupDefaultSession}
\alias{SetupDefaultSession}
\title{Create a NeticaSession that Automatically Closes}
\description{

  This function creates a \code{\link{NeticaSession}} objects and
  assigns it to the value \code{DefaultNeticaSession} in the global
  environment (the value checked by \code{\link{getDefaultSession}()}).
  The session is opened when the function called, and then uses
  \code{withr::\link[withr]{defer}} to close the session when the
  environment exits.

}
\usage{
SetupDefaultSession(license = options("NeticaLicenseKey")[[1]],
                    checking = NULL, maxmem = NULL,
                    env = .GlobalEnv)
}
\arguments{
  \item{LicenseKey}{If supplied, this should be a character scalar
    providing the license key purchased from Norsys
    \url{http://www.norsys.com/}.  If left as default, and if the
    license key is not set in Netica, RNetica will run
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
  \item{env}{An R \link{envirnoment} passed to \code{withr::defer}, see details.
  }
}
\details{

  This is basically a synonym for \code{\link{NeticaSession}} with a
  deferred call to \code{\link{stopSession}} when testing is no longer
  needed.  The \code{stopSession()} function is called when the
  \code{env} argument exits.  There are three common values for the
  \code{env} argument:

  \describe{
    \item{\code{.GlobalEnv} (default)}{In this case \code{stopSession()}
      runs when \code{withr::\link[withr]{deffered_run}()}
      and not run if \code{withr::deffered_clear()} is called.}
    \item{\code{parent.frame()}}{In this case \code{stopSession()} is
      called when the function calling \code{SetupDefaultSession} is run.}
    \item{\code{testthat::\link[testthat]{teardown_env}()}}{This is
    designed to work with 
      the "setup.R" file in the
      \code{\link[testthat::testthat-package]{testthat}} environment.  In
      this case \code{stopSession()} should be called when the testing
      environment is called.}
  }

  All other arguments are passed to \code{\link{NeticaSession}} to
  create the new session.  Note that if \code{DefaultNeticaSession}
  exists and is bound to a \code{NeticaSession} object, that session is
  opened (with a call to \code{\link{startSession}()}) and not recreated.
  
}
\value{

  No return value used for side effects.

  The symbol \code{DefaultNeticaSession} in the global environment is
  bound to a new netica session.  The funciton
  \code{\link{getDefaultSession}()} should return that value.

  The default session is started and a deferred call to stop the session
  is put in the approriate \code{withr} queue.

}
\references{

  See \code{vignette("special-files","testthat")} for information on the
  \code{setup.R} file.
  
}
\author{Russell Almond}
\note{

  The \code{RNetica} and Netica shared libraries still need to be
  loaded.  The easiest way to ensure this happens is through a call to
  \code{library(RNetica)}. 
}
\seealso{
  Referenced functions:
  \code{\link{NeticaSession}}, \code{\link{getDefaultSession}},
  \code{\link{startSession}}, \code{\link{stopSession}},
  \code{\link{stopSession}}, \code{withr::\link[withr::defer]{defer}}

  Other testing helper functions:

  \code{\link{local_create_nets}}, \code{\link{local_load_nets}},
  \code{\link{local_copy_nets}}, \code{\link{local_RNetica_net}}
      
}
\examples{

## For use in a testthat setup.R file.
\dontrun{
  library(RNetica)
  SetupDefaultSession(env=testthat::teardown_env())
}

##For use in a script file not in the testthat framework.
\dontrun{
  library(RNetica)
  SetupDefaultSession()
  ## ... Tests go here
  withr::deferred_run()  #Runs stopSession()
}

}
\keyword{environment}
\concept{testing}
\concept{NeticaSession}

