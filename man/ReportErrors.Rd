\name{ReportErrors}
\alias{ReportErrors}
\alias{ClearAllErrors}
\alias{logErrors}
\alias{flogErrors}
\alias{printErrors}
\title{Reports internal Netica errors.
}
\description{

  This function is normally called at the end of other Netica calls to
  report out on any Netica errors.  These functions are not normally
  needed at the user level.

  These functions are now depricated in favor of using the
  \code{signalError} method of the \code{\linkS4class{NeticaSession}}
  object, which throws and object of class
  \code{\link{NeticaCondition}} if Netica returned a message through the
  error reporting chanel.

}
\usage{

ReportErrors(maxreport = 9, clear = TRUE, session=getDefaultSession())
ClearAllErrors(severity="XXX_ERR",session=getDefaultSession())
logErrors(allErrs)
flogErrors(allErrs)
printErrors(allErrs)

}
\arguments{
  \item{maxreport}{An integer giving the maximum number errors to report.
}
  \item{clear}{A logical flag indicating if errors should be cleared as
    they are reported (default is to clear errors).
}
  \item{severity}{A string giving one of the Netica error levels (in
    order of severity):  \code{"NOTHING_ERR"}, \code{"REPORT_ERR"},
    \code{"NOTICE_ERR"}, \code{"WARNING_ERR"}, \code{"ERROR_ERR"},
    \code{"XXX_ERR"}.
  }
  \item{session}{An object of class \code{\linkS4class{NeticaSession}}
    which provides the link to the Netica environment.  If not supplied,
    then the default value is the value of the function
    \code{\link{getDefaultSession}()} which is usually the value of
    \code{DefaultNeticaSession} in the global environment.}
  \item{allErrs}{\emph{Updated}.  These functions now take an object of
    class \code{\link{NeticaCondition}} instead of a list of error
    messages. }
}
\details{

  More information about the severity levels of Netica errors can be
  found in the documentation for the Netica function
  \code{GetError_ns}.  The \code{ClearAllErrors()} function clears all
  errors of the indicated severity and below.

  Note \code{ReportErrors} and \code{ClearErrors} are just
  wrappers for \code{session$reportErrors} and
  \code{session$clearErrors} (see \code{\linkS4class{NeticaSession}});
  working with the default session returned by
  \code{\link{getDefaultSession}}.  

  The previous version used \code{\link[futile.logger]{flog.logger}}
  to log the errors.  The most recent version uses the
  \code{session$signalErrors} method, which uses the regular R error
  handling mechanisms, throwing the condition using \code{\link[base]{stop}},
  \code{\link[base]{warning}}, or \code{\link[base]{signalCondition}} depending on the
  severity.  The package using \code{RNetica} is now responsible for
  logging.  The function \code{flogErrors} uses the \code{futile.logger}
  mechanism, and requires that package to be installed.

}
\section{Depricated}{

  With verison 0.10-1, a new method has been added to the
  \code{\linkS4class{NeticaSession}} object, \code{signalErrors}.  This
  function not only generates the \code{\link{NeticaCondition}} object,
  but also hands it to the R error handling mechanism, causing a
  non-local exit.

  Therefore, the following functions have had their interfaces changed,
  and it is recommended not depend on them in future versions
  \describe{
    \item{ReportErrors}{This now returns the
      \code{\link{NeticaCondition}} object invisibly instead of the list
      of error counts.  It is recommended that programmers use the
      \code{reportErrors} or \code{signalErrors} methods of the
      \code{\linkS4class{NeticaSession}} object.  Note that many RNetica
      objects, particularly, \code{\linkS4class{NeticaBN}} and
      \code{\link{NeticaNode}} objects have these same messages (which
      the forward to the session.}
    \item{ClearAllErrors}{It is recommended that programers use the
      \code{clearErrors} object of the session instead.}
    \item{flogErrors}{Arguments have changed so that it now takes a
      \code{\link{NeticaCondition}} instead of a list of message
      vectors.  Errors, warnings, notices and reports are logged with
      the corresponding levels of logging.  Note that the
      \code{futile.logger} package is now on the 
      Suggets list instead of the Imports list, so it will need to be
      installed to run this function. }
    \item{logErrors}{Arguments have changed so that it now takes a
      \code{\link{NeticaCondition}} instead of a list of message
      vectors.  This function calls \code{flogErrors} and then returns a
      vector of message counts.}
    \item{printErrors}{Arguments have changed so that it now takes a
      \code{\link{NeticaCondition}} instead of a list of message
      vectors.  This just calls \code{\link{print.NeticaCondition}}.}
  }


}
\value{

  \emph{Update!} \code{ReportErrors()} returns the
  \code{\link{NeticaCondition}} object or \code{NULL} invisibly.
  
  

}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GetError_ns}, \nref{ClearError_ns}
}
\author{
  Russell Almond
}
\section{Implementation}{

  The function \code{ReportErrors} is implemented as follows.  First, it
  calls the \code{reportErrors} method of the
  \code{\linkS4class{NeticaSession}} object.  This returns an object of
  type \code{\link{NeticaCondition}} or (\code{NULL} if there are no
  errors, warnings or messages).  If the return object was a condition,
  it is printed.  The result is returned invisibly.

  This has been largely superceeded by the \code{signalErrors} method of
  the \code{\linkS4class{NeticaSession}} object.  This function
  generates the condition and then passes it to the normal R conditional
  handling functions (\code{\link[base]{stop}, \link[base]{warning}}, and
  \code{\link[base]{signalCondition}}.  

  The \code{flogErrors} function logs the errors using
  \code{\link[futile.logger]{flog.logger}}.  The function
  \code{printErrors} simply prints them to the console.  To remove the
  use of \code{futile.logger} redefine \code{logErrors} to use
  \code{printErrors} instead of \code{logErrors}.

}
\note{

  Netica functions which make calls to the Netica API (through
  \code{.Call}) all call the \code{signalErrors} method of some object.
  \code{\linkS4class{NeticaNode}} object delegate this to the
  \code{\linkS4class{NeticaBN}}, which in turn delegates it to the
  \code{\linkS4class{NeticaSession}} object.  This function will return
  an object of class \code{\link{NeticaCondition}} if Netica generated a
  message during the API call (otherwise it will simply return
  \code{NULL}).

  The \code{signalErrors} function may produce a non-local exit,
  depending on how the R error handling is set up.  This can be
  controled in the usual ways via the \code{\link[base]{options}} or
  \code{\link[base]{tryCatch}} mechansim.  In particular, if the
  condition is of class:
  \describe{
    \item{error}{The special function \code{\link[base]{stop}} will be
      called.  This usually causes the function to be exited to the top
      level (or some enclosing \code{tryCatch}).}
    \item{warning}{The special function \code{\link[base]{warning}} will be
      called.  The default R behavior is to hold these until the end of
      all function calls and then print them to the console.}
    \item{message}{The special function
      \code{\link[base]{signalCondition}} is called.  The default
      behavior is to ignore the message.}
    \item{fatal}{This indicates an error with greater severity than a
      simple error.  It is always paired with error, but it may be
      referenced separately in the \code{tryCatch} handlers to give
      different treatment.}
  }
  Note that all \code{\link{NeticaCondition}} objects have class
  \code{"NeticaCondition"}, so that special handlers for them can be set
  up using \code{\link{tryCatch}}.
}
\section{Old Protocols}{

  The original protocal is that the higher level RNetica functions
  all call \code{ReportErrors()} to report error messages and warnings
  and then throw an error (call \code{\link[base]{stop}()}) whenever the
  error count is greater than one.  These functions are probably not
  necessary in user level code.

  As of version 0.5, this is modified so that functions follow the
  \code{\link{NeticaNode}} to the \code{\link{NeticaBN}}
  to the \code{\linkS4class{NeticaSession}} and then call the report
  error mechanism on that.

    Future versions may use a more sophisticated error checking mechanism
  that allows for more user control.

  There was a known bug where \code{ReportErrors} keeps
  finding an error that indicates that there is an invalid error pointer
  (which keeps retriggering the error).  This should now be fixed.

}

\seealso{
  \code{\linkS4class{NeticaSession}}
  
  \code{\link{StopNetica}()} -- should be called after
  \code{"XXX_ERROR"} is signaled by Netica.

  \code{\link[futile.logger]{flog.logger}} -- describes the logging
  mechanism. 

}
\examples{

myCreateNetwork <- function (names,session) {
  handles <- .Call("RN_New_Net",as.character(names),session,PACKAGE="RNetica")
  ## Check for errors
  session$signalErrors() ## Stop if errors are encountered
  handles
}
\dontrun{
ClearAllErrors(getDefaultSession())
getDefaultSession()$clearErrors()
}

}
\keyword{ interface }
\keyword{ programming }
\keyword{ error }
\keyword{ internal }
