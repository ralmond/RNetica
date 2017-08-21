\name{ReportErrors}
\alias{ReportErrors}
\alias{ClearAllErrors}
\title{Reports internal Netica errors.
}
\description{

  This function is normally called at the end of other Netica calls to
  report out on any Netica errors.  These functions are not normally
  needed at the user level.

}
\usage{

ReportErrors(maxreport = 9, clear = TRUE, session=getDefaultSession())
ClearAllErrors(severity="XXX_ERR",session=getDefaultSession())

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
}
\details{

  More information about the severity levels of Netica errors can be
  found in the documentation for the Netica function
  \code{GetError_ns}.  The \code{ClearAllErrors()} function clears all
  errors of the indicated severity and below.

  Note that these functions are really just wrappers for
  \code{session$reportErrors} and \code{session$clearErrors} (see
  \code{\linkS4class{NeticaSession}}).  


}
\value{

  \code{ReportErrors()} returns a count of the number of errors of
  severity \code{"ERROR_ERR"} or higher.

}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GetError_ns}, \nref{ClearError_ns}
}
\author{
  Russell Almond
}
\note{

  At this point, the protocol is that the higher level RNetica functions
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

  There is currently a known bug where somehow \code{ReportErrors} keeps
  finding an error that indicates that there is an invalid error pointer
  (which keeps retriggering the error).  Hopefully, that will be fixed
  in future versions.

}

\seealso{
  \code{\linkS4class{NeticaSession}}
  
  \code{\link{StopNetica}()} -- should be called after
  \code{"XXX_ERROR"} is signaled by Netica.

}
\examples{

myCreateNetwork <- function (names,session) {
  handles <- .Call("RN_New_Net",as.character(names),session,PACKAGE="RNetica")
  ## Check for errors
  ecount <- ReportErrors()
  if (ecount[1]>0) { ## Stop if errors are encountered
    stop("CreateNetwork: Netica Errors Encountered, see console for details.")
  }
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
