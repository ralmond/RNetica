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
ReportErrors(maxreport = 9, clear = TRUE)
ClearAllErrors(severity="XXX_ERR")
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
}
\details{
  More information about the severity levels of Netica errors can be
  found in the documentation for the Netica function
  \code{GetError_ns}.  The \code{ClearAllErrors()} function clears all
  errors of the indicated severity and below.
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

  Future versions may use a more sophisticated error checking mechanism
  that allows for more user control.
}

\seealso{
  \code{\link{StopNetica}()} -- should be called after
  \code{"XXX_ERROR"} is signaled by Netica.
}
\examples{
myCreateNetwork <- function (names) {
  handles <- .Call("RN_New_Net",as.character(names))
  ## Check for errors
  ecount <- ReportErrors()
  if (ecount[1]>0) { ## Stop if errors are encountered
    stop("CreateNetwork: Netica Errors Encountered, see console for details.")
  }
  handles
}

}
\keyword{ interface }
\keyword{ programming }
\keyword{ error }
\keyword{ internal }% __ONLY ONE__ keyword per line
