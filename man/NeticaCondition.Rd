\name{NeticaCondition}
\alias{NeticaCondition}
\alias{print.NeticaCondition}
\title{A condition raised within a Netica API Call}
\description{

  This is a condition (error, warning, or message) raised during an
  Netica API call.  Netica errors need to be checked by calling the
  \code{reportErrors} method of the \code{\linkS4class{NeticaSession}}.
  If Netica raised an error during a previous API call, then
  \code{Session$reportErrors()} will return an object of class
  \code{NeticaCondition}.  The method \code{Session$signalErrors()},
  will also call \code{\link[base]{stop}} or \code{\link[base]{warning}} if
  the Netica message is an error or warning.

}
\usage{
NeticaCondition(allErrs, call)
\method{print}{NeticaCondition}(x, ...)
}
\arguments{
  \item{allErrs}{This is al list of five character vectors, which is
    returned from Netica.  The elements give the errors of type fatal
    (\code{"XXX_ERR"}), error (\code{"ERROR_ERR"}), warning
    (\code{"WARNING_ERR"}), notice (\code{"NOTICE_ERR"}), and report
    (\code{"REPORT_ERR"}).  If there are no errors of the given type,
    the corresponding character vector will have length zero.}
  \item{call}{The call (function and arguments) from contains the API call
    that generated the messages.}
  \item{x}{An item of class \code{NeticaCondition} to be printed.}
  \item{...}{Other arguments to print.}
}
\details{

The \code{NeticaCondition} function is generally called from within the
\code{reportErrors} method of the \code{\linkS4class{NeticaSession}}
object which is, in turn, called by the \code{signalErrors} message.
The  \code{reportErrors} method calls an internal C method which returns
the list of error messages generate by Netica. This list has five
components in order of severity:  \code{fatal}, \code{error},
\code{warning}, \code{notice}, \code{report}, each of which is a
character vector.  The class of the return
object of \code{NeticaCondition} is determined by the follwing sequence 
of rules (with the condition assigned to the most severe class):

\describe{
  \item{If \code{fatal} is nonempty}{Return object is of class
    \code{c("NeticaCondition" "fatal" "error" "condition")}.}
  \item{If \code{error} is nonempty}{Return object is of class
    \code{c("NeticaCondition" "error" "condition")}.}
  \item{If \code{warning} is nonempty}{Return object is of class
    \code{c("NeticaCondition" "warning" "condition")}.}
  \item{If \code{notice} or \code{report} is nonempty}{Return object is
    of class \code{c("NeticaCondition" "message" "condition")}.}
  \item{If all are empty.}{Return \code{NULL}.}
}

The \code{NeticaCondition} objects are thus all subclasses of the
\code{\link[base]{condition}} class.  In addition to the normal
\code{message} and \code{call} fields, \code{NeticaCondition} objects
also have addition fields giving the error message text passed from
Netica.  Netica indicated a fatal error, and error or a warning, then
the \code{NeticaCondition} class will also be a subclass of \code{error}
or \code{warning}.  The \code{print.NeticaCondition} method prints out
the extra fields.

}
\value{

  Either \code{NULL} (if there were no messages) or an object of class
  \code{c("NeticaCondition", } \emph{severity}\code{, "condition")},
  where \emph{severity} is either \code{"fatal", "error"},
  \code{"error"}, \code{"warning"} or \code{"message"}.

  It has the following fields:
  \item{message}{A character scalar giving the message \code{\"Netica }
      \emph{severity} \code{ encountered.\"}.}
  \item{call}{The \code{RNetica} call in which the error was generated
    (see \code{\link[base]{conditionCall}}).}
  \item{Fatal}{A character vector giving fatal error messages, length
    zero if no fatal errors.}
  \item{Error}{A character vector giving (non-fatal) error messages, length
    zero if no (non-fatal) errors.}
  \item{Warning}{A character vector giving warnings, length
    zero if no warnings.}
  \item{Notice}{A character vector giving notices, length
    zero if no notices.}
  \item{Report}{A character vector giving reports, length
    zero if no reports.}
  
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
\seealso{
  \code{\linkS4class{NeticaSession}}, \code{\link{ReportErrors}},
  \code{\link[base]{condition}}
}
\examples{
}
\keyword{ error }
\keyword{ programming }
\keyword{ interface }
\keyword{ internal }

