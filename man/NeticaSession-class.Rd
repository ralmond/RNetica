\name{NeticaSession-class}
\Rdversion{1.1}
\docType{class}
\alias{NeticaSession-class}
\alias{startSession,NeticaSession-method}
\alias{stopSession,NeticaSession-method}
\alias{print,NeticaSession-method}
\alias{toString,NeticaSession-method}
\alias{as.character,NeticaSession-method}

\title{Class \code{"NeticaSession"}}
\description{

  An R object which provides a link to the Netica session.  One of
  these must be present and active to allow access to Netica from R.
  This object is also how one enables the Netica license.

}
\section{Extends}{

All reference classes extend and inherit methods from
\code{"\linkS4class{envRefClass}"}.  Note that because this is a
reference class unlike traditional S3 and S4 classes it can be
destructively modified.  Also fields (slots) are accessed using the
\sQuote{$} operator.

}
\section{Methods}{
  \describe{
    \item{is.active}{\code{signature(x = "NeticaSession")}: Returns true
    if the link to Netica is currently active and available and false if
  not.  }
    \item{startSession}{\code{signature(session = "NeticaSession")}:
      Starts the Netica session and makes it available. }
    \item{stopSession}{\code{signature(session = "NeticaSession")}:
      Stops the Netica session and makes all \code{\link{NeticaBN}} and
      \code{\link{NeticaNode}} objects inactive.}
    \item{restartSession}{\code{signature(session = "NeticaSession")}:
      Stops and restarts the session. }
  }
}
\details{

  A Netica session is an R wrapper for the internal pointer to the
  Netica workspace.  It is used by a number of high-level Netica
  functions to provide access to the workspace, notably:
  \code{\link{CreateNetwork}}, \code{\link{GetNthNetwork}},
  \code{\link{GetNamedNetworks}}, and \code{\link{ReadNetworks}}.  It is
  also needed by some lower level functions which create Netica objects:
  \code{\link{CaseFileDelimiter}}, \code{\link{CaseFileMissingCode}},
  \code{\link{CaseFileStream}}, \code{\link{CaseMemoryStream}},
  \code{\link{OpenCaseStream}} and \code{\link{NewNeticaRNG}}.

  When initially created, the NeticaSession object is not active; that
  is, the connection to the internal Netica environment is not yet
  established.  Calling the method \code{startSession} will start the
  session, making it active.  The method \code{stopSession} will
  deallocate the Netica workspace.  Note that it will also make any
  \code{\link{NeticaBN}} or \code{\link{NeticaNode}} objects it contains
  inactive. The function \code{\link{is.active}()} tests to see whether
  the session is currently active.

  Starting with the introduction of the NeticaSession class, a license
  key purchased from Norsys (\url{http://www.norsys.com/}) is a field of
  the NeticaSession class.  This field should either be a character
  scalar giving the complete string supplied by Norsys or a character
  scalar vector of length zero (the default).  The licence key is passed
  to Netica with the call to \code{startSession()}; if it is a valid
  license key, then Netica starts in unlimited mode.  If it is not a
  valid license key, the Netica will start in a limited mode which
  restricts the number of objects that can be created.  All of the
  examples in the documentation should work in the limited mode; but
  people wanting to do serious work will need to purchase a license key.


  \emph{As the LicenseKey is stored in the NeticaSession object, do not
  share dumps of the NeticaSession object with people who are not
  eligible to use your license.}

  The \code{nets} field of the NeticaSession object contains a
  collection (actually an \link[base]{environment}) of all of the
  networks.  They are referenced using their Netica names.  In
  particular, the construct \code{session$nets$netname} will return the
  network named netname if it exists, or \code{NULL}.  Similarly, the
  construct \code{\var{session}$nets[[\var{netname}]]} will attempt to
  return the network whose name is the value of \code{netname}.  The
  method \code{session$listNets()} lists the names of all networks
  registered with the session.  This collection is maintained by the
  \code{\link{CreateNetwork}}, \code{\link{DeleteNetwork}}, and
  \code{\link{ReadNetworks}}, so it is almost certainly an error to try
  and manually change it.  Network objects should be renamed using
  \code{\link{NetworkName}} which will update the collection.

  Note that if an R workspace containing a NeticaSession object is saved
  and restored, then the \code{nets} field will be a collection of
  inactive \code{NeticaBN} objects corresponding to the networks that
  were open when the session was saved.  As these contain the pathnames
  where the networks were last saved, it can be used to reload the
  networks for a project.

  It is unknown what would happen if more than one NeticaSession is
  active at the same time.  Many possibilities are less than ideal.
 
}
\references{
  \url{https://pluto.coe.fsu.edu/RNetica/}
}
\author{Russell Almond

}
\note{

  The session object is part of a fundamental redesign of the guts of
  the way RNetica works starting with version 0.5.  There are three
  features of this redesign:

  \enumerate{

    \item{The old \code{\link{NeticaBN}} and \code{\link{NeticaNode}}
      classes, which were implemented as S3 classes formed by adding
      attributes to strings, have been replaced with R6 reference classes,
      which should be more robust.  In particular, the
      \code{\link[base]{c}()} command strips attributes, which tended to
      destroy node and net objects.}

    \item{The session pointer used to be handled with a global variable
      in the RNetica source code.  It is now a field in the new session
      object.  This should allow more flexibility as well as not relying
      on a hidden mechanism.}

    \item{Instead of using back-pointers from the Netica objects,
      Sessions contain an environment where nets are registered and nets
      contain an environment where nodes are registered.  This should fix
      a problem with the backpointers pointing to the wrong location.}  
  }
    
  In the earlier design, the session object was hidden.  For that
  reason, the function \code{\link{getDefaultSession}()} has been
  added.  This looks for an object called \code{DefaultNeticaSession}
  in the gobal environment.  If this object does not exist, the user
  will be prompted to make one the first time
  \code{\link{getDefaultSession}()} is called.  This is the default
  for most high level functions which take a section argument;
  hopefully, this will provide backwards compatability.
  
}
\section{Error Handing}{

    The error reporting mechanism works through the session
  object.  The \code{session$reportErrors()} and
  \code{\var{session}$clearErrors()} methods are used to maintain this
  system.  The \code{\link{NeticaBN}} object contains a back pointer
  to its session, and delegates error reporting to the session.  In
  particular, \code{\var{net}$Session} returns the session object.
  Similarly, a \code{\link{NeticaNode}} contains a back pointer to the
  \code{\link{NeticaBN}}, so \code{\var{node}$Net$Session} accesses the
  session.

    As of Version 0.10-1, the error reporting mechanism has been
  updated.  The \code{reprotErrors} method has been changed to return
  an object of type \code{\link{NeticaCondition}} if Netica signaled
  an error, warning or message during executing the API call.  This is
  of class \dQuote{error}, \dQuote{warning}, or \dQuote{message},
  depending on severety of the message.  If no message was returned,
  then \code{reportErrors} returns \code{NULL}.

  The recommended way to handle errors, used internally in most RNetica
  functions, is to call the \code{signalErrors} message.  This will
  throw the appropriate condition, calling \code{\link[base]{stop}} for errors,
  \code{\link[base]{warning}} for warnings and
  \code{\link[base]{signalCondition}} for conditions.  This will cause
  internal Netica errors to be handled with the same mechanisms used to
  control ordinary R errors.


}
\seealso{
  \code{\link{CreateNetwork}}, \code{\link{GetNthNetwork}},
  \code{\link{GetNamedNetworks}}, and \code{\link{ReadNetworks}}.  It is
  also needed by some lower level functions which create Netica objects:
  \code{\link{CaseFileDelimiter}}, \code{\link{CaseFileMissingCode}},
  \code{\link{CaseFileStream}}, \code{\link{CaseMemoryStream}},
  \code{\link{OpenCaseStream}} and \code{\link{NewNeticaRNG}}.

  \code{\linkS4class{NeticaBN}} 

}
\examples{

## Create a limited mode session
\dontrun{
## Create a fully licensed session, and save it as the default
DefaultNeticaSession <- NeticaSession(LicenseKey="License Key from Norsys")
}
sess <- NeticaSession()

startSession(sess)
NeticaVersion(sess)

myNet <- CreateNetwork("myNet",sess)

stopifnot(myNet==sess$nets$myNet)
stopifnot(myNet==sess$nets[["myNet"]])
stopifnot(myNet==sess$findNet("myNet"))
stopifnot(identical(sess$listNets(),c("myNet")))

sess$reportErrors()
sess$clearErrors()  ## Not necessary as the previous statement clears too.

stopSession(sess)

\dontrun{
## Shows how to restore networks from a default session
## Existing in the workspace
for (netname in DefaultNeticaSession$listNets()) {
  net <- DefaultNeticaSession$findNet(netname)
  ReadNetworks(GetNetworkFileName(net),DefaultNeticaSession)
}
}

}
\keyword{classes}
\keyword{interface}
\section{Fields}{
  Note these should be regarded as read-only from user code.
  \describe{
    \item{\code{LicenseKey}:}{Object of class \code{character} giving
      the license key obtained from Norsys
      (\url{http://www.norsys.com/}.  Leaving this field as
      \code{character(0)} will result in the limited version of Netica
      being used. }
    \item{\code{SessionName}:}{Object of class \code{character} giving
      an identifier for the session.  This is just used for printing. }
    \item{\code{NeticaHandle}:}{Object of class \code{externalptr}
      giving the C memory location of the Netica API workspace.  This
      should not be manipulated by users.  If the session is inactive,
      this pointer will be nil. }
    \item{\code{Checking}:}{Object of class \code{character} one of the
      keywords: 
    \code{"NO_CHECK"}, \code{"QUICK_CHECK"}, \code{"REGULAR_CHECK"},
    \code{"COMPLETE_CHECK"}, or \code{"QUERY_CHECK"}, which controls how
    rigorous Netica is about checking errors. A value of \code{character()}
    uses the Netica default which is \code{"REGULAR_CHECK"}. }
    \item{\code{maxmem}:}{Object of class \code{numeric} containing an
      integer indicating the maximum amount of memory to be
      used by the Netica shared library in bytes.  If supplied, this
      should be at least 200,000. }
    \item{\code{nets}:}{Object of class \code{environment} used to store
    \code{\link{NeticaBN}} objects opened by this session.  This should
    be regarded as read-only by user code.}
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{neticaVersion()}:}{ Returns the netica version
      associated with this session. }
    \item{\code{show()}:}{ Provides information about the session. }
    \item{\code{isActive()}:}{ Returns a logical value indicating
      whether Netica is currently started. }
    \item{\code{listNets(pattern="")}:}{ Lists the names of all of the networks
      registered with this session. If pattern is supplied it should be
      a regular expression, only nets whose name contains the regular
      expression will be listed.}
    \item{\code{initialize(..., SessionName, autostart)}:}{ Creates a
      new session.  If \code{autostart} is true, then the session will
      be started after it is created. }
    \item{\code{findNet(netname)}:}{ Returns a \code{\link{NeticaBN}}
      object associated with a network, or \code{NULL} if no network
      with that name exists.}
    \item{\code{clearErrors(severity)}:}{ Clears errors of a given
      severity level or lower.  Severity levels in order are:
    \code{"NOTHING_ERR"}, \code{"REPORT_ERR"},
    \code{"NOTICE_ERR"}, \code{"WARNING_ERR"}, \code{"ERROR_ERR"},
    \code{"XXX_ERR"}.  The default is to report all errors.}
  \item{\code{reportErrors(maxreport=9, clear = TRUE,
      call= sys.call(sys.parent()))}:}{ Reports errors.  The
      \code{maxreport} value gives the maximum number of errors to
      report.  The \code{clear} value (default true) asks if errors
      should be cleared after reporting. If the internal Netica error
      reporting function returns messages, then a new object of type
      \code{\link{NeticaCondition}} is created.  The original Netica
      errors are given in extra fields with the names \dQuote{Fatal},
      \dQuote{Error}, \dQuote{Warning}, \dQuote{Notice} and
      \dQuote{Report}.  The default value for call is the function which
      calls the \code{session$reportErrors} method.  If there are no
      messages, \code{NULL} is return instead of the condition.}

    \item{\code{signalErrors(maxreport, clear, call)}:}{ Calls
      \code{reportErrors}, and then if the return result is an
      \dQuote{error}, calls \code{\link[base]{stop}}; if the result is a
      \dQuote{warning}, calls \code{warning}; if the reseult is a
      \dQuote{message}, calls \code{\link[base]{signalCondition}} (which
      by default does nothing).  If \code{reportErrors} returns
      \code{NULL} it does nothing.  If the signaling the condition does
      not result in a non-local exit to the function, it returns the
      output of \code{reportErrors} invisibly.}
  }
}
