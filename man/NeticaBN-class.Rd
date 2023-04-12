\name{NeticaBN-class}
\Rdversion{1.1}
\docType{class}
\alias{NeticaBN-class}
\alias{Compare,NeticaBN,NeticaBN-method}
\alias{print,NeticaBN-method}
\alias{toString,NeticaBN-method}
\alias{as.character,NeticaBN-method}
\alias{is.element,NeticaBN,list-method}

\title{Class \code{"NeticaBN"}}
\description{
  This is an R side container for a Netica netork.  Note that it has a
  container for the nodes which have been advertised from the network.
}
\section{Extends}{

All reference classes extend and inherit methods from
\code{"\linkS4class{envRefClass}"}. Note that because this is a
reference class unlike traditional S3 and S4 classes it can be
destructively modified.  Also fields (slots) are accessed using the
\sQuote{$} operator.

}
\section{Methods}{
  \describe{
    \item{Compare}{\code{signature(e1 = "NeticaBN", e2 = "NeticaBN")}:
      Tests for equality (mainly of pointers. }
    \item{is.active}{\code{signature(x = "NeticaBN")}: Returns true if
      the \code{NeticaBN} objectcurrently references an active Netica
      object, and returns false if it references a deleted network or a
      network created in a previous sesion which has not be
      re-activated. }
    \item{print}{\code{signature(x = "NeticaBN")}: Creates a printed
      representation. }
    \item{toString}{\code{signature(x = "NeticaBN")}: Creates a
      character representation. }
    \item{is.element}{\code{signature(el = "NeticaBN", set = "list")}:
      Checks to see if \var{el} is in list of nets.}
  }
}
\details{

  A \code{NeticaBN} is an R wrapper for the internal pointer to the
  Netica nework.  A network is said to be \sQuote{active} if it
  references a network object in a current Netica session.  A network
  become inactive when it is deleted (with a call to
  \code{\link{DeleteNetwork}}) or when the R session is saved an
  restored.  In the latter case, if the network was saved with a call to
  \code{\link{WriteNetworks}}, then calling \code{\link{ReadNetworks}}
  on the inactive network will reload it from the save file.

  Generally, \code{NeticaBN} objects are created with calls to either 
  \code{\link{CreateNetwork}} or \code{\link{ReadNetworks}}.  Both
  require a reference to an active \code{\linkS4class{NeticaSession}}
  object.  \code{NeticaBN} objects are registered with the
  \code{\linkS4class{NeticaSession}} object, which contains a collection
  of all of the networks known about by the session.

  The \code{nodes} field of the \code{NeticaBN} object
  (\code{net$nodes}) contains a cache of all code
  \code{\linkS4class{NeticaNode}} objects that are contained by the
  network and known about by R.  Nodes are registered by their Netica
  name so the expression \code{\var{net}$nodes$\var{nodename}} or
  \code{\var{net}$nodes[[\var{nodename}]]} references a node with the name
  \code{\var{nodename}} in \code{\var{net}}.

  Note that R node objects are created when a node is created in R, but
  not when a network is read in using \code{\link{ReadNetworks}}.  This
  is useful for cases where the network is large and only a few nodes
  will be reference in the R code.  The function
  \code{\link{NetworkFindNode}()} will find a node by name and create
  the R object corresponding to the node if needed.  The function
  \code{\link{NetworkAllNodes}()} will create R objects for all nodes in
  the net.

}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIurl/index.html}:
  \nref{GetNetUserData_bn}, \nref{SetNetUserData_bn} (these are used
  to maintain the back pointers to the R object).
}
\author{Russell Almond}
\note{

  The \code{NeticaBN} class was changed into a formal R6 reference class
  as of version 0.5 of RNetica.  Prior to that, it was an S3 class
  created by adding attributes to a string.  That proved to be less than
  robust, as several R functions (notably \code{\link[base]{c}()}) would
  strip the attributes.

  Another change is the method for finding the network object from the
  Netica pointer inside of the C code.  Now the R objects are cached
  inside of a \code{\linkS4class{NeticaSession}} object by their netica
  name.  The R object is found by searching the cache inside of the
  session object.

}

\seealso{
  \code{NeticaBN} objects are contained by
  \code{\linkS4class{NeticaSession}} objects and contain
  \code{\linkS4class{NeticaNode}} objects.

  \code{\link{CreateNetwork}()},\code{\link{DeleteNetwork}()},
  \code{\link{GetNamedNetworks}()},\code{\link{NetworkName}()},
  \code{\link{is.active}()}, \code{\link{NetworkAllNodes}()},
  \code{\link{WriteNetworks}()}, \code{\link{GetNetworkFileName}()},
  
}
\examples{
sess <- NeticaSession()
startSession(sess)

net1 <- CreateNetwork("aNet",sess)
stopifnot(is.NeticaBN(net1))
stopifnot(is.active(net1))
stopifnot(net1$Name=="aNet")

net2 <- GetNamedNetworks("aNet",sess)
stopifnot(net2$Name=="aNet")
stopifnot(net1==net2)

NetworkName(net1) <- "Unused"
stopifnot(net1==net2)

netd <- DeleteNetwork(net1)
stopifnot(!is.active(net1))
stopifnot(!is.active(net2))
stopifnot(netd$Name=="Unused")
stopifnot(netd == net1)

stopSession(sess)

}
\keyword{classes}
\keyword{interface}
\keyword{graphs}
\section{Fields}{
  Note these should be regarded as read-only from user code.
  \describe{
    \item{\code{Name}:}{Object of class \code{character} giving the
      Netica name of the network. Must follow the \code{\link{IDname}}
      rules.  This should not be set by user code, use
      \code{\link{NetworkName}} instead.}
    \item{\code{PathnameName}:}{Object of class \code{character} giving
      the path from which the network was last read or to which it was
      last saved. }
    \item{\code{Netica_bn}:}{Object of class \code{externalptr} linking
      to the Netica object corresponding to this network. }
    \item{\code{Session}:}{Object of class \code{NeticaSession}: a back
      pointer to the \code{\linkS4class{NeticaSession}} object in which
      the network was created. }
    \item{\code{nodes}:}{Object of class \code{environment} which
      contains a cache of \code{\linkS4class{NeticaNode}} objects
      belonging to this network. }
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{listNodes()}:}{ Lists all of the cached nodes.
      (Contrast this to \code{\link{NetworkAllNodes}(net)} which lists
      all nodes in the network.}
    \item{\code{searchNodes(pattern)}:}{ Lists all cached nodes matching
    the regular expression given in pattern.  (See
    \code{\link[base]{objects}}.)} 
    \item{\code{show()}:}{ Gives a detailed description of the object. }
    \item{\code{isActive()}:}{ Returns true if the object currently
      points to a Netica network, and false if it does not. }
    \item{\code{findNode(nodename)}:}{ Searches for a cached node with
      name \code{nodename}, returns it if found or \code{NULL} if not.}
    \item{\code{clearErrors(severity)}:}{ Calls \code{clearErrors} on
      the \code{Session} object. }
    \item{\code{reportErrors(maxreport, clear, call)}:}{ Calls
      \code{reportErrors} on the \code{Session} object. Returns an
    object of class \code{\link{NeticaCondition}} if there was a
    message, or \code{NULL} if not.}
    \item{\code{signalErrors(maxreport, clear, call)}:}{ Calls
      \code{signalErrors} on the \code{Session} object.  If there was a
    problem, the appropriate condition is signaled, see
    \code{\link{NeticaCondition}}.  }
    \item{\code{initialize(Name, Session, ...)}:}{ Initialization
      function.  Should not be called by user code. }
    \item{\code{deactivate()}:}{ Destroys the pointer to the Netica
      object.  Should not be called by user code. }
    \item{\code{deactivateNodes()}:}{ Recursively deactives all nodes
      contained by this network.  Should not be called by user code. }
  }
}
