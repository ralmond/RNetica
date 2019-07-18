\name{RNetica-package}
\alias{RNetica-package}
\alias{RNetica}
\docType{package}
\title{\packageTitle{RNetica}}
\description{
\packageDescription{RNetica}
}
\details{
The DESCRIPTION file:
\packageDESCRIPTION{RNetica}

This package provides an R interface to the Netica, in particular, it
binds many of the functions in the Netica C API into the R language.
RNetica can create and modify networks, enter evidence and extract the
conditional probabilities from a Netica network.

}
\section{License}{

  While RNetica (the combination of R and C code that connects R and
  Netica) is free software, as is \R, Netica is a commercial product.
  Users of RNetica will need to purchase a Netica API license key (which
  is different from the GUI license key) from Norsys(R)
  (\url{http://www.norsys.com/}).

  Once you have a license key, you can use it in one of three ways.
  The currently (RNetica 0.5 and later) recommended way of using it is
  to create a Netica  Session object that contains it:
  \code{DefaultNeticaSession <-
  NeticaSession(LicenseKey="License Key from Norsys")}.  This will store
  the key in a \code{\linkS4class{NeticaSession}} object.  The special
  variable \code{DefaultNeticaSession} is used as a default for every
  function requiring a session argument, so can be used to skip the need
  for explicitly stating the session argument.  

  Two other mechanisms continue to be supported for backwards
  compatibility.  First, the license key can be used as an argument to
  the function \code{\link{StartNetica}()}.  This will create a session
  and store it in \code{NeticaDefaultSession}.   If the variable 
  \code{NeticaLicenseKey} in the R top-level environment is set before
  the call to \code{library(RNetica)}, \code{StartNetica()} will pick up
  the license key from that location.

  Without the license key, the Netica shared library will be restricted
  to a student/demonstration mode with limited functionality.  Note that
  all of the example code (and hence \code{R CMD check RNetica}) can be
  run using the limited version.

  Note that the \code{\linkS4class{NeticaSession}} object stores the
  complete Netica license key.  Do not share dumps of the session object
  (including the \code{.RData} file containing
  \code{DefaultNeticaSession}) with any third-party.

}
\section{Index}{

  \packageIndices{RNetica}

}
\section{RNetica Environment and Netica Objects}{

Netica exists in both as a stand alone graphical tool for building and
manipulating Bayesian networks (the Netica GUI) and as a shared library
for manipulating Bayesian networks (the Netica API).  The RNetica
package binds the API version of Netica to a series of R functions which
do much of the work of manipulating the network.  The file format
for the GUI and API version of Netica is identical, so analysts can
easily move back and forth between the two.  Note that the
RNetica environment is separate from other Netica environments that may
be created using the Netica GUI (or API invoked from a different
program); RNetica can only manipulate the networks that are currently
loaded into its environment.

There are five objects which provide a handle for objects in the Netica
session.  These are:
\describe{

  \item{\code{\linkS4class{NeticaSession}}}{This is a container for the
  overall Netica session.  It is referenced when creating other Netica
  objects (\code{\linkS4class{NeticaBN}}s,
  {\code{\linkS4class{CaseStream}}}s and
  {\code{\linkS4class{NeticaRNG}}}) and contains the license key needed
  to activate Netica.  Its field \code{$nets} is an environment which
  contains references to all of the networks which have been associate
  with this session. }

  \item{\code{\linkS4class{NeticaBN}}}{This is a handle for a network
  object.  {\code{\linkS4class{NeticaNode}}} objects are created within
  a network, and the \code{$nodes} field is an environment which
  contains node references, at least for those nodes which have been
  referenced in R code.  Networks must have unique names within a
  session.}

  \item{\code{\linkS4class{NeticaNode}}}{This is a handle for a
  particular Netica node.  Nodes must have unique names within a
  network.  Many inference functions are done based on nodes.}

  \item{\code{\linkS4class{CaseStream}}}{This is a stream of Netica case
  data, values for particular nodes.  There are two subclasses:
  \code{\linkS4class{FileCaseStream}} and
  \code{\linkS4class{MemoryCaseStream}}.  As of version 5.04 of the
  Netica API, there are some issues with
  \code{\linkS4class{MemoryCaseStream}}s, so the
  \code{\linkS4class{FileCaseStream}}s should be used instead.}

  \item{\code{\linkS4class{NeticaRNG}}}{This a random number generator
  used by Netica for generating random cases.}
}

All of these follow the \code{\linkS4class{envRefClass}} protocol.  In
particular, their fields are referenced using \sQuote{$}.  Also, all of
them have a method \code{$isActive} (which is called from the
generic function \code{\link{is.active}}) which determines whether or not
the pointer to the Netica object currently exists or not.  Calling
\code{\link{stopSession}} will render all Netica objects inactive.

In particular, when quitting and restarting R, the pointers will all be
initialised to null, and all of the session, node and network objects
will become inactive.  Some examples of how to restart an RNetica
session are provided below.

To connect R to Netica, it is necessary to create and start a
\code{\linkS4class{NeticaSession}}.  This is done by first calling the
constructor \code{\link{NeticaSession}()} and then calling the function
\code{\link{startSession}(\var{session})}.  If you have purchased a
Netica license key from Norsys, this can be passed to the constructor
with the argument \code{LicenseKey} given the value of the license key
as a string.  Note that the session object can be saved in the
workspace, so that it can be used in future R session (it does not need
to be recreated, but it must be restarted with a call to
\code{startSession}).  If it is saved to \code{DefaultNeticaSession},
this value will be used as a default by all of the functions that use
the session as an argument.

\emph{Note that this is a change from how RNetica operated prior to
  version 0.5.}  In older versions of RNetica, the session pointer was
held inside of the C code, and the function
\code{\link{StartNetica}()} was invoked automatically when
the \code{RNetica} package was attached.  Nowt this needs to be
done manually through a call to \code{\link{startSession}}.

The function \code{\link{getDefaultSession}()} emulates the behaviour of
the previous version of RNetica.  It is the default value for all of
the functions which require a session argument.  When invoked, it looks
for an object call \code{DefaultNeticaSession} in the global
environment.  If that exists, it is used, if not, a new
\code{\linkS4class{NeticaSession}} is created.  If the new session is
created, it looks for a variable \code{NeticaLicenseKey} in the global
environment.  If that is present, it will use this as a license key.
Finally, if the \code{DefaultNeticaSession} is not active, it will start
it.

Note that it is almost certainly a mistake to have two sessions open at
the same time.  Users should either set the \code{DefaultNeticaSession},
and use the default, or always explicitly pass the session argument to
functions that need it.

The following functions take a session argument:
\code{\link{CaseFileDelimiter}}, \code{\link{CaseFileMissingCode}},
\code{\link{CaseFileStream}}, \code{\link{CaseMemoryStream}},
\code{\link{ClearAllErrors}}, \code{\link{CreateNetwork}},
\code{\link{GetNthNetwork}}, \code{\link{GetNamedNetworks}},
\code{\link{NeticaVersion}}, \code{\link{ReadNetworks}},
\code{\link{ReportErrors}},  \code{\link{StartNetica}},
\code{\link{StopNetica}}, \code{\link{startSession}},
and \code{\link{stopSession}}.

}
\section{Netica Networks}{

  \code{\linkS4class{NeticaBN}} objects are created through one of three
  functions:  \code{\link{CreateNetwork}()},
  \code{\link{ReadNetworks}()} and \code{\link{CopyNetworks}}.  The
  first two both require a session argument, while the third uses the
  session from its \var{net} argument.  When a network is created it is
  added as a symbol (using its name) to the \code{$nets} field of the
  session.  It can then be referenced using
  \code{\var{session}$nets$\var{netname}} or
  \code{\var{session}$nets[["\var{netname}"]]}.  The field
  \code{$Session} of the \code{\linkS4class{NeticaBN}} points to the
  \code{\linkS4class{NeticaSession}} object in which the network was
  created. 
  
  Note that \code{\var{session}$nets} cache may contain inactive network
  objects for one of two reasons:  (1) it is a deleted network object,
  or (2) this is a session which has been restored from a file, and
  the Netica pointers have not been reconnected.  In particular,
  quitting R will always deactivate the network.
  
  For networks, the simplest solution is to save each network to a file
  using \code{\link{WriteNetworks}()}.  If a
  \code{\linkS4class{NeticaBN}} object \var{net} is used in either a
  \code{\var{net} <- \link{ReadNetworks}()} or
  \code{\link{WriteNetworks}(\var{net})} call, then the R object will be
  badged with the name of the last used filename.  Thus, after saving
  and restoring a R session, the expression \code{\var{net} <-
  ReadNetworks(\var{net})} will recreate \var{net} as an object
  pointing to a new network that is identical to the last saved version.

}
\section{Netica Nodes}{

  \code{\linkS4class{NeticaNode}} objects are created through
  \code{\link{NewDiscreteNode}()} or \code{\link{NewContinuousNode}()},
  or retrieved from the network using \code{\link{NetworkFindNode}()},
  \code{\link{NetworkAllNodes}()}, \code{\link{NetworkNodesInSet}()}, or
  one of a variety of other functions that return nodes.  When a node
  is created it is  added as a symbol (using its name) to the
  \code{$node} field of the network.  It can then be referenced using
  \code{\var{net}$nodes\var{nodename}} or
  \code{\var{net}$nodes[["\var{nodename}"]]}.

  
  Note that if more than one network is loaded they may have identically
  named nodes that are not identical.  For example, \var{net1} and
  \var{net2} may both have a node named \dQuote{Proficiency}.  If the R
  variable \var{Proficiency} is bound to the
  \code{\linkS4class{NeticaNode}} object corresponding to the variable
  \dQuote{Proficiency} in \var{net1}, it can only be used to access the
  instance of that variable in \var{net1}, not the one in \var{net2}.

  Note that the \code{\linkS4class{NeticaNode}} object is created when
  then node is first references in R.  In particular, this means when a
  network is loaded through a call to \code{\link{ReadNetworks}}, the R
  objects for the corresponding Netica nodes are not immediately
  created.  The function \code{\link{NetworkAllNodes}()} returns a list
  of all nodes in the network, and as a side effect, creates
  \code{\linkS4class{NeticaNode}} object for all of the nodes found in
  the network.  If the network has many nodes, it may be more efficient
  to just create R objects for the ones which are used.  In this case
  the functions \code{\link{NetworkFindNode}()}, and
  \code{\link{NetworkNodesInSet}} are useful for finding (and creating R
  objects for) a subset of nodes.

  The following procedure can be used to save and restore a Netica
  network across sessions.  In the first session:

  \preformatted{
    DefaultNeticaSession <- NeticaSession()
    startSession(DefaultNeticaSession)
    net <- CreateNetwork("myNet",DefaultNeticaSession)
    # Work on the network.
    WriteNetworks(net,"myNet.dne")
    q("yes")
  }
  The variables \code{DefaultNeticaSession} and \code{net} will be saved
  in \code{.Rdata}.  Then in the next R session

  \preformatted{
    startSession(DefaultNeticaSession)
    net <- ReadNetworks(net,DefaultNeticaSession)
    net.nodes <- NetworkAllNodes(net)
  }

  This will read \var{net} from the place it was last saved.  It will
  also create R objects for all of the nodes in \var{net}.  This can now
  be access through \code{\var{net}$nodes} or the variable
  \code{net.nodes}. 

}
\section{Creating and Editing Networks}{

  Operations with Bayesian networks generally proceed in two phases:
  Building network, and conducting inference.
  This section describes the most commonly used options for building
  networks.  The following section describes the most commonly used
  options for inference.

  First, the function \code{\link{CreateNetwork}()} is used to create an
  empty network.  Multiple networks can be open within the RNetica
  environment, but each must have a unique name.  Names must conform to
  Netica's \code{\link{IDname}} rules.

  Nodes can be added to a network with the functions
  \code{\link{NewDiscreteNode}()} and
  \code{\link{NewContinuousNode}()}.  Note that Netica makes an internal
  distinction between these two types of nodes and a node cannot be
  changed from one type to another.  Nodes must all have a unique
  (within the network) name which must conform to the
  \code{\link{IDname}} rules.

  Edges between nodes are created using the
  \code{\link{AddLink}(\var{parent},\var{child})} function.  This forms
  a directed graph which must be acyclic (that is it must not be
  possible to follow a path along the direction of the arrows and return
  to the starting place).  The function
  \code{\link{NodeParents}(\var{child})} returns the current set of
  parents for the node \var{child} (nodes which have edges pointing
  towards \var{child}).  \code{NodeParents(\var{child})} may be set,
  which serves several purposes.  First, it allows connections to be
  added and removed.  Second, setting one of the parent locations to
  \code{NULL} produces a special \emph{Stub} node, which serves as a
  placeholder for a later connection.  Third, it allows one to reorder
  the nodes, which determines the order of the dimensions of the
  conditional probability table.

  A completed Bayesian network has a conditional probability table (CPT)
  associated with each node.  The CPT provides the conditional
  probability distributions of the node given the states of its parents
  in the graph.  RNetica provides two functions for accessing and
  setting this CPT.  The function \code{\link{NodeProbs}()} returns (or
  sets) the conditional probability table as a multi-dimensional
  array. However, using the array extractor \dQuote{[...]}
  (\code{\link{Extract.NeticaNode}}) allows the conditional probability
  table to be manipulated as a data frame, where the first several
  columns provide the states of the parent variables, and the remaining
  columns the probabilities of the the node being in each of those
  states given the parent configurations.  This latter approach has a
  number of features for working with large tables and tables with
  complex structure.  The double square bracket extractor
  \dQuote{[[...]]} (\code{\link{Extract.NeticaNode}}) words with
  logical \code{\link{IsNodeDeterministic}} nodes where parent
  configuration maps to exactly one state of the child.  So rather than
  returning a probability distribution, they return a value table.
  
  Finally, when the network is complete, the function
  \code{\link{WriteNetworks}()} can be used to save it to a file, which
  can either be later read into RNetica, or can be used with the Netica
  GUI or other applications that use the Netica API.
  
}
\section{Inference}{

  The basic purpose for building a Bayesian network is to rapidly
  calculate conditional probabilities.  In Netica language, one enters
  \emph{findings} (conditions) on the known or hypothesised variables
  and then calculates \emph{beliefs} (conditional probabilities) on
  certain variables of interest.

  Netica, like most Bayesian network software, uses two different
  graphical representations, one for model construction and one for
  inference.  The acyclic directed graph is use for model construction
  (previous section).  The function \code{\link{CompileNetwork}()}
  builds the second graphical representation:  the junction tree.  The
  function \code{\link{JunctionTreeReport}()} provides information about
  the compiled representation.

  While compiling can take a long time (depending on the size and
  connectivity of the network), repeated compilations appear to be
  harmless.  There is an \code{\link{UncompileNetwork}()} function, but
  performing any editing operation (adding or removing nodes or edges)
  will automatically return the network to an uncompiled state.  Netica
  tries to preserve finding information.  In particular the function
  \code{\link{AbsorbNodes}()} provides a mechanism for removing nodes
  from a network without changing the joint probability (including
  influence of findings) of the remaining nodes.  (The network must be
  recompiled after a call to \code{AbsorbNodes()} though.)

  The principle way to enter observed evidence is setting
  \code{\link{NodeFinding}(\var{node}) <- \var{value}}.  The function 
  \code{\link{NodeLikelihood}()} can be used to enter \emph{virtual
  evidence}, however, some care must be taken as it alters the meanings
  of several of the other functions.

  The conditional (given the entered findings and likelihoods)
  probability distribution can be queried at any time using the function
  \code{\link{NodeBeliefs}(\var{node})}.  If the states of a node have been
  given numeric values using \code{\link{NodeLevels}(\var{node})}, then
  \code{\link{NodeExpectedValue}(\var{node})} will calculate the expected
  numeric value (and the standard deviation).  The function
  \code{\link{JointProbability}(\var{nodelist})} calculates the joint
  distribution over a collection of nodes, and the function
  \code{\link{FindingsProbability}(\var{net})} calculates the prior
  probability of all the findings entered into the network.  The
  function \code{\link{MostProbableConfig}(nodelist)} finds the mode of
  the joint probability distribution (given the current findings and
  likelihood).


  Note that in the default state, when findings are entered, the beliefs
  about all other nodes in the network are then updated.  This can be
  time consuming in large networks.  The function
  \code{\link{SetNetworkAutoUpdate}()} can be used to change this to a
  lazy updating mode, when the evidence from the findings are only
  propagated when required for a call to \code{NodeBeliefs()} or a
  similar function.  The function
  \code{\link{WithoutAutoUpdate}(\var{net},\var{expr})} is useful for
  setting findings in a large number of nodes in \var{net} without the
  overhead of belief updating.
  
}
\section{Node Sets}{

  The function \code{\link{NodeSets}()} allows the modeller to attach
  labels to the nodes in the network.  For the most part, Netica ignores
  these labels, except that it will colour nodes from various sets
  different colours (\code{\link{NetworkNodeSetColor}()}).  Aside from a
  few internal labels used by Netica, these node sets are reserved for
  user programming.

  RNetica provides some functions that make node sets incredibly
  convenient ways to describe the intended usage of the nodes.  In
  particular, the function \code{\link{NetworkNodesInSet}()} returns a
  list of all nodes which are tagged as being in a particular node set.
  For example, suppose that the modeller has marked a number of nodes as
  being in the node set \code{"ReportingVar"}.  Then the following code
  would generate a report about the network:

  \preformatted{
    net.ReportingVars <- NetworkNodesInSet(net, "ReportingVar")
    lapply(net.ReportingVars, NodeBeliefs)
  }

}
\section{Warning}{

  The current status of RNetica is that of a beta
  release.  The code base is stable enough to do useful work, but more
  testing is still required.  Users are advised to work in such a way
  that they can easily recover from problems.

  In particular, because RNetica calls C code, there is a possibility
  that it will crash R.  There is also a possibility that pointers
  embedded in \code{\linkS4class{NeticaBN}} and
  \code{\linkS4class{NeticaNode}} objects will become corrupted.  If
  such problems occur, it is best to restart R and reload the networks.

  Please send information about both serious and not-so-serious problems
  to the maintainer.
  
}
\section{Legal Stuff}{
  Netica and Norsys are registered trademarks of Norsys, LLC, used by
  permission. 

  Although Norsys is generally supportive of the RNetica project, it
  does not officially support RNetica, and all questions should be sent
  to the package maintainers.

}
\author{
Russell Almond
\cr
Maintainer: Russell Almond <almond@acm.org>
}
\references{

  The general Netica manual can be found at:
  \url{http://www.norsys.com/WebHelp/NETICA.htm}
  
  The Netica API documentation can be found at
  \url{http://norsys.com/onLineAPIManual/index.html}.

  Almond, R. G. & Mislevy, R. J. (1999) Graphical models and computerized
  adaptive testing.  \emph{Applied Psychological Measurement}, 23,
  223--238.

  Almond, R. G., Mislevy, R. J., Steinberg, L. S., Yan, D. & Williamson,
  D. M. (2015) \emph{Bayesian Networks in Educational Assessment}.
  Springer. 

}
\keyword{ package }
\keyword{ interface }
\examples{
###########################################################
## Network Construction:
sess <- NeticaSession()
startSession(sess)

abc <- CreateNetwork("ABC", session=sess)
A <- NewDiscreteNode(abc,"A",c("A1","A2","A3","A4"))
B <- NewDiscreteNode(abc,"B",c("B1","B2","B3"))
C <- NewDiscreteNode(abc,"C",c("C1","C2"))

AddLink(A,B)
NodeParents(C) <- list(A,B)

NodeProbs(A)<-c(.1,.2,.3,.4)
NodeProbs(B) <- normalize(matrix(1:12,4,3))
NodeProbs(C) <- normalize(array(1:24,c(4,3,2)))
abcFile <- tempfile("peanut",fileext=".dne")
WriteNetworks(abc,abcFile)

DeleteNetwork(abc)

###################################################################
##  Inference using the EM-SM algorithm (Almond & Mislevy, 1999).  
## System/Student model
EMSMSystem <- ReadNetworks(file.path(library(help="RNetica")$path,
                           "sampleNets","System.dne"), session=sess)


## Evidence model for Task 1a
EMTask1a <- ReadNetworks(file.path(library(help="RNetica")$path,
                           "sampleNets","EMTask1a.dne"), session=sess)

## Evidence model for Task 2a
EMTask2a <- ReadNetworks(file.path(library(help="RNetica")$path,
                           "sampleNets","EMTask2a.dne"), session=sess)

## Task 1a has a footprint of Skill1 and Skill2 (those are the
## referenced student model nodes.  So we want joint the footprint into
## a single clique.
MakeCliqueNode(NetworkFindNode(EMSMSystem, NetworkFootprint(EMTask1a)))
## The footprint for Task2 a is already a clique, so no need to do
## anything. 

## Make a copy for student 1
student1 <- CopyNetworks(EMSMSystem,"student1")
## Monitor nodes for proficiency
student1.prof <- NetworkNodesInSet(student1,"Proficiency")

student1.t1a <- AdjoinNetwork(student1,EMTask1a)
## We are done with the original EMTask1a now
DeleteNetwork(EMTask1a)

## Now add findings
CompileNetwork(student1)
NodeFinding(student1.t1a$Obs1a1) <- "Right"
NodeFinding(student1.t1a$Obs1a2) <- "Right"

student1.probt1a <- JointProbability(student1.prof)

## Done with the observables, absorb them
AbsorbNodes(student1.t1a)
CompileNetwork(student1)
student1.probt1ax <- JointProbability(student1.prof)

## Now Task 2
student1.t2a <- AdjoinNetwork(student1,EMTask2a,"t2a")
DeleteNetwork(EMTask2a)

## Add findings
CompileNetwork(student1)
NodeFinding(student1.t2a$Obs2a) <- "Half"

AbsorbNodes(student1.t2a)
CompileNetwork(student1)
student1.probt1a2ax <- JointProbability(student1.prof)

DeleteNetwork(list(student1, EMSMSystem))
stopSession(sess)

}
