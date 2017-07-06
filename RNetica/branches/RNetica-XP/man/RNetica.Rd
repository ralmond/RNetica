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

  Once you have a license key, you can use it in one of two ways.
  First, it can be used as an argument to the function
  \code{\link{StartNetica}()}.  As this function is called when RNetica
  is loaded, you may need to call \code{StopNetica()} first and restart
  the licensed version.  Alternatively, if you set the variable
  \code{NeticaLicenseKey} in the R top-level environment before the call
  to \code{library(RNetica)}, RNetica will pick up the license key from
  that location.

  Without the license key, the Netica shared library will be restricted
  to a student/demonstration mode with limited functionality.  Note that
  all of the example code (and hence \code{R CMD check RNetica}) can be
  run using the limited version.

}
\section{Index}{

  \packageIndices{Peanut}

}
\section{RNetica Environment and Netica Objects}{

Netica exists in both as a stand alone graphical tool for building and
manipulating Bayesian networks (the Netica GUI) and as a shared library
for manipulating Bayesian networks (the Netica API).  The RNetica
package binds the API version of Netica to a series of R functions which
do much of the work of manipulating the network.  The file format
for the GUI and API version of Netica is identical, so analysts can
easily move back and forth between the two.

The function \code{\link{StartNetica}()} (invoked automatically when
\code{library(RNetica)} is called) builds a Netica environment which can be
accessed from R.  Networks created and loaded into the RNetica
environment can then be manipulated from inside of R.  Note that the
RNetica environment is separate from other Netica environments that may
be created using the Netica GUI (or API invoked from a different
program); RNetica can only manipulate the networks that are currently
loaded into its environment.

The key to this process is that the two most common functions for
creating networks, \code{\link{CreateNetwork}()} and
\code{\link{ReadNetworks}()} both return a special object of class
\code{\link{NeticaBN}} which encapsulates a pointer back to the Bayesian
network in the RNetica environment.  This object can be manipulated with
the functions in this package.

Netica nodes (created through \code{\link{NewDiscreteNode}()} or
\code{\link{NewContinuousNode}()}, or retrieved from the network using
\code{\link{NetworkFindNode}()}, \code{\link{NetworkAllNodes}()},
\code{\link{NetworkNodesInSet}()}, or one of a variety of other
functions that return nodes) are represented as special objects of class
\code{NeticaNode} which contain pointers to the node in a Netica
network.  Netica nodes know which network they belong to, so each node
implicitly references its network.

Note that if more than one network is loaded they may have identically
named nodes that are not identical.  For example, \code{net1} and
\code{net2} may both have a node named \dQuote{Proficiency}.  If the R
variable \code{Proficiency} is bound to the \code{NeticaNode} object
corresponding to the variable \dQuote{Proficiency} in \code{net1}, it
can only be used to access the instance of that variable in \code{net1},
not the one in \code{net2}.

Because of the way R likes to hang onto references to objects, it is
quite possible for a \code{NeticaBN} or \code{NeticaNode} object to hang
around after it has been deleted, renamed or otherwise rendered
invalid.  The function \code{\link{is.active}()} does a quick check to
make sure that the pointer to the object in the RNetica environment has
not be set to \code{NULL}.

Note that unlike ordinary R objects, \code{NeticaBN} and
\code{NeticaNode} objects only last as long as the RNetica environment
lasts.  In particular, if \code{\link{StopNetica}()} is called to close
the RNetica environment, or the R session is exited (either cleanly or
through a crash), then all of the \code{NeticaBN} and \code{NeticaNode}
objects should become inactive.  It is an error to execute RNetica
functions with the old objects.

For networks, the simplest solution is to save each network to a file
using \code{\link{WriteNetworks}()}.  If a \code{NeticaBN} object
\code{net} is used in either a \code{net <- ReadNetworks()} or
\code{WriteNetworks(net)} call, then the R object will be badged with
the name of the last used filename.  Thus, after saving and restoring a
R session, the expression \code{net <- ReadNetworks(net)} will recreate
\code{net} as an object pointing to a new network that is identical to
the last saved version.

For nodes, the best solution is to use a query function to return a list
of the desired nodes, in particular, \code{\link{NetworkFindNode}()} or
\code{\link{NetworkAllNodes}()}.  If a particular subset of nodes should
be loaded every time the network is loaded, then they can be placed in a
node set, and the function \code{\link{NetworkNodesInSet}()} can be used
to retrieve just the interesting nodes.  All of these functions return a
list of \code{NeticaBN} objects, which can be used to provide convenient
access.  For example, if \code{net} was previously saved and
\dQuote{Proficiency} is a node in \code{net}, then:
\preformatted{
  net <- ReadNetworks(net)
  net.nodes <- NetworkAllNodes(net)
}
will load all of the nodes in \code{net}, and the expression
\code{net.nodes$Proficiency} will access the \dQuote{Proficiency} node.

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
  \code{\link{AddLink}(parent,child)} function.  This forms
  a directed graph which must be acyclic (that is it must not be
  possible to follow a path along the direction of the arrows and return
  to the starting place).  The function
  \code{\link{NodeParents}(child)} returns the current set of
  parents for the node \kbd{child} (nodes which have edges pointing
  towards \kbd{child}).  \code{NodeParents(child)} may be set,
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
  array. However, using the array extractor \code{\link{[.NeticaNode}} %]
  allows the conditional probability table to be manipulated as a data
  frame, where the first several columns provide the states of the
  parent variables, and the remaining columns the probabilities of the
  the node being in each of those states given the parent
  configurations.  This latter approach has a number of features for
  working with large tables and tables with complex structure.

  Finally, when the network is complete, the function
  \code{\link{WriteNetworks}()} can be used to save it to a file, which
  can either be later read into RNetica, or can be used with the Netica
  GUI or other applications that use the Netica API.
  
}
\section{Inference}{

  The basic purpose for building a Bayesian network is to rapidly
  calculate conditional probabilities.  In Netica language, one enters
  \dQuote{findings} (conditions) on the known or hypothesized variables
  and then calculates \dQuote{beliefs} (conditional probabilities) on
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
  \code{\link{NodeFinding}(node) <- value}.  The function
  \code{\link{NodeLikelihood}()} can be used to enter \dQuote{virtual
  evidence}, however, some care must be taken as it alters the meanings
  of several of the other functions.

  The conditional (given the entered findings and likelihoods)
  probability distribution can be queried at any time using the function
  \code{\link{NodeBeliefs}(node)}.  If the states of a node have been
  given numeric values using \code{\link{NodeLevels}(node)}, then
  \code{\link{NodeExpectedValue}(node)} will calculate the expected
  numeric value (and the standard deviation).  The function
  \code{\link{JointProbability}(nodelist)} calculates the joint
  distribution over a collection of nodes, and the function
  \code{\link{FindingsProbability}(net)} calculates the prior
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
  \code{\link{WithoutAutoUpdate}(net,expr)} is useful for
  setting findings in a large number of nodes in \kbd{net} without the
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

  The current status of RNetica is that of a late alpha to early beta
  release.  The code base is stable enough to do useful work, but more
  testing is still required.  Users are advised to work in such a way
  that they can easily recover from problems.

  In particular, because RNetica calls C code, there is a possibility
  that it will crash R.  There is also a possibility that pointers
  embedded in \code{\link{NeticaBN}} and \code{\link{NeticaNode}}
  objects will become corrupted.  If such problems occur, it is best to
  restart R and reload the networks.

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

abc <- CreateNetwork("ABC")
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
EMSMSystem <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","System.dne",
                           sep=.Platform$file.sep))

## Evidence model for Task 1a
EMTask1a <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","EMTask1a.dne",
                           sep=.Platform$file.sep))

## Evidence model for Task 2a
EMTask2a <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","EMTask2a.dne",
                           sep=.Platform$file.sep))

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

}
