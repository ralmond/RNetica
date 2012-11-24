\name{RNetica-package}
\alias{RNetica-package}
\alias{RNetica}
\docType{package}
\title{R interface to Netica Bayesian Network Engine}
\description{
This provides an R interface to the Netica, in particular, it binds many
of the functions in the Netica C API into the R langauge.  RNetica can
create and modify networks, enter evidence and extract the conditional
probabilities from a Netica network.
}
\details{
\tabular{ll}{
Package: \tab RNetica\cr
Version: \tab 0.2-6\cr
Date: \tab 2012/11/22\cr
Depends: \tab R (>= 2.0), methods, utils, grDevices\cr
(http: \tab //norsys.com/) Bayesian network library API\cr
License: \tab Artistic-2.0 + file LICENSE\cr
URL: \tab http://ralmond.net/RNetica\cr
Built: \tab R 2.15.2; x86_64-pc-linux-gnu; 2012-11-22 19:38:18 UTC; unix\cr
}
}
\section{License}{

  While RNetica (the combination of R and C code that connects R and
  Netica) is free software, as is \R, Netica is a commercial product.
  Users of RNetica will need to purchase a Netica API license key (which
  is different from the GUI license key) from Norsys
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
\preformatted{
AbsorbNodes             Delete a Netica nodes in a way that maintains
                        the connectivity.
AddLink                 Adds or removes a link between two nodes in a
                        Netican network.
AdjoinNetwork           Links an evidence model network to a system
                        model netwok.
CPA                     Representation of a conditional probability
                        table as an array.
CPF                     Representation of a conditional probability
                        table as a data frame.
CompileNetwork          Builds the junction tree for a Netica Network
CopyNetworks            Makes copies of Netica networks.
CopyNodes               Copies or duplicates nodes in a Netica network.
CreateNetwork           Creates (destroys) a new Netica network.
DeleteNodeTable         Deletes the conditional probability table of a
                        Netica node.
EliminationOrder        Retrieves or sets the elimination order used in
                        compiling a Netica network.
EnterFindings           Enters findings for multiple nodes in a Netica
                        network.
EnterNegativeFinding    Sets findings for a Netaca node to a list of
                        ruled out values.
Extract.NeticaNode      Extracts portions of the conditional
                        probability table of a Netica node.
FindingsProbability     Finds the probability of the findings entered
                        into a Netica network.
GetNamedNetworks        Finds a Netica network (if it exists) for the
                        name.
GetNetworkAutoUpdate    Turns Netica automatic updating on or off for a
                        network.
GetNthNetwork           Fetch a Netica network by its position in the
                        Netica list.
HasNodeTable            Tests to see if a Netica node has a conditional
                        probability table.
IDname                  Tests to see if a string is a valid as a Netica
                        Identifier.
IsNodeDeterministic     Determines if a node in a Netica Network is
                        deterministic or not.
JointProbability        Calculates the joint probability over several
                        network nodes.
JunctionTreeReport      Produces a report about the junction tree from
                        a compiled Netica network.
MakeCliqueNode          Forces a collection of nodes in a Netica
                        network to be in the same clique.
MostProbableConfig      Finds the configuration of the nodes most likel
                        to have lead to observed findings.
NeticaBN                An object referencing a Bayesian network in
                        Netica.
NeticaNode              An object referencing a node in a Netica
                        Bayesian network.
NeticaVersion           Fetches the version number of Netica.
NetworkFindNode         Finds nodes in a Netica network.
NetworkFootprint        Returns a list of names of unconnected edges.
NetworkName             Gets or Sets the name of a Netica network.
NetworkNodeSetColor     Returns or sets a display colour to use with a
                        netica node.b
NetworkNodeSets         Returns a list of node sets associated with a
                        Netica network.
NetworkNodesInSet       Returns a list of node labeled with the given
                        node set in a Netica Network.
NetworkSetPriority      Changes the priority order of the node sets.
NetworkTitle            Gets the title or comments associated with a
                        Netica network.
NetworkUndo             Undoes (redoes) a Netica operation on a
                        network.
NetworkUserField        Gets user definable fields associated with a
                        Netica network.
NewDiscreteNode         Creates (or destroys) a node in a Netica
                        Bayesian network.
NodeBeliefs             Returns the current marginal probability
                        distribution associated with a node in a Netica
                        network.
NodeChildren            Returns a list of the children of a node in a
                        Netica network.
NodeFinding             Returns of sets the observed value associated
                        with a Netica node.
NodeInputNames          Associates names with incomming edges on a
                        Netica node.
NodeKind                Gets or changes the kind of a node in a Netica
                        network.
NodeLevels              Accesses the levels associated with a Netica
                        node.
NodeLikelihood          Returns or sets the virtual evidence associated
                        with a Netica node.
NodeName                Gets or set of a Netica node.
NodeNet                 Finds which Netica network a node comes from.
NodeParents             Gets or sets the parents of a node in a Netica
                        network.
NodeProbs               Gets or sets the conditional probability table
                        associated with a Netica node.
NodeSets                Lists or changes the node sets associated with
                        a Netica node.
NodeStateTitles         Accessors for the titles and comments
                        associated with states of Netica nodes.
NodeStates              Accessor for states of a Netica node.
NodeTitle               Gets the title or Description associated with a
                        Netica node.
NodeUserField           Gets user definable fields associated with a
                        Netica node.
NodeVisPos              Gets, sets the visual position of the node on
                        the Netica display.
NodeVisStyle            Gets/sets the nodes visual appearence in
                        Netica.
ParentStates            Returns a list of the names of the states of
                        the parents of a Netica node.
RetractNodeFinding      Clears any findings for a Netica node or
                        network.
ReverseLink             Reverses a link in a Netica network.
StartNetica             Starting and stopping the Netica shared
                        library.
WriteNetworks           Reads or writes a Netica network from a file.
is.NodeRelated          Computes topological properities of a 'Netica'
                        network.
is.active               Check to see if a Netica network or node object
                        is still valid.
is.discrete             Determines whether a Netica node is discrete or
                        continuous.
normalize               Normalizes a conditional probability table.
}}
\section{RNetica Environment and Netica Objects}{

Netica exists in both as a stand alone graphical tool for building and
manipulating Bayesian networks (the Netica GUI) and as a shared library
for manipulating Bayesian networks (the Netica API).  The RNetica
package binds the API version of Netica to a series of R functions which
do much of the work of manipulating the network.  The file format
for the GUI and API version of Netica is identical, so analysts can
easily move back and forth between the two.

The function \code{\link{StartNetica}()} (invoked automatically when
\code{library(RNetica)}) builds a Netica environment which can be
accessed from R.  Networks created and loaded into the RNetica
environment can then be manipulted from inside of R.  Note that the
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
correspoinding to the variable \dQuote{Proficiency} in \code{net1}, it
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
  associated with each node.  The CPT provies the conditional
  probability distributions of the node given the states of its parents
  in the graph.  RNetica provides two functions for accessing and
  setting this CPT.  The function \code{\link{NodeProbs}()} returns (or
  sets) the conditional probability table as a multi-dimensional
  array. However, using the array extractor \code{\link{[.NeticaNode}} %]
  allows the conditional probability table to be manipulated as a data
  frame, where the first several columns provide the states of the
  parent variables, and the remaining columns the probabilities of the
  the node being in each of thoses states given the parent
  configurations.  This latter approach has a number of features for
  working with large tables and tables with complex structure.

  Finally, when the network is complete, the function
  \code{\link{WriteNetworks}()} can be used to save it to a file, which
  can either be later read into RNetica, or can be used with the Netica
  GUI or other applications that use the Netica API.
  
}
\section{Inference}{

  The basic purpose for building a Bayesian network is to raplidly
  calculate conditional probabilities.  In Netica language, one enters
  \dQuote{findings} (conditions) on the known or hypothesized variables
  and then calculates \dQuote{beliefs} (conditional probalities) on
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
  \code{\link{NodeBeliefs}()}.  The function
  \code{\link{JointProbability}()} calculates the joint distribution
  over a collection of nodes, and the function
  \code{\link{FindingsProbability}()} calculates the prior probability
  of the observed findings.  The function
  \code{\link{MostProbableConfig}()} finds the mode of the joint
  probability distribution (given the current findings and likelihood).

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
  convenient ways to describe the indended usage of the nodes.  In
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

  In particular, because RNetica calls C code, there is a possiblity
  that it will crash R.  There is also a possibility that pointers
  embedded in \code{\link{NeticaBN}} and \code{\link{NeticaNode}}
  objects will become corrupted.  If such problems occur, it is best to
  restart R and reload the networks.

  Please send information about both serious and not-so-serious problems
  to the maintainer.
  
}
\author{
Russell Almond
\cr
Maintainer: Russell Almond <almond@acm.org>
}
\references{

  The Netica API documentation can be found at
  \url{http://norsys.com/onLineAPIManual/index.html}.

  Almond, R. G. & Mislevy, R. J. (1999) Graphical models and computerized
  adaptive testing.  \emph{Applied Psychological Measurement}, 23, 223--238.

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