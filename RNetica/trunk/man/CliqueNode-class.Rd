\name{CliqueNode-class}
\Rdversion{1.1}
\docType{class}
\alias{CliqueNode-class}
\alias{toString,CliqueNode-method}

\title{Class \code{"CliqueNode"}}
\description{

  A dummy node used to force it parents into the same clique in the
  junction tree.  In particular, the node has a single state but its
  parents are listed in its \code{clique} field.

}
\section{Extends}{
Class \code{"\linkS4class{NeticaNode}"}, directly.

All reference classes extend and inherit methods from
\code{"\linkS4class{envRefClass}"}. Note that because this is a
reference class unlike traditional S3 and S4 classes it can be
destructively modified.  Also fields (slots) are accessed using the
\sQuote{$} operator.

}
\section{Methods}{
  \describe{
    \item{toString}{\code{signature(x = "CliqueNode")}: Provides a
      pretited representation. }
  }
}
\references{
Almond, R. G. & Mislevy, R. J. (1999) Graphical models and computerized
adaptive testing.  \emph{Applied Psychological Measurement}, 23, 223-238.

Almond, R., Herskovits, E., Mislevy, R. J., & Steinberg,
L. S. (1999). Transfer of information between system and evidence
models. In Artificial Intelligence and Statistics 99, Proceedings
(pp. 181--186). Morgan-Kaufmann

\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  See the NeticaEx function \code{FormCliqueWith} is the documentation
  for \nref{JointProbability_bn}
}
\author{Russell Almond}
\note{
  Clique nodes only last for the R session that was used to create
  them.  After that, they will appear like ordinary nodes.  They will
  still be present in the network, but the special \code{"clique"}
  attribute will be lost.

  Currently Netica only allows virtual evidence at the node level
  (\code{\link{NodeLikelihood}()}).  I'm lobbying to get Netica to
  support it at the clique level as well.  At which point, this function
  becomes extremely useful.
}

\seealso{
  \code{\link{MakeCliqueNode}()}, \code{\linkS4class{NeticaNode}},
  \code{\link{JointProbability}()}, \code{\link{AddLink}()},
  \code{\link{JunctionTreeReport}()}

}
\examples{
sess <- NeticaSession()
startSession(sess)

EMSMSystem <- ReadNetworks(file.path(library(help="RNetica")$path,
                           "sampleNets","System.dne"), session=sess)

CompileNetwork(EMSMSystem)
## Note that Skill1 and Skill2 are in different cliques
JunctionTreeReport(EMSMSystem)

Skills12 <- NetworkFindNode(EMSMSystem,c("Skill1","Skill2"))
cn <- MakeCliqueNode(Skills12)
cnclique <- GetClique(cn)

stopifnot(
  is.CliqueNode(cn),
  setequal(sapply(cnclique,NodeName),sapply(Skills12,NodeName))
)

CompileNetwork(EMSMSystem)
## Note that Skill1 and Skill2 are in different cliques
JunctionTreeReport(EMSMSystem)

DeleteNodes(cn) ## This clears the clique.

DeleteNetwork(EMSMSystem)
stopSession(sess)

}
\keyword{ interface }
\keyword{ misc }
\keyword{classes}
\section{Fields}{
  Note these should be regarded as read-only from user code.
  \describe{
    \item{\code{Name}:}{Object of class \code{character} giving the
      Netica name of the node.  Must follow the \code{\link{IDname}} rules. }
    \item{\code{Netica_Node}:}{Object of class \code{externalptr} giving
      the address of the node in Netica's memory space. }
    \item{\code{Net}:}{Object of class \code{\linkS4class{NeticaBN}}, a
      back reference to the network in which this node resides. }
    \item{\code{discrete}:}{Always \code{TRUE} for clique nodes.}
    \item{\code{clique}:}{A list of \code{\linkS4class{NeticaNode}}
      objects which are the parents of the clique node. }
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{show()}:}{ Prints a description of the node. }
    \item{\code{initialize(..., clique)}:}{ Internal initializer, should
    not be called directly by user code.  Use
    \code{\link{MakeCliqueNode}} instead.}
  }

The following methods are inherited (from the \code{\linkS4class{NeticaNode}}): 
deactivate ("NeticaNode"), isActive ("NeticaNode"), show ("NeticaNode"),
clearErrors ("NeticaNode"), reportErrors ("NeticaNode"), initialize
("NeticaNode") 
}
