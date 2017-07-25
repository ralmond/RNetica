\name{EnterFindings}
\alias{EnterFindings}
\title{
  Enters findings for multiple nodes in a Netica network.
}
\description{
  This function takes two arguments, a network and a list of nodes and
  the corresponding findings.  It sets all of the findings at once.
}
\usage{
EnterFindings(net, findings)
}
\arguments{
  \item{net}{
    An active and compiled \code{\link{NeticaBN}}.
  }
  \item{findings}{
    An integer or character vector giving the findings.  The
    \code{names(findings)} should be names of nodes in \code{net}.  The
    values of \code{findings} should be corresponding states either
    expressed as a character string or as an integer index into the
    list of states for that node.  (See
    \code{\link{NodeFinding}(node)}.  
  }
}
\details{
  
  This function enters findings for multiple nodes at the same time.  It
  offers two improvements over repeated calls to \code{NodeFinding()}.
  First, it finds the nodes by name in the network, making it easier to
  work with data in the form of key--value pairs that might come from
  other systems.  Second, it wraps the calls to \code{NodeFinding()} in
  a call to \code{\link{WithoutAutoUpdate}()} which should only
  propagate the new findings after all values have been entered.
  
}
\value{
  The value of \code{net} is returned invisibly.
}
\author{
  Russell Almond
}
\seealso{
  \code{\link{NeticaBN}}, \code{\link{NodeBeliefs}()},
  \code{\link{EnterNegativeFinding}()}, \code{\link{NodeFinding}()},
  \code{\link{RetractNodeFinding}()}, \code{\link{NodeLikelihood}()},
  \code{\link{EnterGaussianFinding}()}, \code{\link{EnterIntervalFinding}()},
  \code{\link{JointProbability}()},  \code{\link{NodeValue}()},
  \code{\link{MostProbableConfig}()},
  \code{\link{FindingsProbability}()}
}
\examples{

Motif <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","EMSMMotif.dne",
                           sep=.Platform$file.sep))

CompileNetwork(Motif)
obs <- c(Obs1a1="Right",Obs1a2="Wrong",
         Obs1b1="Right",Obs1b2="Wrong",
         Obs2a="Half", Obs2b="Half")

EnterFindings(Motif,obs)
JointProbability(NetworkNodesInSet(Motif,"Proficiency"))

DeleteNetwork(Motif)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ interface }
\keyword{ manip }% __ONLY ONE__ keyword per line
