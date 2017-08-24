\name{FindingsProbability}
\alias{FindingsProbability}
\title{
  Finds the probability of the findings entered into a Netica network.
}
\description{
  This function assumes that the network has been compiled and that a
  number of findings have been entered.  The function calculates the
  prior probability for the entered findings (that is, the
  normalization constant of the Bayesian network).  
}
\usage{
FindingsProbability(net)
}
\arguments{
  \item{net}{
    An active and compiled Bayesian Network (class
    \code{\linkS4class{NeticaBN}}). 
  }
}
\details{

  In the usual algorithms for propagating probabilities in a Bayesian
  network the probabilities are passed unnormalized.  When reporting the
  probabilities, a normalization constant is calculated.  This
  normalization constant is the probability of all of the findings that
  have been entered through \code{\link{NodeFinding}()}.  (See Almond,
  1995, for details on the use of normalization constants as
  probabilities of findings.)

  It is not meaningful to call this function before the network has been
  compiled.  Calling it before findings have been entered will result in
  a value of \code{1.0}.
}
\value{
  A scalar real value representing the probability of the findings, or
  \code{NA} if the network was not found or not compiled.
}
\references{

  Almond, R. G. (1995) \emph{Graphical Belief Modeling}.  Chapman and
  Hall.

  \newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{FindingsProbability_bn}

}
\author{
  Russell Almond
}
\note{
  Netica gives a warning about the interpretation if likelihood findings
  have been set (through \code{\link{NodeLikelihood}()}.  In this case,
  the value is perhaps better though of as a normalization constant.
}
\seealso{
  \code{\linkS4class{NeticaNode}},\code{\linkS4class{NeticaBN}},
  \code{\link{NodeBeliefs}()},
  \code{\link{EnterNegativeFinding}()},
  \code{\link{RetractNodeFinding}()}, \code{\link{NodeLikelihood}()}
}
\examples{
sess <- NeticaSession()
startSession(sess)

EMSMMotif <- ReadNetworks(file.path(library(help="RNetica")$path,
                           "sampleNets","EMSMMotif.dne"), session=sess)

CompileNetwork(EMSMMotif)
norm1 <- FindingsProbability(EMSMMotif)
stopifnot ( abs(norm1-1) <.0001)

## Find observable nodes
obs <- NetworkNodesInSet(EMSMMotif,"Observable")

NodeFinding(obs$Obs1a1) <- "Right"
NodeFinding(obs$Obs1a2) <- "Wrong"

prob1r2w <- FindingsProbability(EMSMMotif)
stopifnot (prob1r2w < 1, prob1r2w > 0)

## Clear it out and try again
RetractNetFindings(EMSMMotif)
NodeLikelihood(obs$Obs2a) <- c(.75,.75,.75)
prob75 <- FindingsProbability(EMSMMotif)
stopifnot( abs(prob75-.75) < .0001)

DeleteNetwork(EMSMMotif)
stopSession(sess)

}
\keyword{ interface }
\keyword{ manip }
