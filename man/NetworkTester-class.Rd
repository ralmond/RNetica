\name{NetworkTester-class}
\Rdversion{1.1}
\docType{class}
\alias{NetworkTester-class}

\title{Class \code{"NetworkTester"}}
\description{

  The Network tester is special object that simulates data from a Bayes
  net and then tests how well the values of the \code{targetNodes} can be
  recovered from evidence in the remaining nodes (excluding the nodes in
  the \code{ignoreNodes}.  
}
\section{Objects from the Class}{

  Objects can be created by calling \code{\link{testNetwork}(targetNodes,
    dataStreams, ingoreNodes)}.  Here \code{dataStreams} should be a
  list of active \code{\linkS4class{CaseStream}} objects
  containing generated test data for the network.
}
\section{Slots}{
  \describe{
    \item{\code{Net}:}{Object of class \code{"NeticaBN"} which
      represents the network to be tested. }
    \item{\code{targetNodes}:}{A list of active \code{\linkS4class{NeticaNode}}
      objects which represent the nodes to be tested.}
    \item{\code{ignoreNodes}:}{A (possibly empty) list of active
      \code{\linkS4class{NeticaNode}} objects whose values will be ignored
      (not instantiated) during the tests.}
    \item{\code{data}:}{A list of active (open)
      \code{\linkS4class{CaseStream}} objects containing the test data.}
    \item{\code{errorRate}:}{A vector corresponding to the target nodes
      which contains the error rates from the test (see
      \code{\link{testerErrorRate}}). }
    \item{\code{logLoss}:}{A vector corresponding to the target nodes
      which contains the log loss rates from the test (see
      \code{\link{testerLogLoss}}). }
    \item{\code{quadraticLoss}:}{A vector corresponding to the target nodes
      which contains the quadratic loss rates from the test (see
      \code{\link{testerQuadraticLoss}}). }
    \item{\code{confusion}:}{A list of matrixes corresponding to the
      target nodes.  Each matrix is a list corresponding to the states
      of the target node with rows representing the predicted values and
      actual representing the actual (simulated) value.  (See     
      \code{\link{testerConfusion}}). }
  }
}
\details{

  The fuction \code{\link{testNetwork}} behaves a follows:  It opens a
  \code{\linkS4class{NetworkTester}} object to hold the results.  It
  then runs through the case streams.  For each case, it instantiates
  each node in the network to the findings as found in the case stream,
  except for those nodes in \code{targetNodes} or \code{ignoreNodes}.
  It then looks at the \code{NodeBeliefs} associated with the
  \code{targetNodes} and compares these with the actual value of the
  target nodes as found in the case stream.  It accumulates statistics
  related to the accuracy.

  After all case streams are processed, the
  \code{\linkS4class{NetworkTester}} object is returned and its
  statistics can be queried.  See \code{\link{testerConfusion}} for a
  list of available results.


}
\section{Methods}{

  \describe{
    \item{\code{\link{testNetwork}(targetNodes, dataStreams,
        ingoreNodes=list())}:}{ This is the de facto constructor, it
      runs a test on the network and stores the results in tester object.} 
    \item{\code{\link{testerNet}(tester)}:}{Accessor for the network.}
    \item{\code{\link{testerTarget}(tester)}:}{Accessor for the target nodes.}
    \item{\code{\link{testerIgnore}(tester)}:}{Accessor for the ignored nodes.}
    \item{\code{\link{testerErrorRate}(tester,node=NULL)}:}{Accessor for
      the error rate, if \code{node} is supplied, will give specific
      error rate, otherwise, it will give all of them.}
    \item{\code{\link{testerLogLoss}(tester,node=NULL)}:}{Accessor for
      the log loss, if \code{node} is supplied, will give specific
      log loss, otherwise, it will give all of them.}
    \item{\code{\link{testerQuadraticLoss}(tester,node=NULL)}:}{Accessor for
      the quadratic loss, if \code{node} is supplied, will give specific
      loss, otherwise, it will give all of them.}
    \item{\code{\link{testerConfusion}(tester,node=NULL)}:}{Accessor for
      the confusion matrixes, if \code{node} is supplied, will give specific
      confusion matrix, otherwise, it will give a list of all of them.}
    \item{\code{\link{testerKappa}(tester,node=NULL,
        weights=c("None","Linear","Quadratic"), W=NULL)}:}{Will
      calculate Cohen's Kappa for the specified node or for all of
      them. }
    \item{\code{\link{testerLambda}(tester,node=NULL,
        weights=c("None","Linear","Quadratic"), W=NULL)}:}{Will
      calculate Goodman and Kruskal's Lambda for the specified node or
      for all of them. }
    \item{\code{\link{summary.NetworkTester}(object,...)}:}{Produces a
      matrix giving the error rate, log loss, quadratic loss, about both
      weighted and unweighted kappas and lambdas.}
    
  }
  [Note:  These are implmemented as ordinary functions and not as methods.]

}
\references{
  Almond, R.G., Mislevy, R.J. Steinberg, L.S., Yan, D. and Willamson, D.
  M. (2015). \emph{{Bayesian} Networks in Educational Assessment}.
  Springer.  Chapter 7.

  \newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{NewNetTester_bn}, \nref{DeleteNetTester_bn},
  \nref{TestWithCaseset_bn}, \nref{GetTestConfusion_bn},
  \nref{GetTestErrorRate_bn}, \nref{GetTestLogLoss_bn},
  \nref{GetTestQuadraticLoss_bn}
}
\author{Russell Almond}
\note{

  Although the Netica API defines a NetTester object, this object is
  both created and destroyed in the C code for
  \code{\link{testNetwork}}.  That code creates the tester object, runs
  the tests and extracts the results and places the cached results in an
  R object.  This avoids memory management issues with foreign pointers.

  Note that new data cannot simply be added to the tester, as works
  using the Netica API.  However, tester objects can have multiple data
  streams.

}
\seealso{
  The output of \code{\link{testNetwork}} is \code{\linkS4class{NetworkTester}}.
 
  Accessors for \code{\linkS4class{NetworkTester}}
  \code{\link{testerConfusion}}, \code{\link{testerErrorRate}},
  \code{\link{testerIgnore}}, \code{\link{testerKappa}}, 
  \code{\link{testerLambda}}, \code{\link{testerLogLoss}},
  \code{\link{testerNet}}, \code{\link{testerQuadraticLoss}}, 
  \code{\link{testerTarget}}

}
\examples{
sess <- NeticaSession()
startSession(sess)

irt5 <- ReadNetworks(system.file("sampleNets","IRT5.dne",
                                 package="RNetica"),
                     session=sess)

irt5.theta <- NetworkFindNode(irt5,"Theta")
irt5.x <- NetworkFindNode(irt5,paste("Item",1:5,sep="_"))

CompileNetwork(irt5)


## This generates a fixed series of random cases and saves them to a
## file. 
N <- 100L
rnodes <- c(list(irt5.theta),irt5.x)
casefile <- tempfile("irt5testcase",fileext=".cas")
filestream <- CaseFileStream(casefile, session=sess)
rng <- NewNeticaRNG(123456779, session=sess)
WithOpenCaseStream(filestream,
  WithRNG(rng,
    for (n in 1L:N) {
      GenerateRandomCase(rnodes,rng=rng)
      WriteFindings(rnodes,filestream,n)
      lapply(rnodes,RetractNodeFinding) # Only retract findings for
                                        # generated nodes
    }))


irt5.test <- testNetwork(list(irt5.theta),OpenCaseStream(filestream))
summary(irt5.test)


DeleteNetwork(irt5)
stopSession(sess)

}
\keyword{classes}
