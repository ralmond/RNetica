\name{testerConfusion}
\alias{testerConfusion}
\alias{testerErrorRate}
\alias{testerIgnore}
\alias{testerKappa}
\alias{testerLogLoss}
\alias{testerNet}
\alias{testerQuadraticLoss}
\alias{testerTarget}
\alias{testerLambda}
\alias{summary.NetworkTester}
\title{Accessor methods for class \code{"NetworkTester"}}

\description{

  The \code{\linkS4class{NetworkTester}} object is created by calls to
  the \code{\link{testNetwork}} function, and contains the cached
  results from the test.  These function return the cached results.
}
\usage{
testerNet(tester)
testerTarget(tester)
testerIgnore(tester)
testerErrorRate(tester, node = NULL)
testerLogLoss(tester, node = NULL)
testerQuadraticLoss(tester, node = NULL)
testerConfusion(tester, node = NULL)
testerKappa(tester, node = NULL, weights = c("None", "Linear", "Quadratic"), W = NULL)
testerLambda(tester, node = NULL, weights = c("None", "Linear", "Quadratic"), W = NULL)
\S3method{summary}{NetworkTester}(object, ...)
}
\arguments{
  \item{tester}{An object of class \code{\linkS4class{NetworkTester}}.}
  \item{node}{One of the target nodes for tester (an element of
    \code{testerTarget()}).  If supplied, only results for that node are
    returned.}
  \item{weights}{Loss weights for Kappa and Lambda.  See details.}
  \item{W}{A loss matrix for Kappa and Lambda.  If \code{weights} are
    specified, the loss matrix is defined according to the pattern.}
  \item{object}{An object of class \code{\linkS4class{NetworkTester}} to
    be summarized.}
  \item{...}{Other arguments to \code{summary}, ignored.}
}
\details{

  The \code{\link{testNetwork}} function takes as input generated data
  with values for both the \code{testTarget} nodes and the observed nodes
  (the nodes that are not in the \code{testerTarget} or
  \code{testerIgnore} lists.  It estimates the values for the generated
  nodes and compares the estimated values with the actual values from
  the data set.  The results are stashed in a
  \code{\linkS4class{NetworkTester}} object.  The functions described
  here extract information from the \code{NetworkTester} object.

  The function \code{testerErrorRate} gives the fraction of cases where
  the predicted value for the node was different from the actual value.
  If \code{node} is specified, the value for that node is returned,
  otherwise a vector of values for all nodes are returned.  

  The \emph{log loss} (also called the \emph{score}) for target node
  \eqn{X} in case \eqn{i} is defined as the \eqn{- log (P(X_i = c_i)) };
  it ranges from 0 to infinity with lower scores being better.  The
  function \code{testerLogLoss} gives the average log loss over all
  available test cases.  If \code{node} is specified, the value for that
  node is returned, otherwise a vector of values for all nodes are
  returned.

  The \emph{quadratic loss} (also called the \emph{Brier score}) for
  target node \eqn{X} in case \eqn{i} is defined as the \eqn{1 - 2 P(X_i
  = c_i)) + \sum_{k=1}^K (P(X_i=k))^2 }; it ranges from 0 to 2 with
  lower scores being better.  The function \code{testerQuadraticLoss}
  gives the average quadratic loss over all available test cases.  If
  \code{node} is specified, the value for that node is returned,
  otherwise a vector of values for all nodes are returned.

  The function \code{testerConfusion} returns the confusion matrixes
  associated with each node in the \code{testerTarget} node list.  This
  is a square matrix whose rows and columns correspond to the predicted
  and actual values of the node respectively.  The value in cell \eqn{(i, j)}
  of cases for which the predicted value for the node was in State
  \eqn{i} and the actual value was in State \eqn{j}.  There is one
  confusion matrix for each target node.  If the \code{node} argument is
  specified, then the confusion matrix for that node is returned,
  otherwise a named list of confusion matrixes is returned.

  Cohen's \eqn{\kappa} and Goodman and Kruskal's \eqn{\lambda} provide
  ways of summarizing a confusion matrix (Almond et al., 2015).  One
  obvious way to summarize the confusion matrix is to look at the
  proportion of results along the main diagonal.  This is the error rate
  described above.  However, if one category is very common in the
  population, the error rate could be very high because of the base rate
  and not because of the power of the classification system.  Kappa and
  lambda are two ways to normalize the error rate to avoid the base rate
  issue.  Kappa assumes that the two raters (the predicted and the
  actual) are both rating by chance at their marginal frequencies.  It
  adjusts the base rate for the proportion of time the two agree by
  chance.  Lambda compares the predicted results to a predictor who
  always predicts the most common category; thus is indicates how much
  better a differentiated approach is to one that treats all cases
  equally.  Both values are numbers between -1 and 1 (similar to
  correlations) with 1 indicating perfect agreement, -1 perfect
  disagreement and 0 the two measures are unrelated.

  For both kappa and lambda, the analyst can specify a weight matrix (1
  - loss matrix) \code{W} which indicates how desirable it is to
  classify a subject as \eqn{i}, when they really are a \eqn{j}.  This
  is usually scaled so that the main diagnoal is 1 and the smallest
  value if 0 (or at least all values are positive).  If the states are
  ordered, common choices of weight are linear weights (weights decrease
  linearly as they move from the diagonal) and quadratic weights
  (weights decrease quadratically).  These can be specified using the
  keywords for the \code{weights}.  The default keyword is \dQuote{None}
  which produces the identify matrix (the unweighted case).
  


}
\value{
  The functions \code{testerNet} returns the
  \code{\linkS4class{NeticaBN}} on which the test was run.

  The functions \code{testerTarget} and \code{testerIgnore} both return
  lists of \code{\linkS4class{NeticaNode}} objects.  In the first case
  the target nodes.

  The functions \code{testerErrorRate}, \code{testerLogLoss},
  \code{testerQuadraticLoss}, \code{testerKappa} and \code{testerLambda}
  give a vector of values for the respecrive function.  If \code{node}
  is specified, the value for that node is returned, otherwise a named
  vector of values for all nodes is returned.

  The function \code{testerConfusion} returns a single confusion matrix
  if \code{node} is specified or a named list of confusion matrixes.

  The \code{summary} method for \code{\linkS4class{NetworkTester}}
  objects returns a data frame where the rows represent the target nodes
  and the columns have the following values:
  \describe{
    \item{ErrorRate}{\code{testerErrorRate(object)}}
    \item{LogLoss}{\code{testerLogLoss(object)}}
    \item{QuadraticLoss}{\code{testerQuadraticLoss(object)}}
    \item{kappa}{\code{testerKappa(object)}}
    \item{QWK}{\code{testerKappa(object,weights="Quadratic")}}
    \item{lambda}{\code{testerLambda(object)}}
    \item{LinearLambda}{\code{testerLambda(object,weights="Linear")}}
  }

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
\seealso{

  The output of \code{\link{testNetwork}} is \code{\linkS4class{NetworkTester}}.

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
\keyword{ interface }
\keyword{ classif }
