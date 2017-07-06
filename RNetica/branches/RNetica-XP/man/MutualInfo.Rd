\name{MutualInfo}
\alias{MutualInfo}
\alias{VarianceOfReal}
\title{Calculates strength of relationship between two nodes in a network
}
\description{
  The mutual information is a measure of how closely related one node is
  to another, i.e., how much information each node in \code{nodelist}
  provides about the \code{target} node.  The expression
  \code{MutualInfo(target, nodelist)} calculates the mutual information of
  each node in \code{nodelist} with \code{target}.

  The function \code{VarianceOfReal()} is similar, but instead it
  measures the reduction in variance of the target.  The \code{target}
  node must be continuous or have numeric values assigned to all levels
  using \code{\link{NodeLevels}}.

}
\usage{
MutualInfo(target, nodelist)
VarianceOfReal(target, nodelist)
}
\arguments{
  \item{target}{
    An active \code{\link{NeticaNode}} object that is the target of
    inference (i.e., we want to find the influence of other nodes on
    this node). 
}
  \item{nodelist}{
    A non-empty list of active \code{\link{NeticaNode}} objects whose
    effect on target is desired.
}
}
\details{
  The mutual information between two discrete variables is defined as:
  \deqn{MI(X,Y) = \sum_{x,y} \Pr(x,y) \log \frac{ \Pr(x,y) }{\Pr(x)\Pr(y)}.}
  It is a measure of how much information \code{X} provides about
  \code{Y}.  This measure is appropriate when both \code{X} and \code{Y}
  are discrete variables.

  Mutual information is often used to select the next best variable to
  test (in the educational context, this would be the next item to
  select for an adaptive test).  The highest value of the mutual
  information will provide the most information.  (See Chapter 7 of
  Almond et al, 2015).

  The function \code{VarianceOfReal(target,nodelist)} is related, but in
  this case \code{target} must either be continuous
  (\code{\link{is.continuous}(target)} is true) or have numeric values
  assigned to each level using \code{\link{NodeLevels}(target)}.  For
  this function, the value returned is the amount by which the variance
  of \code{target} is expected to be reduced if the value of the
  observable node in the nodelist was learned.  Again, higher values
  indicate better information.
  
}
\value{
  Returns a named numeric vector with the names corresponding to the
  nodes in \code{nodelist} and the value the mutual information or
  variance reduction.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{MutualInfo_bn}, \nref{VarianceOfReal_bn}

  Almond, R. G., Mislevy, R. J., Steinberg, L. S., Yan, D. & Williamson,
  D. M. (2015) \emph{Bayesian Networks in Educational Assessment}.
  Springer.   

}
\author{
  Russell Almond
}
\seealso{
  \code{\link{is.continuous}()}, \code{\link{NodeExpectedValue}()},
  \code{\link{NodeLevels}()},
}
\examples{

irt5 <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","IRT5.dne",
                           sep=.Platform$file.sep))

irt5.theta <- NetworkFindNode(irt5,"Theta")
irt5.x <- NetworkFindNode(irt5,paste("Item",1:5,sep="_"))

CompileNetwork(irt5)

MutualInfo(irt5.theta,irt5.x)

VarianceOfReal(irt5.theta,irt5.x)

DeleteNetwork(irt5)

}
\keyword{ interface }
\keyword{ manip }% __ONLY ONE__ keyword per line
