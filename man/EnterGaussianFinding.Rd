\name{EnterGaussianFinding}
\alias{EnterGaussianFinding}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
EnterGaussianFinding(node, mean, sd, retractFirst = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{node}{
    An active \code{\link{NeticaNode}} object that references the node.
    Node should be continuous, or have numeric value ranges assigned to
    it.
  }
  \item{mean}{
%%     ~~Describe \code{mean} here~~
}
  \item{sd}{
%%     ~~Describe \code{sd} here~~
}
  \item{retractFirst}{
%%     ~~Describe \code{retractFirst} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{EnterGaussianFinding_bn}, 
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (node, mean, sd, retractFirst = TRUE) 
{
    if (length(node) > 1 || !is.NeticaNode(node) || !is.active(node)) {
        stop("Node is not an active Netica node", node)
    }
    if (length(mean) > 1 || length(sd) > 1) {
        stop("Mean and sd must be scalars.")
    }
    if (is.character(mean) || is.na(mean)) {
        stop("Mean value ", mean, " is not numeric.")
    }
    if (is.character(sd) || is.na(sd)) {
        stop("SD value ", sd, " is not numeric.")
    }
    resetFirst <- as.logical(retractFirst)
    handle <- .Call("RN_SetNodeGaussian", node, val, sd, resetFirst, 
        PACKAGE = "RNetica")
    ecount <- ReportErrors()
    if (ecount[1] > 0) {
        stop("NodeFinding: Netica Errors Encountered, see console for details.")
    }
    invisible(handle)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
