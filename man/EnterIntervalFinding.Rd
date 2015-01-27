\name{EnterIntervalFinding}
\alias{EnterIntervalFinding}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
EnterIntervalFinding(node, low, high, retractFirst = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{node}{
    An active \code{\link{NeticaNode}} object that references the node.
}
  \item{low}{
%%     ~~Describe \code{low} here~~
}
  \item{high}{
%%     ~~Describe \code{high} here~~
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
  \nref{EnterIntervalFinding_bn}
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
function (node, low, high, retractFirst = TRUE) 
{
    if (length(node) > 1 || !is.NeticaNode(node) || !is.active(node)) {
        stop("Node is not an active Netica node", node)
    }
    if (length(low) > 1 || length(high) > 1) {
        stop("Low and high must be scalars.")
    }
    hv <- high
    lv <- low
    if (is.discrete(node)) {
        if (is.character(hv)) {
            hv <- match(hv, NodeStates(node))
        }
        hv <- as.integer(hv) - 1L
        if (is.na(hv)) {
            stop("High value ", high, " not legal for node ", 
                node)
        }
        if (is.character(lv)) {
            lv <- match(lv, NodeStates(node))
        }
        lv <- as.integer(lv) - 1L
        if (is.na(lv)) {
            stop("Low value ", low, " not legal for node ", node)
        }
    }
    else {
        if (is.character(low) || is.na(low)) {
            stop("Low value ", low, " is not numeric.")
        }
        if (is.character(high) || is.na(high)) {
            stop("High value ", high, " is not numeric.")
        }
        if (high <= low) {
            stop("High value must be greater than low value.")
        }
    }
    resetFirst <- as.logical(retractFirst)
    handle <- .Call("RN_SetNodeGaussian", node, lv, hv, resetFirst, 
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
