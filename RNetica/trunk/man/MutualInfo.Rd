\name{MutualInfo}
\alias{MutualInfo}
\alias{VarianceOfReal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MutualInfo(target, nodelist)
MutualInfo(target, nodelist)
}
%- maybe also 'usage' for other objects documented here.
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
  \nref{MutualInfo_bn}, \nref{VarianceOfReal_bn}
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
function (target, nodelist) 
{
    if (length(target) > 1L || !is.NeticaNode(target) || !is.active(target)) {
        stop("Target is not an active Netica node", target)
    }
    if (!is.list(value) || length(nodelist) == 0) {
        stop("Value must be list of Netica nodes")
    }
    if (!all(sapply(value, is.NeticaNode))) {
        stop("Value must be list of Netica nodes, or NULLs")
    }
    result <- .Call("RN_GetMutalInfo", target, nodelist, PACKAGE = "RNetica")
    ecount <- ReportErrors()
    if (ecount[1L] > 0) {
        stop("MutualInfo: Netica Errors Encountered, see console for details.")
    }
    if (!is.null(names(nodelist))) {
        names(result) <- names(nodelist)
    }
    else {
        names(result) <- sapply(nodelist, NodeName)
    }
    result
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
