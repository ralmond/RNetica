\name{NodeExpectedUtils}
\alias{NodeExpectedUtils}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
NodeExpectedUtils(node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{node}{
    An active \code{\link{NeticaNode}} object that references the node.
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
  \nref{GetNodeExpectedUtils_bn}
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
function (node) 
{
    if (length(node) > 1 || !is.NeticaNode(node) || !is.active(node)) {
        stop("Node is not an active Netica node", node)
    }
    result <- .Call("RN_GetNodeExpectedValue", node, PACKAGE = "RNetica")
    ecount <- ReportErrors()
    if (ecount[1] > 0) {
        stop("NodeExpectedUtils: Netica Errors Encountered, see console for details.")
    }
    names(result) <- NodeStates(node)
    result
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
