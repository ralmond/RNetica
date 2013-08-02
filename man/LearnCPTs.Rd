\name{LearnCPTs}
\alias{LearnCPTs}
\title{Learn Conditional Probability Tables with Missing Data.}
\description{
  This function updates the conditional probabilities associated with
  the given list of nodes based on the findings associated with that
  node and its parents found in the \code{caseStream} argument, which
  should be a \code{\link{NeticaCaseStream}} object.  Unlike
  \code{\link{LearnCases}}, these algorithms can support cases with
  missing or latent variables.
}
\usage{
LearnCPTs(caseStream, nodelist, method = "COUNTING", maxIters = 1000L, maxTol = 1e-06, weight = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{caseStream}{
%%     ~~Describe \code{caseStream} here~~
}
  \item{nodelist}{
%%     ~~Describe \code{nodelist} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{maxIters}{
%%     ~~Describe \code{maxIters} here~~
}
  \item{maxTol}{
%%     ~~Describe \code{maxTol} here~~
}
  \item{weight}{
%%     ~~Describe \code{weight} here~~
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
function (caseStream, nodelist, method = "COUNTING", maxIters = 1000L, 
    maxTol = 1e-06, weight = 1) 
{
    if (is.NeticaNode(nodelist) && length(nodelist) == 1L) {
        nodelist <- list(nodelist)
    }
    if (any(!sapply(nodelist, function(nd) {
        is.NeticaNode(nd) && is.active(nd)
    }))) {
        stop("Expected a list of Netica nodes, got, ", nodelist)
    }
    weight <- as.numeric(weight)
    if (length(weight) > 1) {
        warning("LearnCPTs:  Only the first value of weight will be used.")
    }
    maxTol <- as.numeric(maxTol)
    if (length(maxTol) > 1) {
        warning("LearnCPTs:  Only the first value of maxTol will be used.")
    }
    maxIters <- as.integer(maxIters)
    if (length(maxTols) > 1) {
        warning("LearnCPTs:  Only the first value of maxIters will be used.")
    }
    if (!is.character(method) || length(method) > 1L) {
        stop("Method must be one of 'GRADIENT', 'EM', or 'COUNTING'")
    }
    method <- toupper(method)
    if (!(method = "GRADIENT" || method == "EM" || method == 
        "COUNTING")) {
        stop("Method must be one of 'GRADIENT', 'EM', or 'COUNTING'")
    }
    label <- deparse(substitute(caseStream))
    if (is.character(caseStream)) {
        stream <- CaseFileStream(caseStream)
    }
    else if (is.data.frame(caseStream)) {
        stream <- MemoryCaseStream(caseStream, label)
    }
    else if (is.NeticaCaseStream(caseStream)) {
        stream <- caseStream
    }
    else {
        stop("Expected a Case stream or a filename or a data frame to make a case stream.")
    }
    WithOpenCaseStream(stream, {
        result <- .Call("RN_LearnCPTs", stream, nodelist, method, 
            maxIters, maxTol, weight, PACKAGE = "RNetica")
        ecount <- ReportErrors()
        if (ecount[1] > 0) {
            stop("SetNodeName: Netica Errors Encountered, see console for details.")
        }
        result
    })
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
