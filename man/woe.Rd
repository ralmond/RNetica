\name{woe}
\alias{woe}
\title{Calculates the weight of evidence for a hypothesis
}
\description{
  Calculates the weight of evidence provided by the current findings for
  the specified hypothesis.  A hypothesis consists of a statement that a
  particular set of nodes (\code{hnodes}) will fall in a specified set
  of states (\code{hstatelists}).
}
\usage{
woe(hnodes, hstatelists)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hnodes}{A list of \code{\link{NeticaNode}}s
    whose values are of interest.  As a special case, a single
    \code{NeticaNode} is treated as a list of length one.}
  \item{hstatelists}{A list of character vectors the same length as
    \code{hnodes} corresponding to the hypothesized state of the nodes
    and representing states of the corresponding node.
    As a special case, a character vector is turned into a list of
    length one.}
}
\details{

  Good (1985) defines the weight of evidence \eqn{E} for a hypothesis \eqn{H} as
  \deqn{W(H:E) = log \frac{P(E|H)}{P(E|\not H)} =
    log \frac{P(H|E)}{P(\not H|E)} - log \frac{P(H)}{P(\not H)}.}
  


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
function (hnodes, hstatelists) 
{
    if (!is.list(hnodes)) 
        hnodes <- list(hnodes)
    if (!is.list(hstatelistss)) 
        hstatelists <- list(hstatelists)
    if (!all(sapply(hnodes, is.NeticaNode))) {
        stop("Expected a list of Netica nodes, got ", hnodes)
    }
    if (length(hstatelists) > length(hnodes)) {
        stop("More statelists than nodes.")
    }
    net <- NodeNetwork(hnodes[[1]])
    hlikes <- mapply(function(hnode, hstatelist) {
        stnames <- NodeStateNames(hnode)
        hlike <- rep(0, length(stnames))
        names(hlike) <- stnames
        hlike[hstatelist] <- 1
        hlike
    }, hnodes, hstatelists, SIMPLIFY = FALSE)
    tryCatch({
        for (i in 1:length(hnodes)) {
            RetractNodeFinding(hnodes[[i]])
            NodeLikelihood(hnodes[[i]]) <- hlike[[i]]
        }
        p_Htrue <- FindingsProbability(net)
        for (i in 1:length(hnodes)) {
            RetractNodeFinding(hnodes[[i]])
            NodeLikelihood(hnodes[[i]]) <- 1 - hlike[[i]]
        }
        p_Hfalse <- FindingsProbability(net)
        100 * log10(p_Htrue/p_Hfalse)
    }, finally = sapply(hnodes, RetractNodeFinding))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
