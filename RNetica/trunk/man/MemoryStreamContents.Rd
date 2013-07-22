\name{MemoryStreamContents}
\alias{MemoryStreamContents}
\alias{MemoryStreamContents<-}
\title{Access the contents of a MemoryCaseStream}
\description{
  This function returns the contents of a
  \code{\link{MemoryCaseStream}}'s internal buffer as a
  \code{\link[base]{data.frame}}.  Alternatively, it sets the contents
  of the buffer to a given data frame.
}
\usage{
MemoryStreamContents(stream)
MemoryStreamContents(stream) <- value
}

\arguments{
  \item{stream}{A \code{\link{MemoryCaseStream}} object whose contents
  is to be access.
}
  \item{value}{Either a data frame giving the new value (see details),
  or else \code{NULL}.}
}
\details{

  A set of cases for a Netica network corresponds to a
  \code{data.frame}.  The columns represent nodes in the graph, and the
  values in that particular column correspond to findings for that node:
  a particular instantiation for that state, with a value of \code{NA}
  if the state of that node is unknown.  
  
  In addition to columns representing variables, two special columns are
  allowed.  The column named \dQuote{IDnum}, if present should contain
  integers which correspond to ID numbers for the cases (this correspond
  to the \code{id} argument of \code{\link{WriteFindings}}).  The column
  named \dQuote{NumCases} should contain number values and this allows
  rows to be differentially weighted (this correspond to the \code{freq}
  argument of \code{\link{WriteFindings}}).

  A \code{MemoryCaseStream} contains an R data frame object written out
  in string form.  This function converts between the internal string
  object and the data frame representation.  When called as
  \code{MemoryStreamContents(stream)} it reads the current value of the
  stream and converts it to a data frame.  When called as setter
  function, it converts the value into a string and focuses the
  \code{MemoryCaseStream} object on this string.

  Setting the contents to \code{NULL} creates a new empty stream buffer
  inside of the stream object.  This is useful for creating a blank
  buffer for writing cases.

  The code \code{MemoryCaseStream} object maintains a cached copy of the
  data frame associated with the memory stream.  Calling the function in
  either the setter or getter form updates that cache.  Calling this
  function when the stream is closed, will access the cached copy.  In
  the case of the setter form, this will updated the cached value, and
  if the stream is reopened, it will focus on the new cached value.
  Note that if the stream is closed before \code{MemoryStreamContents}
  is called, then the value returned will be the cached value created
  when \code{MemoryStreamContents} was last called, or when the stream
  is opened.

}
\value{
  A data frame which corresponds to the contents of the stream buffer,
  or \code{NULL} if the stream buffer is empty.
}
\references{
  \newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{SetStreamContents_ns},
}
\author{Russell Almond}
\note{
  The cached value of the stream can be accessed with the expression
  \code{attr(stream,"Case_Stream_DataFrame")}.  While it is almost
  certainly a mistake to set this value directly, there may be
  situations (e.g., avoiding duplicating the data frame when the stream
  is essentially open for reading only) where it is useful.  On the
  other hand, there may be situations where it is useful to read the
  cached value without forcing a reread of the memory buffer.
    
}
\seealso{
  \code{\link{CaseFileDelimiter}}, \code{\link{CaseFileMissingCode}},
  \code{\link{WriteFindings}}, \code{\link{ReadFindings}},
  \code{\link{MemoryCaseStream}}, \code{\link{NeticaCaseStream}}
}
\examples{

casefile <- paste(library(help="RNetica")$path,
                           "testData","abctestcases.cas",
                           sep=.Platform$file.sep)
CaseFileDelimiter("\t")
CaseFileMissingCode("*")
cases <- read.CaseFile(casefile)

memstream <- MemoryCaseStream(cases)

## Should be the same as cases
MemoryStreamContents(memstream)

MemoryStreamContents(memstream) <- cases

CloseCaseStream(memstream) ## Don't forget to read off the value
                           ## first if needed before closing.

stopifnot(!isCaseStreamOpen(memstream))
## This should return the cached value.
MemoryStreamContents(memstream)

## Will clear stream when next open
MemoryStreamContents(memstream) <- NULL

OpenCaseStream(memstream)
stopifnot(is.null(MemoryStreamContents(memstream)))

CloseCaseStream(memstream)

}
\keyword{ interface }
\keyword{ IO }
