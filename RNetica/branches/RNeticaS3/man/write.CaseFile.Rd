\name{write.CaseFile}
\alias{write.CaseFile}
\alias{read.CaseFile}
\title{Read or write data frame in Netica Case File format.}
\description{
  These functions our wrapper around \code{\link[utils]{read.table}} and
  \code{\link[utils]{write.table}} to format the file in the expected
  Netica case file format.
}
\usage{
write.CaseFile(x, file, ...)
read.CaseFile(file, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A data frame to be written to the file.  See details.}
  \item{file}{A file name or a connection object.  By convention, Netica
    expects case files to end in the \dQuote{.cas} suffix.}
  \item{\dots}{Other arguments to \code{read.table} or \code{write.table}}
}
\details{

  A Netica case file has a format that very much resembles the output of
  \code{\link[utils]{write.table}}.  The first row is a header row, which
  contains the names of the variables, the second and subsequent rows
  contain a set of findings:  an assignment of values to the nodes
  indicated in the columns.  There are no row numbers, and the separator
  and missing value codes are the values of
  \code{\link{CaseFileDelimiter}()}, and
  \code{\link{CaseFileMissingCode}()} respectively.

  In addition to columns representing variables, two special columns are
  allowed.  The column named \dQuote{IDnum}, if present should contain
  integers which correspond to ID numbers for the cases (this correspond
  to the \code{id} argument of \code{\link{WriteFindings}}).  The column
  named \dQuote{NumCases} should contain number values and this allows
  rows to be differentially weighted (this correspond to the \code{freq}
  argument of \code{\link{WriteFindings}}).  If these special arguments
  are present, \code{write.table} permutes the columns if necessary to
  make them first in the order (as Netica does in \code{WriteFindings}).

  The function \code{read.CaseFile} overrides following arguments of
  \code{read.table}:  \code{header = TRUE}, \code{sep =
  \link{CaseFileDelimiter}()}, and \code{na.strings =
  \link{CaseFileMissingCode}()}.  The function \code{write.CaseFile}
  overrides following arguments of \code{write.table}:  \code{col.name =
  TRUE}, \code{row.names = FALSE}, \code{quote = FALSE}, \code{sep =
  \link{CaseFileDelimiter}()}, and \code{na =
  \link{CaseFileMissingCode}()}.
  
}
\value{
  The function \code{read.CaseFile} returns a data frame containing the
  information in the case file.  The function \code{write.CaseFile}
  returns the output of the \code{\link[utils]{write.table}} call (which
  is undocumented).
}
\author{Russell Almond}
\seealso{
  \code{\link{CaseFileDelimiter}}, \code{\link{CaseFileMissingCode}},
  \code{\link{WriteFindings}}, \code{\link{ReadFindings}},
  \code{\link{MemoryCaseStream}},\code{\link{CaseFileStream}},
  \code{\link{MemoryStreamContents}},
  \code{\link[utils]{read.table}},\code{\link[utils]{write.table}}

}
\examples{

casefile <- paste(library(help="RNetica")$path,
                           "testData","abctestcases.cas",
                           sep=.Platform$file.sep)
CaseFileDelimiter("\t")
CaseFileMissingCode("*")
cases <- read.CaseFile(casefile)

outfile <- tempfile("testcase",fileext=".cas")
write.CaseFile(cases,outfile)

}
\keyword{ interface }
\keyword{ IO }
