\name{CaseFileDelimiter}
\alias{CaseFileDelimiter}
\alias{CaseFileMissingCode}
\title{Gets or sets special characters for case files.
}
\description{
  The function \code{CaseFileDelimiter} sets the field delimiter used
  when writing case files.  The function \code{CaseFileMissingCode} sets
  the character code used for missing values in case files.  If called
  with a null argument, then the current value is returned.
}
\usage{
CaseFileDelimiter(newdelimiter = NULL)
CaseFileMissingCode(newcode = NULL)
}
\arguments{
  \item{newdelimiter}{A character scalar containing the new delimiter.  It must
    be either a comma, a space, or a tab.
  }
  \item{newcode}{The character to be used as a delimiter.  It must
    be either an asterisk ("*"), a question mark ("?"), a space, (" ")
    or the empty string ("").
  }
}
\details{
  Case files are essentially a comma separated value files, although tab
  and space are allowed as alternative delimiters.  The space and empty
  string are only allowed as missing value codes when the delimiter is a
  comma.

  The value of the delimiter is global, and applies to all case files
  written from this point on.

  When the argument is null (the default) the current value is returned
  without changing it.
}
\value{
  The value of the delimiter or missing code before the function call as
  a string.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
    \nref{SetCaseFileDelimChar_ns}, \nref{SetMissingDataChar_ns}
    
}
\author{Russell G. Almond}
\note{
  The default R missing code "NA" does not work with Netica.
}
\seealso{
  \code{\link{WriteFindings}}, \code{\link{WriteFindings}},
  \code{\link{read.CaseFile}}
}
\examples{

defaultDelim <- CaseFileDelimiter() # Get default
d1 <- CaseFileDelimiter("\t")
d2 <- CaseFileDelimiter(" ")
d3 <- CaseFileDelimiter(",")

defaultMiss <- CaseFileMissingCode() # Get default
m1 <- CaseFileMissingCode("*")
m2 <- CaseFileMissingCode("?")
m3 <- CaseFileMissingCode(" ")
m4 <- CaseFileMissingCode("")
\dontrun{
 ## This should throw an error.
 CaseFileDelimiter(" ")
}

m5 <- CaseFileMissingCode("?")

d4<- CaseFileDelimiter(" ")
\dontrun{
  ## This should throw an error
  CaseFileMissingCode(" ")
}
## But this is okay
CaseFileMissingCode("*")

stopifnot(d1==defaultDelim, d2=="\t", d3==" ", d4==",")
stopifnot(m1==defaultMiss, m2=="*", m3=="?", m4==" ", m5=="")

## restore defaults
CaseFileDelimiter(defaultDelim)
CaseFileMissingCode(defaultMiss)
  
}
\keyword{ interface }
\keyword{ IO }

