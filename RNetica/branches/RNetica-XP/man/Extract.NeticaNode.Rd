\name{Extract.NeticaNode}
\alias{Extract.NeticaNode}
\alias{[.NeticaNode}
\alias{[[.NeticaNode}
\alias{[<-.NeticaNode}
\alias{EVERY_STATE}
\title{
  Extracts portions of the conditional probability table of a Netica node.
}
\description{
  Provides an efficient mechanism for extracting or setting portions of
  large conditional probability tables.  In particular, allows setting
  many rows a CPT to the same value.
}
\usage{
\method{[}{NeticaNode}(x, \ldots, drop=FALSE) 
\method{[[}{NeticaNode}(x, \ldots)  
\method{[}{NeticaNode}(x, \ldots) <- value 
EVERY_STATE
%]]]]  To make parens come out right.
}
\arguments{
  \item{x}{
    An active, discrete \code{\link{NeticaNode}} whose conditional
    probability table is to be accessed.
  }
  \item{\ldots}{
    Indices specifying rows of the table to extract or
    replace.  If a single index, \code{i}, is given, it should be a data
    frame selecting the parent states, or an integer pointing at a
    configuration.  If multiple indexes are given, the number of indexes
    should correspond to the number of parent states of the variable.  The
    values should either be character strings (corresponding to parent
    variable states), or numeric (indexes to parent states).  In character
    strings, the special value \code{"*"} is allowed
    to select all values of that variable. In numeric indexes, the special
    value \code{EVERY_STATE} indicates that all states are selected.
    Leaving the index position blank is the same as specifying \code{"*"}
    or \code{EVERY_STATE}.
  }
  \item{drop}{
    If true and a single row is selected, that row will be returned as a
    numeric vector instead of a conditional probability frame
    (\code{CPF}).  
  }
  \item{value}{
    Either a numeric vector with length \code{\link{NodeNumStates}(x)}
    giving the conditional probabilities for the specified rows in the
    table or a character scalar (discrete node) or numeric scalar (continuous
    node) giving the value that should be given probability 1.
  }
}
\details{
  The function \code{\link{NodeProbs}(node)} allows one to access the
  entire conditional probability at once as a conditional probability
  array (\code{\link{CPA}}).  Although the built-in R array replacement
  mechanisms allow one to make various kinds of edits, it is relatively
  inefficient.  In particular, to set a single row of an array, the
  entire table is read into R and then written back to Netica.

  This function allows the syntax \code{node[\ldots]} to be used to
  access only a portion of the table.  There are many different ways
  \code{...} can be interpreted, which are described below.

  In this access model the value \code{EVERY_STATE} or the character
  value \code{"*"} has a special meaning of match every level of that
  state variable.  Netica supports this as a shortcut method for
  specifying conditional probability tables with many similar
  values. However, when reading the conditional probability tables from
  Netica they are expanded and no attempt is made to collapse over
  identical rows.

  A second difference is that \code{node[\ldots]} returns the
  conditional probability table in data frame (\code{\link{CPF}})
  format.  This is particularly convenient because that format does not
  need to cover every parent configuration, thus it is ideal for holding
  subset of the complete table.

  A third difference is that a number of special values are allowed for
  the probability table.  First, if the node is deterministic, the value
  of a parent configuration can be set to the state name instead of a
  probability vector.  This creates a deterministic conditional
  probability table full of 1's and 0's.  For continuous nodes, the
  nodes value for a parent configuration (assuming all discrete or
  discretized parents) can be set directly.  Finally, if the last column
  of the conditional probabilities is not supplied, it will be
  computed.  This is particularly handy for binary nodes.

  Normally, the expression \code{node[\ldots]} produces a data frame
  either in \code{\link{CPF}} format, or with the probabilities replaced
  by a single column of values.  If \code{drop==TRUE} or equivalently if
  \code{node[[\ldots]]} was the expression, only the matrix of
  probabilities or the vector of values will be returned.  The
  expression \code{node[[\ldots]] <- value} is not supported.
  
  The sections below describe the various indexing options.

}
\section{Selecting Rows Using Data Frames}{
  This selection uses the syntax \code{node[df]} or
  \code{node[df]<-value}, where \code{df} is a data frame or a matrix. 
  It is assumed that the columns represent the variables, and the rows
  represent the selected configurations of the parent variables.

  In this configuration, the number of rows of \code{df} and
  \code{value} should match (or the length of \code{value} should equal
  the number of rows if one of the special values is used).  When the
  value is being queried rather than set, the number of rows in the
  result may be greater than the number of rows in \code{df} because of
  \code{EVERY_STATE} expansion.

  There are three different ways that \code{df} could be represented:
  \enumerate{
    \item It can be a data frame filled with factor variables whose
    levels correspond to the states of the corresponding parent node.
    \item It can be a matrix or data frame of type character whose
    values correspond to the state names of the corresponding parent
    variables, or possibly the special value \code{"*"} meaning that all
    values of that parent should be matched.
    \item It can be a matrix of data frame of integers whose values
    correspond to the state indexes of the parent variables.  In this case
    the special value \code{EVERY_STATE} can be supplied indicating that
    all values should be matched.  Otherwise, it should be a number
    between 1 and the number of states of that variable, inclusive.
  }

  The number of columns in \code{df} should be the same as the number of
  parent variables for \code{node}.  If \code{df} has column names, then
  all columns should be named.  In this case the parent variables will
  be match by the \code{\link{NodeInputNames}(node)} if they exist, or
  the names of the parent variables if they do not (see
  \code{\link{ParentStates}(node)} for more details).  Otherwise,
  positional selection is used.

}
\section{Selecting Rows Using Array-type Selection}{

  The second way that rows from the conditional probability table can be
  selected is using an analogue of the selection mechanisms supported by
  R for selecting cells from an array.  Essentially, the rows of the
  conditional probability table are treated as if they are the elements
  of an array whose dimnames correspond to
  \code{\link{ParentStates}{node}}.  In particular the number of
  dimensions corresponds to the number of parent variables, and the
  extent of each dimension corresponds to the number of states of the
  corresponding parent variable.

  In this selection mode, the length of \code{...} should correspond to
  the number of parent variables (that is, there should be one fewer
  comma, than parent variables).  Each element can be one of three
  things:
  \enumerate{
    \item A character or factor vector selecting the appropriate states
    of the parent variable.
    \item An integer vector selecting the appropriate states of the
    parent variable by position.
    \item One of the special values \code{EVERY_STATE}, \code{"*"} or
    blank indicating that all values of the appropriate variable should be
    selected.
  }
  The order of the entries should be the same as the order of the parent
  variables in \code{\link{NodeParents}{node}}.  The selection looks
  very similar to selection using a data frame, where the data frame
  consists of applying  \code{\link[base]{expand.grid}(\ldots)}.

  Once again \code{EVERY_STATE} or \code{"*"} entries are treated
  specially inside of Netica, which allows every matching row of the
  table to be simultaneously set to the same probabilities.
  
  Note that negative selections and logical selections are not currently
  supported. 

}
\section{Selecting Rows Using Named Parents}{

  As with R array index selection, the dimensions of the selection in
  the \code{\ldots} argument can be specified using named arguments.  If
  one of the elements of \code{\ldots} is named, they all should be
  named.  The names should correspond to
  \code{\link{ParentNames}(node)}, that is the
  \code{\link{NodeInputNames}(node)} are used if available, and the
  names of the parent nodes are used as a fallback.

  As before the value for a parent variable can be set to a value or a
  vector of possible values as either an integer, factor or character
  value.  The special values \code{EVERY_STATE} and \code{"*"} are
  interpreted as before.  If the value of a parent variable is
  unspecified, this is equivalent to using the value
  \code{EVERY_STATE}. 
  
}
\section{Selecting Rows Using a Single Integer}{

  If \code{...} is a single integer, it is treated as an index into the
  possible configurations.  These are defined by
  \code{\link[base]{expand.grid}(\link{ParentStates}(node)}.  Each index
  refers to a row in that table.  This is particularly meant for running
  through loops on all values, although working with value as a data
  frame or using \code{NodeProbs} may be faster in those cases.

  There is some ambiguity when there is a single parent variable about
  whether the array-type selection or the index was intended, but both
  are identical, so there should be no conflict.
  
}
\section{Special Meaning for \code{NULL} selection}{

  If \code{\ldots} is \code{NULL}, that is if the calling expression looks
  like \code{node[]} then the intention is that all rows of the
  conditional probability table are to be selected.  This is the only
  meaningful selection type if there are no parent variables.  It also
  provides a fast and convenient way to set all rows of the conditional
  probability table to the same value (if \code{value}) has a single
  row, or to retrieve the complete conditional probability table in
  \code{\link{CPF}} format.

  If \code{value} is a data frame with both factor and numeric
  variables, then it takes on a different meaning.  In this case, the
  factor variables are used as if they were the selection argument (the
  \code{\ldots}) and the remaining numeric values the probabilities.
  
}
\section{Setting Value to a Probability Matrix}{

  In general the replacement value should be a matrix.  The number of
  columns should match the number of states of \code{node} (see below
  for the behavior if the number of columns is one less than the number
  of states).  It should have the same number of rows as the number of
  rows in the selection after any expansion has been applied for vector
  valued arguments, but not counting the special values
  \code{EVERY_STATE} or \code{"*"} (or blank entries in the list).

  Netica has a special shortcut for \code{EVERY_STATE} and all matching
  rows are set to the same probability value. This means that the number
  of rows in the value must match the selection counting the special
  values as if they selected a single row.  In particular, if
  \code{node} has one or more parent variables and \code{value} is a
  matrix with more than one row, \code{node[] <- value} will generate a
  error, because the selection has only one row (with every value set to
  \code{EVERY_STATE}).

  When \code{value} is an undimensioned vector, the function will do its
  best to figure out if it should be treated as a row or a column
  vector.  In the case of unusual behavior, expressing \code{value} as a
  matrix should make the programmer's intention clear.

}
\section{Setting Deterministic Values}{

  When a node is deterministic, that is all probabilities are \eqn{0} or
  \eqn{1}, then it is meaningful to talk about the conditional value of
  a node instead of the conditional probability table.  The expression
  \code{node[\ldots]} displays the conditional probability table in a
  special way when the node is deterministic.  In this case it displays
  the value as a single variable giving the state of the child variable
  given the configuration of the parents.  In the case of discrete
  nodes, this is a factor variable giving the state.  In the case of
  continuous nodes, this is a numeric vector giving the value.

  The same conventions can be used in setting the conditional
  probability of a node.  In the expression \code{node[\ldots] <- value}
  if \code{value} is a factor or character vector then the selected
  configurations are set to deterministic probabilities with the
  indicated value given probability of \eqn{1} and all others with
  probability \eqn{0}.  It is possible to set some rows of a conditional
  probability table to be deterministic and others to have unrestricted
  probabilities, however, the deterministic rows will then print out as
  unconstrained probabilities with \eqn{0} and \eqn{1} values.

  Continuous nodes (nodes for which \code{\link{is.continuous}(node) ==
  TRUE}) use a variation of this system.  Here the value is an arbitrary
  numeric value.  For this to be meaningful, it is assumed that all of
  the parents of \code{node} are either discrete or have been
  discretized. 

  Warning:  Setting an unconditional discrete node to a constant value,
  that is executing an expression like \code{node[] <- value} 
  is almost certainly a mistake.  Probably what is intended by that
  expression is \code{\link{NodeFinding}(node) <- value}.  In
  particular, if the former expression is used and the later someone
  attempts to set \code{\link{NodeFinding}(node) <- value1}, where
  \code{ value1 != value}, this will produce a contradiction
  (probability zero event) and all kinds of error will follow.
  
}
\section{Automatic normalization}{

  If the number of columns in \code{value} is one less than the number of
  states in \code{node}, then is assumed that the probability values
  should be calculated for the last state via normalization, that is it
  is assigned all of the remaining probability not assigned in the first
  couple of columns.  In particular, the value is internally translated
  via the expression:
  \code{value <- cbind(value,1-apply(value,1,sum))}.

  This is particularly useful when the node is binary (has exactly 2
  states).  Then the replacement only needs to specify the probability
  for the first one.  For example \code{node[] <- .5} would set the
  probability distribution of \code{node} to the uniform distribution if
  \code{node} is binary.

  There is some potential for confusion if \code{value} is not specified
  as a matrix.  In particular, if the number of states of the child
  value is one more than the number of configurations of the parents, it
  is unclear whether this is an attempt to set the node value of a
  discrete node or an unnormalized probability.  It should be possible
  by specifying value as a matrix or one row or one column to clarify
  the intent.

}
\value{
  For the form \code{node[\ldots]} the return value is a data frame in
  the \code{\link{CPF}} format giving the conditional probability
  table.  If the node is deterministic
  (\code{IsNodeDetermistic(node)==TRUE}), then the probabilities will be
  replaced with a single column giving the value of the node.  If the
  node is discrete, then the value will be a factor.  If the node is
  continuous, then the value will be a real vector.

  If \code{drop==TRUE} or an expression of the form
  \code{node[[\ldots]]} was called, then the return value will be a
  matrix of probabilities (the last several columns of the data frame).
  If the node is deterministic, then the result will instead be either a
  factor (discrete node) or real vector (continuous node) giving the
  value of the node for each parent configuration.

  The form \code{node[\ldots]<-value} returns \code{node} invisibly.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GetNodeProbs_bn}, \nref{SetNodeProbs_bn},
  \nref{GetNodeFuncState_bn}, \nref{SetNodeFuncState_bn},
  \nref{GetNodeFuncReal_bn}, \nref{SetNodeFuncReal_bn},
  

}
\author{
  Russell Almond
}
\note{

  I have tried to anticipate most of the ways that somebody might want
  to index the conditional probability table, not to mention all of the
  peculiar ways that R overloads the extraction operator.  Negative
  selections are not allowed.  I have almost certainly missed some
  combinations, and some untested combinations might preform rather
  strangely.  Undoubtedly somebody will come to rely on that strangeness
  and it will never get fixed.

  Factor variables do not easily handle the use of \code{"*"} as a
  wildcard.  To make this work, a construction like
  \code{factor(varstates, c(1:3,EVERY_STATE),
    labels=c("a1","a2","a3","*"))}.

  Internally R uses 1-based indexing and Netica uses 0-based indexing.
  RNetica makes the translation inside of the C layer, so these
  function should be called with R-style 1-based indexing.

  This documentation file is longer than \emph{War and Peace}.
}
\seealso{
  \code{\link{NeticaNode}}, \code{\link{NodeParents}()},
  \code{\link{NodeInputNames}()}, \code{\link{NodeStates}()},
  \code{\link{ParentStates}()}, \code{\link{CPF}}, \code{\link{CPA}} 
}
\examples{

## Setup
xnet <- CreateNetwork("X")

A <- NewDiscreteNode(xnet,"A",c("A1","A2","A3","A4"))
Aalt <- NewDiscreteNode(xnet,"Aalt",c("A1","A2","A3","A4"))
B <- NewDiscreteNode(xnet,"B",c("B1","B2","B3"))
B2 <- NewDiscreteNode(xnet,"B2",c("B1","B2"))
Balt <- NewDiscreteNode(xnet,"Balt",c("B1","B2","B3"))
C2 <- NewDiscreteNode(xnet,"C2",c("C1","C2"))
C3 <- NewDiscreteNode(xnet,"C3",c("C1","C2","C3"))
C4 <- NewDiscreteNode(xnet,"C4",c("C1","C2","C3","C4"))
Cont <- NewContinuousNode(xnet,"Cont")
CC <- NewContinuousNode(xnet,"CC")
CCC <- NewContinuousNode(xnet,"CCC")

### Tests for various setting modes.

## Null before we set any probabilities anything
stopifnot(
  all(is.na(C2[])), length(C2[]) == 2,
  all(is.na(Cont[])), length(Cont[])==1
)

NodeProbs(C2) <- c(1,0)
stopifnot(
  C2[]=="C1"
)
## This is just a demonstration of the syntax, in practice
## the expression NodeFinding(C2) <- "C2" is usually better.
C2[] <- "C2"
stopifnot(
  NodeProbs(C2)==c(0,1)
)
C3[] <- 3
stopifnot(
  C3[] == "C3"
)

## Setting value of continuous node
Cont[] <- 145.4
stopifnot( abs(Cont[] - 145.4) < .0001)

## Setting value with probabilities
C2[] <- c(.3,.7)
stopifnot( sum(abs(NodeProbs(C2)-c(.3,.7))) < .0001)
C3[] <- c(1,2,1)/4
stopifnot( sum(abs(NodeProbs(C3)-c(.25,.5,.25))) < .0001)

## Automatic normalization
C2[] <- .25
stopifnot( abs(sum(NodeProbs(C2)-c(.25,.75))) < .0001)
C3[] <- c(1,1)/3
stopifnot( abs(sum(NodeProbs(C3)-1/3)) < .0001)


### Now some one parent cases
AddLink(A,B)
AddLink(A,B2)

stopifnot(
  nrow(B[])==NodeNumStates(A),
  ncol(B[])==1+NodeNumStates(B),
  nrow(B[[]])==NodeNumStates(A),
  ncol(B[[]])==NodeNumStates(B),
  all(is.na(B[][,2:(1+NodeNumStates(B))])),
  all(is.na(B[[]]))
)

NodeProbs(B) <- normalize(matrix(1:12,4))
Brow1 <- B[1]
stopifnot(
  nrow(Brow1)==1,ncol(Brow1)==4,
  sum(abs(Brow1[,2:4]-c(1,5,9)/15))<.00001
)
Brow12 <- B[1:2]
stopifnot(
  nrow(Brow12)==2,ncol(Brow12)==4,
  sum(abs(Brow12[2,2:4]-c(2,6,10)/18))<.00001
)

Brow4 <- B["A4"]
stopifnot(
  nrow(Brow4)==1,ncol(Brow4)==4,
  sum(abs(Brow4[,2:4]-c(1,2,3)/6))<.00001
)
Brow34 <- B[c("A3","A4")]
stopifnot(
  nrow(Brow34)==2,ncol(Brow34)==4,
  abs(sum(Brow4[1,2:4]-c(3,7,11)/21))<.00001
)
Ball <- B["*"]
stopifnot(
  nrow(Ball)==4,ncol(Ball)==4
)
Ball <- B[EVERY_STATE]
stopifnot(
  nrow(Ball)==4,ncol(Ball)==4
)

Brow24 <- B[data.frame(A=factor(c("A2","A4"),NodeStates(A)))]
stopifnot(
  nrow(Brow24)==2,ncol(Brow24)==4,
  sum(abs(Brow24[2,2:4]-c(1,2,3)/6))<.00001
)

## Set all rows to the same value.
B[] <- matrix(c(1,1,1)/3,1)
stopifnot(
  abs(NodeProbs(B)-1/3)<.0001
)
B[EVERY_STATE] <- matrix(c(1,2,1)/4,1)
stopifnot(
  abs(NodeProbs(B)[3,]-c(.25,.5,.25))<.0001
)
B["*"] <- matrix(c(1,2,3)/6,1)
stopifnot(
  abs(NodeProbs(B)[2,]-c(1/6,1/3,.5))<.0001
)


## Setting to exact values
B2[1:2] <- "B1"
B2[3] <- "B2"
B2[4] <- "B2"
B2tab <- B2[]
stopifnot(
  IsNodeDeterministic(B2),
  nrow(B2tab)==4,ncol(B2tab)==2,
  length(B2[[]]) == 4,
  B2[[]] == c("B1","B1","B2","B2"),
  as.integer(B2tab[,2]) == c(1,1,2,2)
)
## Setting one value to non-deterministic changes the way the table is
## displayed.
B2[2] <- c(.5,.5)
B2tab <- B2[]
stopifnot(
  !IsNodeDeterministic(B2),
  nrow(B2tab)==4,ncol(B2tab)==3,
  sum(abs(B2tab[2,2:3]- c(.5,.5))) < .001,
  B2tab[1,2:3] == c(1,0),
  B2[[3]] == c(0,1)
)

## Self-normalizing setting
\dontrun{
## This will generate an error because it is trying to set all four
## configurations to the same value but it is given four values.
B2[] <- c(.1,.2,.3,.4)
}

B2[1:4] <- c(.1,.2,.3,.4)
stopifnot(
  sum(abs(NodeProbs(B2)[,2]-c(.9,.8,.7,.6))) < .001
)
B2[1:2] <- .5  ## Set both values to the same thing
B2[3:4] <- c(.6,.7) ## set to normalizing probs
stopifnot(
  sum(abs(NodeProbs(B2)[,2]-c(.5,.5,.4,.3))) < .001
)
## Beware!  This next form assumes you are setting the rows to the same
## thing.
B2[3:4] <- c(.2,.8) ## Ambiguous instructions
stopifnot(
  sum(abs(NodeProbs(B2)[,2]-c(.5,.5,.8,.8))) < .001
)
## Using a matrix makes intent clear
B2[3:4] <- matrix(c(.2,.8),2) ## set to normalizing probs
stopifnot(
  sum(abs(NodeProbs(B2)[,2]-c(.5,.5,.8,.2))) < .001
)

## Data frame as value
## First do a blank extraction to get general shape.
B2frame <- B2[]
## Now manipulate it however
B2frame[,2:3] <- 1:8
## And set it back
B2[] <- normalize(B2frame)
stopifnot(
  sum(abs(NodeProbs(B2)[,1]-c(1/6,2/8,3/10,4/12))) <.001
)

B2frame1 <-B2frame[B2frame$A=="A3",]
B2frame1[,2:3] <- c(4,6)/10
B2[] <- B2frame1  ## Only row 3 affected
stopifnot(
  sum(abs(NodeProbs(B2)[,1]-c(1/6,2/8,4/10,4/12))) <.001
)

## Continuous node with one discrete parent
AddLink(A,Cont)  ##Notice how old value is replicated
stopifnot(
  nrow(Cont[]) ==4, ncol(Cont[]) == 2,
  length(Cont[[]]) == 4,
  abs(Cont[][,2]-145.4) <.0001,
  abs(Cont[[3]]-145.4) <.0001
)
AddLink(A,CC)
stopifnot(
  nrow(CC[]) ==4, ncol(CC[]) == 2,
  is.na(CC[][,2])
)

Cont[] <- 7
stopifnot(
  abs(Cont[[]]-7) <.0001
)
Cont[2] <- 3.2
stopifnot(
  abs(Cont[[]]-c(7,3.2,7,7)) <.0001
)

Cont[1:2] <- 0
Cont[3:4] <- c(8,1)
stopifnot(
  abs(Cont[[]]-c(0,0,8,1)) <.0001,
  abs(Cont[3:4,drop=TRUE]-c(8,1)) < .0001
)


## Two parent case
AddLink(A,C2)
AddLink(B,C2)

C2[] <- c(.5,.5)
stopifnot(
  nrow(C2[])==12, ncol(C2[])==4,
  sum(abs(C2[[]]-.5)) < .0001
)
  

AddLink(A,C4)
AddLink(B,C4)
stopifnot(
  nrow(C4[])==12, ncol(C4[])==6,
  all(is.na(C4[[]]))
)

NodeProbs(C4) <- normalize(array(1:48,c(4,3,4)))


## Data Frame/matrix Selection

dfsel <- data.frame(A=factor(c("A2","A3"),levels=NodeStates(A)),
                    B=factor(c("B1","B3"),levels=NodeStates(B)))

C21.33 <- C4[dfsel]
stopifnot(
   nrow(C21.33)==2, ncol(C21.33)==6,
   C21.33[1,1] == "A2",
   C21.33[2,2] == "B3",
   abs(C21.33[1,3]-2/80) < .0001,
   abs(C21.33[2,4]-23/116) < .0001
)

dfselbak <- data.frame(B=factor(c("B3","B2"),levels=NodeStates(B)),
                       A=factor(c("A1","A4"),levels=NodeStates(A)))
C13.42 <- C4[dfselbak]
stopifnot(
   nrow(C13.42)==2, ncol(C13.42)==6,
   C13.42[1,1] == "A1",
   C13.42[2,2] == "B2",
   abs(C13.42[1,3]-9/108) < .0001,
   abs(C13.42[2,4]-20/104) < .0001
)

C2[dfsel] <- matrix(c(.7,.6,.3,.4),2)
C2[dfselbak] <- c(.9,.1)
stopifnot(
  sum(abs(C2[[]][,1] - c(.5,.7,.5,.5, .5,.5,.5,.9, .9,.5,.6,.5))) < .0001
)
## Test for error with using variables in selection inside of a
## function.
testSel <- function(node,sel1,sel2, val) {
 localselvar <- data.frame(sel1,sel2)
 names(localselvar) <- ParentNames(node)
 node[localselvar]
 node[localselvar]<-val
 invisible(node)
}
 
testSel(C2,factor(c("A2","A3"),levels=NodeStates(A)),
           factor(c("B1","B3"),levels=NodeStates(B)),
           matrix(c(.7,.6,.3,.4),2))


## Array-like selection
stopifnot(
  sum(abs(C4[[2,3]]-c(10,22,34,46)/112))<.0001,
  sum(abs(C4[[B=2,A=4]]-c(8,20,32,44)/104))<.0001
)

C1.23 <- C4[1,2:3]
stopifnot(
  nrow(C1.23)==2, ncol(C1.23)==6,
  sum(abs(C1.23[,3] - c(5/92 ,9/108))) <.0001
)
C2[] <- .5
C2[1,2:3] <- .99
stopifnot(
  sum(abs(C2[[]][,1] - c(.5,.5,.5,.5, .99,.5,.5,.5, .99,.5,.5,.5))) < .0001
)

C1.23 <- C4["A1",c("B2","B3")]
stopifnot(
  nrow(C1.23)==2, ncol(C1.23)==6,
  sum(abs(C1.23[,3] - c(5/92 ,9/108))) <.0001
)
C2[] <- .5
C2["A1",c("B2","B3")] <- .99
stopifnot(
  sum(abs(C2[[]][,1] - c(.5,.5,.5,.5, .99,.5,.5,.5, .99,.5,.5,.5))) < .0001
)

C34.12 <- C4[3:4,1:2]
stopifnot(
  nrow(C34.12)==4, ncol(C34.12)==6,
  sum(abs(C34.12[,3] - c(3/84,4/88, 7/100, 8/104))) <.0001
)
C2[] <- .5
C2[3:4,1:2] <- .99
stopifnot(
  sum(abs(C2[[]][,1] - c(.5,.5,.99,.99, .5,.5,.99,.99, .5,.5,.5,.5))) < .0001
)

## Wildcards

C1. <- C4[1,EVERY_STATE]
stopifnot(
  nrow(C1.) == 3, ncol(C1.)==6,
  sum(abs(C1.[,3] -c(1/76, 5/92, 9/108))) < .0001
)
C2[] <-.5
C2[1,EVERY_STATE] <- "C1"
stopifnot(
  sum(abs(C2[[]][,1] - c(1,.5,.5,.5, 1,.5,.5,.5, 1,.5,.5,.5))) < .0001
)

C.2 <- C4[EVERY_STATE,2]
stopifnot(
  nrow(C.2) == 4, ncol(C.2)==6,
  sum(abs(C.2[,3] -c(5/92, 6/96, 7/100, 8/104))) < .0001
)
C2[] <-.5
C2[EVERY_STATE,2] <- "C2"
stopifnot(
  sum(abs(C2[[]][,1] - c(.5,.5,.5,.5, 0,0,0,0, .5,.5,.5,.5))) < .0001
)

C1. <- C4["A1","*"]
stopifnot(
  nrow(C1.) == 3, ncol(C1.)==6,
  sum(abs(C1.[,3] -c(1/76, 5/92, 9/108))) < .0001
)
C2[] <-.5
C2["A1","*"] <- "C1"
stopifnot(
  sum(abs(C2[[]][,1] - c(1,.5,.5,.5, 1,.5,.5,.5, 1,.5,.5,.5))) < .0001
)

C.2 <- C4["*","B2"]
stopifnot(
  nrow(C.2) == 4, ncol(C.2)==6,
  sum(abs(C.2[,3] -c(5/92, 6/96, 7/100, 8/104))) < .0001
)
C2[] <-.5
C2["*","B2"] <- "C2"
stopifnot(
  sum(abs(C2[[]][,1] - c(.5,.5,.5,.5, 0,0,0,0, .5,.5,.5,.5))) < .0001
)

## Missing parent values

C1. <- C4[1,]
stopifnot(
  nrow(C1.) == 3, ncol(C1.)==6,
  sum(abs(C1.[,3] -c(1/76, 5/92, 9/108))) < .0001
)
C2[] <-.5
C2[1,] <- "C1"
stopifnot(
  sum(abs(C2[[]][,1] - c(1,.5,.5,.5, 1,.5,.5,.5, 1,.5,.5,.5))) < .0001
)

C.2 <- C4[,2]
stopifnot(
  nrow(C.2) == 4, ncol(C.2)==6,
  sum(abs(C.2[,3] -c(5/92, 6/96, 7/100, 8/104))) < .0001
)
C2[] <-.5
C2[,2] <- "C2"
stopifnot(
  sum(abs(C2[[]][,1] - c(.5,.5,.5,.5, 0,0,0,0, .5,.5,.5,.5))) < .0001
)

C1. <- C4[A=1]
stopifnot(
  nrow(C1.) == 3, ncol(C1.)==6,
  sum(abs(C1.[,3] -c(1/76, 5/92, 9/108))) < .0001
)
C2[] <-.5
C2[A=1] <- "C1"
stopifnot(
  sum(abs(C2[[]][,1] - c(1,.5,.5,.5, 1,.5,.5,.5, 1,.5,.5,.5))) < .0001
)

C.2 <- C4[B="B2"]
stopifnot(
  nrow(C.2) == 4, ncol(C.2)==6,
  sum(abs(C.2[,3] -c(5/92, 6/96, 7/100, 8/104))) < .0001
)
C2[] <-.5
C2[B="B2"] <- "C2"
stopifnot(
  sum(abs(C2[[]][,1] - c(.5,.5,.5,.5, 0,0,0,0, .5,.5,.5,.5))) < .0001
)

## Data frame as value

dfset <- data.frame(A=factor(c("A2","A3"),levels=NodeStates(A)),
                    B=factor(c("B1","B3"),levels=NodeStates(B)),
                    C.C1=c(1,0), C.C2=c(0,1))
C2[] <- .5
C2[] <- dfset
stopifnot(
  sum(abs(C2[[]][,1] - c(.5,1,.5,.5, .5,.5,.5,.5, .5,.5,0,.5))) < .0001
)

## Continuous Child node
AddLink(B2,Cont)
stopifnot(
  nrow(Cont[])==8, ncol(Cont[])==3,
  sum(abs(Cont[[]]-c(0,0,8,1))) < .0001
)

AddLink(A,CCC)
AddLink(B,CCC)
stopifnot(
  nrow(CCC[])==12, ncol(CCC[])==3,
  all(is.na(CCC[[]]))
)

Cont[] <- 0
Cont[1,1] <- 1.1
Cont[2:3,2] <- c(2.2,3.2)
Cont["A4","*"] <- 4
\dontrun{
## Can't set to multiple values when using * selection.
Cont["A4","*"] <- c(4.1,4.2) ## Generates an error
}
stopifnot(
  sum(abs(Cont[[]]-c(1.1,0,0,4,0,2.2,3.2,4))) < .0001,
  abs(Cont[["A1","B1"]]-1.1) <.0001,
  sum(abs(Cont[[B=2,A=2:3]]-c(2.2,3.2))) < .0001,
  sum(abs(Cont[[A=4]] -4)) < .0001
)

## Set by integer count
## 12 rows in A*B combinations
for (i in 1:12) {
  CCC[i] <- i
  C2[i] <- i/100
}
stopifnot(
  sum(abs(CCC[[]]-t(matrix(1:24,3,4)))) <.0001,
  sum(abs(C2[[]][,1]-t(matrix(1:24/100,3,4)))) <.001
)
for (i in 1:12) {
  stopifnot(
    abs(CCC[[i]] - i) <.0001,
    abs(C2[[i]][1] - i/100) <.0001
  )
}

DeleteNetwork(xnet)

}
\keyword{array}
\keyword{interface}
