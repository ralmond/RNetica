##
## Edges.R -- These functions are related to links between nodes and
## conditional probability tables.


## #############################################################
## Clique node is a subclass of node

CliqueNode <-
  setRefClass("CliqueNode",
              fields=c(clique="list"),
              contains="NeticaNode",
              methods=list(
                  initialize = function(...,clique=list()) {
                    node <- callSuper(...,clique=clique)
                    node
                  },
                  show = function() {
                    cat(ifelse(discrete,"Discrete ","Continuous"),
                        "Netica Clique Node named ",Name,"in network ",Net$Name,"\n")
                    if (isActive()) {
                      cat("  Node is currently active.\n")
                    } else {
                      cat("  Node is not currently active.\n")
                    }
                    cat("Contains nodes: ",paste(sapply(clique,NodeName),
                                                 collapse=","),"\n")
                  }
              ))

setMethod("toString","CliqueNode", function(x,...) {
  if (is.active(x))
    paste("<Clique Node:",x$Name,">")
  else
    paste("<Deleted Clique Node:",x$Name,">")
})



AddLink <- function (parent, child) {
  if (length(parent)>1L || !is.NeticaNode(parent) || !is.active(parent)) {
    stop ("Parent is not an active Netica node", parent)
  }
  if (length(child)>1L || !is.NeticaNode(child) || !is.active(child)) {
    stop ("Child is not an active Netica node", child)
  }
  handle <- .Call("RN_AddLink",parent,child,PACKAGE=RNetica)
  parent$signalErrors()
  invisible(handle)
}

DeleteLink <- function (parent, child) {
  if (length(parent)>1L || !is.NeticaNode(parent) || !is.active(parent)) {
    stop ("Parent is not an active Netica node", parent)
  }
  if (length(child)>1L || !is.NeticaNode(child) || !is.active(child)) {
    stop ("Child is not an active Netica node", child)
  }
  handle <- .Call("RN_DeleteLink",parent,child,PACKAGE=RNetica)
  parent$signalErrors()
  invisible(handle)
}

ReverseLink <- function (parent, child) {
  if (length(parent)>1L || !is.NeticaNode(parent) || !is.active(parent)) {
    stop ("Parent is not an active Netica node", parent)
  }
  if (length(child)>1L || !is.NeticaNode(child) || !is.active(child)) {
    stop ("Child is not an active Netica node", child)
  }
  handle <- .Call("RN_ReverseLink",parent,child,PACKAGE=RNetica)
  parent$signalErrors()
  invisible(handle)
}

NodeChildren <- function (parent) {
  if (length(parent)>1L || !is.NeticaNode(parent) || !is.active(parent)) {
    stop ("Parent is not an active Netica node", parent)
  }
  handle <- .Call("RN_GetNodeChildren",parent,PACKAGE=RNetica)
  parent$signalErrors()
  handle
}


NodeParents <- function (child) {
  if (length(child)>1L || !is.NeticaNode(child) || !is.active(child)) {
    stop ("Child is not an active Netica node", child)
  }
  handle <- .Call("RN_GetNodeParents",child,PACKAGE=RNetica)
  child$signalErrors()
  handle
}

"NodeParents<-" <- function (child, value) {
  if (length(child)>1L || !is.NeticaNode(child) || !is.active(child)) {
    stop ("Child is not an active Netica node", child)
  }
  if(!is.list(value)) {
    stop("Value must be list of Netica nodes, or NULLs")
  }
  ## Damn the R Core Development Team Anyway!  The fact that if x is a
  ## list x[[2]] <-NULL deletes the second element rather than
  ## replacing it will null is an incredibly clever overloading of a
  ## function in such a way that makes no logical sense.  They need to
  ## go back an reread _Surreal Numbers_ (and while they are at it
  ## Kernighan and Richie, so that can see what clean minimalist
  ## language design is).  This R Bug can be worked around by
  ## using x[2] <- list(NULL).  But inevitably users are going to
  ## mistake this for x[[2]] <-list(NULL) which produces a different
  ## result or x$Input2 <-list(NULL) which also produces list(NULL) as
  ## the second element instead of NULL.  The following line of code
  ## should fix this overcompensation.
  value <- lapply(value, function(x) if(is.list(x)) x[[1L]] else x)
  if (length(value) > 0) {
    if (!all(sapply(value,is.null) | (sapply(value, is.NeticaNode)))) {
      stop("Value must be list of Netica nodes, or NULLs")
    }
  }
  ## Although it looks like we are setting all parents at once,
  ## internally, we are changing them one at a time.  We can get into
  ## an awkward race condition if we are reorder nodes in such a way
  ## as to make a node a parent twice.  This next bit tests for that.
  oldpar <- NodeParents(child)
  newindex <- 1L:length(value)
  oldparnames <- sapply(oldpar,NodeName)
  newvalnames <- sapply(value,function(n) if(is.null(n)) "" else NodeName(n))
  ## match() forces everything to char, but as.character() does not
  ## correctly use as.character() on elements of a list. So force names.
  oldindex <- match(newvalnames, oldparnames)
  if( any(newindex<oldindex,na.rm=TRUE)) {
    ##cat("Reorder nulling. \n")
    ##Okay, safest thing to do is to set to a vector of NULLs first,
    ##Then add new links.
    oldnames <- names(oldpar)
    oldpar <- vector("list",length(oldpar))
    .Call("RN_SetNodeParents",child, oldpar,PACKAGE=RNetica)
    child$signalErrors()
    ## This sets names to old parent names, which is probabily not
    ## what was wanted, so clear names.
    .Call("RN_SetNodeInputNames",child, oldnames,PACKAGE=RNetica)
    child$signalErrors()
  }
  handle <- .Call("RN_SetNodeParents",child, value,PACKAGE=RNetica)
  child$signalErrors()
  invisible(handle)
}

## Now that these are formal R6 objects instead of strings with
## attributes, this should no longer be an issue.

## ## This is another workaround for an ancient S design flaw (I think
## ## this one has been around since the Blue Book.  The c() function
## ## strips attributes, which turns NeticaNodes and NeticaBN objects
## ## into ordinary strings.  This is a replacement for c() which (a)
## ## only works on lists and (b) does not strip attributes.
## cc <- function (...) {
##   args <- list(...)
##   isList <- sapply(args,function(x) is(x,"list"))
##   lens <- sapply(args,length)
##   lens <- ifelse(isList,lens,1L)
##   result <- vector("list",sum(lens))
##   n <- 1
##   for (arg in args) {
##     if (is(arg,"list")) {
##       m <- length(arg)
##       result[n:(n+m-1)] <- arg
##       n <- n+m
##     } else {
##       result[[n]] <- arg
##       n <- n+1
##     }
##   }
##   result
## }

## c.NeticaBN <- cc
## c.NeticaNode <- cc


NodeInputNames <- function (node) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  CCodeLoader()
  names <- .Call("RN_GetNodeInputNames",node,PACKAGE=RNetica)
  node$signalErrors()
  names
}

"NodeInputNames<-" <- function (node, value) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  value <- as.character(value)
  if (any(is.na(value)) || any(!is.IDname(value))) {
    stop("Illegal link names: ", value)
  }
  CCodeLoader()
  handle <- .Call("RN_SetNodeInputNames",node, value,PACKAGE=RNetica);
  node$signalErrors()
  invisible(handle)
}


##This function is meant to do minimal checking so it will be fast.
ParentStates <- function (node) {
  result <- lapply(NodeParents(node),NodeStates)
  names(result) <- ParentNames(node)
  result
}

ParentNames <- function (node) {
  parnames <- sapply(NodeParents(node),NodeName)
  inames <- NodeInputNames(node)
  ifelse (nchar(inames)==0,parnames,inames)
}

AbsorbNodes <- function (nodes) {
  if (is.NeticaNode(nodes) && length(nodes) ==1L) {
    nodes <- list(nodes)
  }
  if (any(!sapply(nodes,is.NeticaNode))) {
    stop("Expected a list of Netica nodes, got, ",nodes)
  }
  ## print(nodes)
  net <- nodes[[1]]$Net
  CCodeLoader()
  if (NeticaVersion(NetworkSession(net))$number < 600) {
    warning("Absorb Nodes has errors in API version < 6.0")
    return(NULL)
  }
  handles <- .Call("RN_AbsorbNodes",nodes,PACKAGE=RNetica)
  net$signalErrors()
  ## Delete the node objects from the net cache.
  for (nd in nodes) {
    if (is(nd,"NeticaNode")) {
      nd$deactivate()
      rm(list=nd$Name,envir=net$nodes)
    }
  }
  if (length(handles)==1L) handles <- handles[[1L]]
  invisible(handles)
}

is.NodeRelated <- function (node1, node2, relation="connected") {
  if (length(node1)>1L || !is.NeticaNode(node1) || !is.active(node1)) {
    stop ("Node1 is not an active Netica node", node1)
  }
  if (length(node2)>1L || !is.NeticaNode(node2) || !is.active(node2)) {
    stop ("Node2 is not an active Netica node", node2)
  }
  relation <- as.character(relation)
  if (is.na(relation[1L])) {
    stop("Bad relation.")
  }
  if (length(relation) >1L) {
    warning("Relation has length > 1, only first value is used.")
  }
  handle <- .Call("RN_IsNodeRelated",node1,relation,node2,PACKAGE=RNetica)
  node1$signalErrors()
  handle
}

GetRelatedNodes <- function (nodelist, relation="connected") {
  if (is.NeticaNode(nodelist) && length(nodelist) ==1L) {
    nodelist <- list(nodelist)
  }
  if (any(!sapply(nodelist,is.NeticaNode))) {
    stop("Expected a list of Netica nodes, got, ",nodelist)
  }
  relation <- as.character(relation)
  if (is.na(relation[1L])) {
    stop("Bad relation.")
  }
  if (length(relation) >1L) {
    warning("Relation has length > 1, only first value is used.")
  }
  CCodeLoader()
  handle <- .Call("RN_GetRelatedNodes",nodelist,relation,PACKAGE=RNetica)
  nodelist[[1]]$signalErrors()
  handle
}

MakeCliqueNode <- function(nodelist) {
  if (any(!sapply(nodelist,function (nd) {is.NeticaNode(nd) &&
                                          is.active(nd)}))) {
    stop("Expected a list of Netica nodes, got, ",nodelist)
  }
  net <- nodelist[[1]]$Net
  CCodeLoader()
  handle <- .Call("RN_MakeCliqueNode",nodelist,net,PACKAGE=RNetica)
  net$signalErrors()
  handle
}

is.CliqueNode <- function (x) {
  is(x,"CliqueNode")
}

GetClique <- function (cliquenode) {
  cliquenode$clique
}


#####################################################################
## Probability Calculations
#####################################################################


## To start, pass -1.
## Using 1 based indexing, but Netica uses 1-based.  Convert inside C code.
nextconfig <- function (current, maxvals) {
  ## Base case
  if (current[1L] < 0) return (rep(1L,length(maxvals)))
  whichDim = length(maxvals)
  while (whichDim > 0) {
    current[whichDim] <- current[whichDim] +1L
    if (current[whichDim] <= maxvals[whichDim]) {
      return (current)
    } else {
      current[whichDim] <- 1L
      whichDim <- whichDim-1L
    }
  }
  ## Dropped off of the end of the list, return NA as a signal
  ## we are done
  return(NA)
}

## Converts an index into a matrix that can be used to
## access the corresponding row of the array.
configindex <- function(config,nstates) {
  ind <- matrix(0,nstates,length(config)+1L)
  for (p in 1L:length(config)) {
    ind[,p] <- config[p]
  }
  ind[,length(config)+1L] <- 1L:nstates
  return(ind)
}

NodeProbs <- function (node) {
  CCodeLoader()
  if (length(node)>1L || !is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  parnames <- ParentStates(node)
  statecounts <- sapply(parnames,length)

  childnames <- list(NodeStates(node))
  names(childnames) <- NodeName(node)
  nstates <- length(childnames[[1L]])

  dimnames <- c(parnames,childnames)
  dims <- sapply(dimnames,length)
  result <- array(NA_real_,dim=dims,dimnames=dimnames)

  if (length(statecounts)>0) {
    config <- -1L
    while (!is.na((config <- nextconfig(config,statecounts))[1L])) {
      row <- .Call("RN_GetNodeProbs",node,config,PACKAGE=RNetica)
      if (!is.null(row))
        result[configindex(config,nstates)] <- row
    }
  } else { ## Prior node, no parents.
    row <- .Call("RN_GetNodeProbs",node,NULL,PACKAGE=RNetica)
    if (!is.null(row))
      result[] <- row
  }
  node$signalErrors()
  class(result) <- c("CPA",class(result))
  result
}

"NodeProbs<-" <- function (node,value) {
  CCodeLoader()
  if (length(node)>1L || !is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  if (!is.numeric(value)) {
    stop("Value must be numeric")
  }
  if (any(value >1 | value <0, na.rm=TRUE)) {
    stop("Values outside of range [0,1] in probability table.")
  }

  statecounts <- sapply(NodeParents(node),NodeNumStates)
  nstates <- NodeNumStates(node)
  dims <- c(statecounts,nstates)
  if (length(dims) == 1L) {
    if (length(value) != dims) {
      stop("Dimensions not correct for this node.")
    }
  } else {
    if (length(dims) != length(dim(value)) || any (dim(value) != dims)) {
      stop("Dimensions not correct for this node.")
    }
  }
  if (length(statecounts)>0) {
    config <- -1L
    while (!is.na((config <- nextconfig(config,statecounts))[1L])) {
      ci <- configindex(config,nstates)
      .Call("RN_SetNodeProbs",node,config,value[ci],PACKAGE=RNetica)
    }
  } else { ## Prior node with no parents.
    .Call("RN_SetNodeProbs",node,NULL,value,PACKAGE=RNetica)
  }
  node$signalErrors()
  invisible(node)
}


IsNodeDeterministic <- function (node) {
  if (length(node)>1L || !is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  CCodeLoader()
  handle <- .Call("RN_IsNodeDeterministic",node,PACKAGE=RNetica)
  node$signalErrors()
  handle
}

HasNodeTable <- function (node) {
  if (length(node)>1L || !is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  CCodeLoader()
  result <- .Call("RN_HasNodeTable",node,PACKAGE=RNetica)
  node$signalErrors()
  names(result) <- c("exists","complete")
  result
}

DeleteNodeTable <- function (node) {
  if (length(node)>1L || !is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  CCodeLoader()
  handle <- .Call("RN_DeleteNodeTable",node,PACKAGE=RNetica)
  node$signalErrors()
  invisible(handle)
}


## This function interprets the various input modes and returns an
## integer matrix which does the selection.
## env is frame in which to do the evaluation, which is probably the
## parent frame of the calling funciton
## Now getting this using the sys.call function.
parseDims <- function (node, clist, dropvalue=FALSE, calling.frame=2) {
  ## This creates a call object with the arguments
  ## My code works, I don't know why!
  ## R seems to stick an extra .local when called with [[...]], so move from
  ## -1 to -2
  if (dropvalue) clist <- clist[-length(clist)]
  if (!is.null(names(clist))) {
    dropme <- names(clist) == "drop"
    clist <- clist[!dropme]
  }
  ## Empty condition
  if (length(clist)==0) return(NULL)
  if (length(clist)==1 && class(clist[[1]]) == "name" &&
      clist[[1]]=="") return (NULL)
  ## Need to find and irradicate stray marks
  for (idim in 1:length(clist)) {
    if (is.name(clist[[idim]]) && nchar(clist[[idim]]) == 0) {
      ## Blank entry, replace with 1:n
      clist[idim] <- EV_STATE
    }
  }
  selection <- do.call("list",clist,envir=parent.frame(calling.frame))
  ## Unwrap selections by data frame and matrix.
  if (length(selection) == 1L) {
    if (is.data.frame(selection[[1]])) {
      selection <- selection[[1]]
    } else if (is.matrix(selection[[1]])) {
      selection <- selection[[1]]
      if (!is.null(colnames(selection))) {
        ## If there are column names, make it a data frame
        ## so we can permute columns as needed.
        selection <- as.data.frame(selection)
      }
    }
  }
  ## Need to deal with named arguments and missing dimensions.
  if (is.null(names(selection)) ||
      length(NodeParents(node)) == 0) {
    return (selection)
  }
  if (any(nchar(names(selection))==0)) {
    warning("Incomplete set of names, positional selection assumed.")
    return(selection)
  }
  inames <- ParentNames(node)
  matches <- pmatch(names(selection),inames)
  if (any(is.na(matches))) {
    ##Unmatched names, try parent names.
    matches <- pmatch(names(selection),
                      sapply(NodeParents(node),NodeName))
    if (any(is.na(matches))) {
      stop("Unmatched parent variable names in selection.")
    }
  }
  ## Replace abbreviated names with full names.
  names(selection) <- inames[matches]
  unmatched <- setdiff(inames,names(selection))
  if (length(unmatched)>0) {
    uml <- as.list(rep(EV_STATE,length(unmatched)))
    names(uml) <- unmatched
    selection <- c(selection,uml)
  }
  ## Reorder columns according to the list state.
  return (selection[inames])
}

## Forces selection into an integer matrix.
integerIndex <- function(node,selection,expandEvery=FALSE) {
  if(length(selection) ==1L && is.list(selection) &&
     is.data.frame(selection[[1L]])) {
    selection <- selection[[1L]]
  }
  if (!all(sapply(selection,is.numeric))) {
    for (i in 1L:length(selection)) {
      if (is.factor(selection[[i]])) {
        selection[[i]] <- as.integer(selection[[i]])
      } else if (is.character(selection[[i]])) {
        names <- c(NodeStates(NodeParents(node)[[i]]),"*")
        selection[[i]] <- match(selection[[i]],names)
        selection[[i]][selection[[i]]==length(names)] <- EV_STATE
        if (any(is.na(selection[[i]]))) {
          stop("Illegal state name.")
        }
      }
    }
  } else if (length(selection)==1L && length(NodeParents(node))>1L) {
    if (is.list(selection)) {
      ##Unlist if needed
      selection <- selection[[1]]
    }
    if (length(selection) > 1L) {
      ## Only one selection allowed in this mode.
      stop("Fewer selection arguments than parent variables.")
    }
    if (selection <1L) {
      stop("Negative configuration count selected.")
    }
    statecounts <- sapply(ParentStates(node),length)
    config <- -1L
    for (i in 1L:selection) {
      config <- nextconfig(config,statecounts)
    }
    if (is.na(config[1L])) {
      stop("Configuration count bigger than number of configurations.")
    }
    selection <- config
  }
  if (is.list(selection)) {
    if (!is.data.frame(selection)) {
      ## Call list object
      selection <- do.call(expand.grid,selection)
    }
    nsel <- nrow(selection)
    nvar <- ncol(selection)
    selection <- matrix(as.integer(as.matrix(selection)),nsel,nvar)
  } else { ## Vector, make it a one row matrix.
    selection <- matrix(as.integer(selection),1L)
  }
  if (ncol(selection) != length(NodeParents(node))) {
    stop("Configuration specification number of columns must match number of parents.")
  }
  ## Okay, now we have a matrix of numbers.  Substitute EVERY_STATE
  ## (external advertised value) for EV_STATE (internal Netica value).
  selection <- ifelse(selection==EVERY_STATE,EV_STATE,selection)

  if (expandEvery && any(selection == EV_STATE)) {
    pdims <- lapply(NodeParents(node),
                    function(par) 1L:NodeNumStates(par))
    result <- NULL
    for (irow in 1L:nrow(selection)) {
      expanded <- selection[irow,]
      if (any(expanded==EV_STATE)) {
        wild <- expanded==EV_STATE
        grid <- do.call("expand.grid",pdims[wild])
        expanded <- matrix(expanded,nrow(grid),length(pdims),byrow=TRUE)
        expanded[,wild] <- as.matrix(grid)
      }
      if (irow==1L) {
        result <- expanded
      } else {
        result <- rbind(result,expanded)
      }
    }
    selection <- result
  }
  return (selection)
}

## Converts integer matrix form into data frame of factors.
selectionToConfig <- function(node,selection) {
  result <- as.data.frame(selection)
  pstates <- ParentStates(node)
  names(result) <- names(pstates)
  for (i in 1L:length(result)) {
    result[[i]] <- factor(result[[i]],1L:length(pstates[[i]]),pstates[[i]])
  }
  result
}

setMethod("[","NeticaNode", function(x, i, j, ...,  drop=FALSE) {
  if (!is.NeticaNode(x) || !is.active(x)) {
    stop("Expected an active netica node.")
  }
  if (IsNodeDeterministic(x)) {
    warning("Use of '[' with deterministic nodes depricated, use '[['.", x)
  }
  ## Massage selection into a matrix of numeric indexes, EVERY_STATE
  ## values are only recognized on setting values, so expand them
  clist <- as.list(sys.call(-1)[-1:-2])          #kill braces and node name.
  selection <- parseDims(x,clist)             #Reads call to get args.
  if (is.null(selection) && length(NodeParents(x))>0) {
    selection <- rep(EV_STATE,length(NodeParents(x)))
  }
  doSelection(x,selection,drop,returnCPT=TRUE)
})

setMethod("[[","NeticaNode", function(x, i, j, ...,  drop=FALSE) {
  if (!is.NeticaNode(x) || !is.active(x)) {
    stop("Expected an active netica node, got",x)
  }
  if (!IsNodeDeterministic(x)) {
    ## Work around for rather obscure bug because of 
    ## R and RStudio interaction
    rsenv <- call(".rs.environment.isSuspendable")
    sc <- sys.calls()
    for (nframe in 1:length(sc)) {
      if (sc == sc) {
        stop("No Peeking RStudio!")
      }
    }
    warning("Use of '[[' with chance nodes is depricated: Use numericPart and '['.")
  }
  ## Massage selection into a matrix of numeric indexes, EVERY_STATE
  ## values are only recognized on setting values, so expand them
  clist <- as.list(sys.call(-1)[-1:-2])          #kill braces and node name.
  selection <- parseDims(x,clist)             #Reads call to get args.
  if (is.null(selection) && length(NodeParents(x))>0) {
    selection <- rep(EV_STATE,length(NodeParents(x)))
  }
  doSelection(x,selection,drop, returnCPT=FALSE)
})

doSelection <- function (x,selection,drop, returnCPT=FALSE) {
  CCodeLoader()
  ## No parent case
  if (is.null(selection)) {
    if (!returnCPT && IsNodeDeterministic(x)) {
      if (is.discrete(x)) {
        val <- .Call("RN_GetNodeFuncState",x,NULL,PACKAGE=RNetica)
        result <- NodeStates(x)[val]
      } else {
        result <- .Call("RN_GetNodeFuncReal",x,NULL,PACKAGE=RNetica)
      }
    } else {
      result <- .Call("RN_GetNodeProbs",x,NULL,PACKAGE=RNetica)
      if (is.null(result)) {
        result <- rep(NA_real_,length(NodeStates(x)))
      }
      if (length(result) > 0) {
        names(result) <- paste(NodeName(x),NodeStates(x),sep=".")
      } else {
        result <- NA_real_
        names(result) <- NodeName(x)
      }
    }
  } else {
    ## Multiple parent case
    selection <- integerIndex(x,selection,expandEvery=TRUE)
    if (drop && (is.null(nrow(selection)) || nrow(selection) == 1L)) {
      ## Single Row case
      if (!returnCPT && IsNodeDeterministic(x)) {
        if (is.discrete(x)) {
          val <- .Call("RN_GetNodeFuncState",x,selection,PACKAGE=RNetica)
          result <- NodeStates(x)[val]
        } else {
          result <- .Call("RN_GetNodeFuncReal",x,selection,PACKAGE=RNetica)
        }
      } else {
        result <- .Call("RN_GetNodeProbs",x,selection,PACKAGE=RNetica)
        if (is.null(result)) {
          result <- rep(NA_real_,length(NodeStates(x)))
        }
        if (length(result) > 0) {
          names(result) <- paste(NodeName(x),NodeStates(x),sep=".")
        } else {
          result <- NA_real_
          names(result) <- NodeName(x)
        }
      }
    } else {
      ## matrix or data frame
      config <- selectionToConfig(x,selection)
      nsel <- nrow(selection)
      if (!returnCPT && IsNodeDeterministic(x)) {
        val <- rep(NA,nsel)
        if (is.discrete(x)) {
          for (i in 1L:nsel) {
            val[i] <- .Call("RN_GetNodeFuncState",x,selection[i,],
                            PACKAGE=RNetica)
          }
          val <- factor(val,levels=1L:NodeNumStates(x),NodeStates(x))
        } else {
          for (i in 1L:nsel) {
            val[i] <- .Call("RN_GetNodeFuncReal",x,selection[i,],
                            PACKAGE=RNetica)
          }
        }
        if (drop) {
          result <- val
        } else {
          result <- data.frame(config,val)
          names(result)[length(result)] <- NodeName(x)
        }
      } else {
        probs <- matrix(NA,nsel,max(NodeNumStates(x),1L))
        for (i in 1L:nsel) {
          val <- .Call("RN_GetNodeProbs",x,selection[i,],PACKAGE=RNetica)
          if (!is.null(val)) {
            probs[i,] <- val
          }
        }
        probs <- as.data.frame(probs)
        if (length(probs) > 1L) {
          names(probs) <- paste(NodeName(x),NodeStates(x),sep=".")
        } else {
          names(probs) <- NodeName(x)
        }
        if (drop) {
          result <- probs
        } else {
          result <- as.CPF(data.frame(config,probs))
        }
      }
    }
  }
  x$signalErrors()
  result
}

## Replaced.
## setMethod("[[", "NeticaNode", function(x, i, j, ...) {
##   if (!is.NeticaNode(x) || !is.active(x)) {
##     stop("Expected an active netica node.")
##   }
##   ## Massage selection into a matrix of numeric indexes, EVERY_STATE
##   ## values are only recognized on setting values, so expand them
##   selection <- parseDims(x)             #Reads call to get args.
##   if (is.null(selection) && length(NodeParents(x))>0) {
##     selection <- rep(EV_STATE,length(NodeParents(x)))
##   }
##   doSelection(x,selection,drop=TRUE)
## })


setMethod("[<-","NeticaNode",function(x, i, j, ..., value) {
  CCodeLoader()
  if (!is.NeticaNode(x) || !is.active(x)) {
    stop("Expected an active netica node, got",x)
  }

  ## Massage selection into a matrix of numeric indexes, EVERY_STATE
  ## values are handled specially by Netica, so leave them in place.
  ## env is where
  clist <- as.list(sys.call(-1)[-1:-2])          #kill braces and node name.
  selection <- parseDims(x,clist,dropvalue=TRUE)
  npar <- length(NodeParents(x))
  nstate <- NodeNumStates(x)
  if (is.null(selection) && is.data.frame(value)) {
    facts <- sapply(value,is.factor)
    if (sum(facts) == npar) {
      ## Use selection from factors in data frame
      selection <- value[facts]
      value <- value[!facts]
    } else if (sum(facts) == npar+1L && all(facts)) {
      ## Same condition, only last column is values
      selection <- value[1L:npar]
      value <- value[[npar+1L]]
    }
  }
  if (is.null(selection) && npar>0) {
    selection <- rep(EV_STATE,npar)
  }
  if (is.list(selection)) {
    selection <- integerIndex(x,selection,expandEvery=FALSE)
  }

  ## Now massage value into a matrix or appropriate vector
  if (is.data.frame(value)) {
    if (length(value)==1L) {
      value <- value[[1L]] ## Single value coded in data frame.
    } else {
      value <- as.matrix(value)
    }
  }
  valisprobs <- TRUE
  if (is.null(nrow(value)) && is.numeric(value) &&
      length(value) == nstate && abs(sum(value)-1) <.0001) {
    ## probability vector, remake as matrix
    value <- matrix(value,1L)
  }
  if (is.null(nrow(value)) && is.numeric(value) &&
      length(value) == nstate-1L &&
      (is.null(nrow(selection)) || nrow(selection) != length(value)) &&
      sum(value) <1L) {
    ## Unnormalized probability vector, remake as matrix
    value <- matrix(c(value,1-sum(value)),1L)
  }
  if (is.matrix(value)) {
    if (nrow(value) > 1L && (is.null(nrow(selection)) ||
                            nrow(selection)!=nrow(value))) {
      stop("Number of rows selected and rows in value do not match; node",
           x$Name)
    }
    if (!is.numeric(value) || any(value<0) || any(value>1)) {
      stop("Expected a matrix of values between 0 and 1; node", x$Name)
    }
    if (ncol(value) == nstate-1L) {
      ## Normalize
      value <- cbind(value,1-apply(value,1L,sum))
    }
    if (ncol(value) != nstate) {
      stop("Probabilities not supplied for every state; node", x$Name)
    }
  } else {
    if (is.numeric(value) && all(value == as.integer(value)) &&
                             all (value > 0)) {
      ## Numeric state selection
      valisprobs <- FALSE
    }
    if (is.factor(value)) {
      valisprobs <- FALSE
      value <- as.numeric(value)
    }
    if (is.character(value)) {
      valisprobs <- FALSE
      value <- match(value,NodeStates(x))
    }
    if (length(value) > 1L && (is.null(nrow(selection)) ||
                            nrow(selection)!=length(value))) {
      stop("Number of rows selected and rows in value do not match; node",
           x$Name)
    }
    if(is.discrete(x) && nstate==2 && valisprobs) {
      ## Single column and binary node, normalize
      value <- cbind(value,1-value)
    }
  }
  if (!valisprobs) {
    warning("The use of '[<-' with deterministic nodes is depricated.  Use '[[<-'.")
  }
  if (is.null(selection)) {
    ## No parents
    if (is.continuous(x)) {
        if (valisprobs) {
          .Call("RN_SetNodeProbs",x,NULL,as.double(value),PACKAGE=RNetica)
        } else {
          .Call("RN_SetNodeFuncReal",x,NULL,as.double(value),
                PACKAGE=RNetica)
        }
    } else {
      if (valisprobs) {
        .Call("RN_SetNodeProbs",x,NULL,as.double(value),PACKAGE=RNetica)
      } else {
        .Call("RN_SetNodeFuncState",x,NULL,as.integer(value),PACKAGE=RNetica)
      }
    }
  } else {
    if (is.null(nrow(selection))) {
      ## Single Row case
      if (is.continuous(x)) {
        if (valisprobs) {
          .Call("RN_SetNodeProbs",x,selection,as.double(value),PACKAGE=RNetica)
        } else {
          .Call("RN_SetNodeFuncReal",x,selection,as.double(value),
                PACKAGE=RNetica)
        }
      } else {
        if (valisprobs) {
          .Call("RN_SetNodeProbs",x,selection,as.double(value),PACKAGE=RNetica)
        } else {
          .Call("RN_SetNodeFuncState",x,selection,as.integer(value),
                PACKAGE=RNetica)
        }
      }
    } else {
      ## matrix or data frame
      nsel <- nrow(selection)
      if (!valisprobs && is.continuous(x)) {
        for (i in 1L:nsel) {
          .Call("RN_SetNodeFuncReal",x,selection[i,],
                as.double(ifelse(length(value)==1L,value,value[i])),
                PACKAGE=RNetica)
        }
      } else if (valisprobs) {
        if (is.matrix(value) && nrow(value) ==1L) {
          value <- value[,]
        }
        if (is.matrix(value)) {
          for (i in 1L:nsel) {
            .Call("RN_SetNodeProbs",x,selection[i,],
                  as.double(value[i,]),PACKAGE=RNetica)
          }
        } else {
          for (i in 1L:nsel) {
            .Call("RN_SetNodeProbs",x,selection[i,],as.double(value),
                  PACKAGE=RNetica)
          }
        }
      } else {
        if (length(value) > 1L) {
          for (i in 1L:nsel) {
            .Call("RN_SetNodeFuncState",x,selection[i,],
                  as.integer(value[i]), PACKAGE=RNetica)
          }
        } else {
          for (i in 1L:nsel) {
            .Call("RN_SetNodeFuncState",x,selection[i,],
                  as.integer(value), PACKAGE=RNetica)
          }
        }
      }
    }
  }
  x$signalErrors()
  invisible(x)
})

setMethod("[[<-","NeticaNode",function(x, i, j, ..., value) {
  CCodeLoader()
  if (!is.NeticaNode(x) || !is.active(x)) {
    stop("Expected an active netica node, got",x)
  }

  ## Massage selection into a matrix of numeric indexes, EVERY_STATE
  ## values are handled specially by Netica, so leave them in place.
  ## env is where
  clist <- as.list(sys.call(-1)[-1:-2])          #kill braces and node name.
  selection <- parseDims(x,clist,dropvalue=TRUE)
  npar <- length(NodeParents(x))
  nstate <- NodeNumStates(x)
  if (is.null(selection) && is.data.frame(value)) {
    facts <- sapply(value,is.factor)
    if (sum(facts) == npar) {
      ## Use selection from factors in data frame
      selection <- value[facts]
      value <- value[!facts]
    } else if (sum(facts) == npar+1L && all(facts)) {
      ## Same condition, only last column is values
      selection <- value[1L:npar]
      value <- value[[npar+1L]]
    }
  }
  if (is.null(selection) && npar>0) {
    selection <- rep(EV_STATE,npar)
  }
  if (is.list(selection)) {
    selection <- integerIndex(x,selection,expandEvery=FALSE)
  }

  ## Now massage value into an appropriate vector
  if (is.data.frame(value)) {
      value <- value[,1L]
  }
  if (is.continuous(x)) {
    valisfact <- FALSE
    if (is.factor(value)) {
      value <- as.numeric(value)
      valisfact <- TRUE
    }
    if (is.character(value)) {
      valisfact <- TRUE
      value <- match(value,NodeStates(x))
    }

  } else {
    valisfact <- TRUE
    if (is.numeric(value))
      if (any(value != as.integer(value)) || any (value < 0) ||
          any(value > NodeNumStates(x))) {
        stop ("Values must be strings, factors, or integers between 1 and ",
              NodeNumStates(x))
      }
    if (is.factor(value)) {
      value <- as.numeric(value)
    }
    if (is.character(value)) {
      value <- match(value,NodeStates(x))
    }
  }
  if (length(value) > 1L && (is.null(nrow(selection)) ||
                             nrow(selection)!=length(value))) {
    stop("Number of rows selected and rows in value do not match; node",
         x$Name)
  }

  if (is.null(selection)) {
    ## No parents
    if (valisfact) {
      .Call("RN_SetNodeFuncState",x,NULL,as.integer(value),PACKAGE=RNetica)
    } else {
      .Call("RN_SetNodeFuncReal",x,NULL,as.double(value),
            PACKAGE=RNetica)
    }
  } else if (is.null(nrow(selection))) {
    ## Single Row case
    if (valisfact) {
      .Call("RN_SetNodeFuncState",x,selection,as.integer(value),
            PACKAGE=RNetica)
    } else {
      .Call("RN_SetNodeFuncReal",x,selection,as.double(value),
            PACKAGE=RNetica)
    }
  } else {
    ## matrix or data frame
    nsel <- nrow(selection)
    if (valisfact) {
      if (length(value) > 1L) {
        for (i in 1L:nsel) {
          .Call("RN_SetNodeFuncState",x,selection[i,],
                as.integer(value[i]), PACKAGE=RNetica)
        }
      } else {
        for (i in 1L:nsel) {
          .Call("RN_SetNodeFuncState",x,selection[i,],
                as.integer(value), PACKAGE=RNetica)
        }
      }
    } else {
      for (i in 1L:nsel) {
        .Call("RN_SetNodeFuncReal",x,selection[i,],
              as.double(ifelse(length(value)==1L,value,value[i])),
              PACKAGE=RNetica)
      }
    }
  }
  x$signalErrors()
  invisible(x)
})


############################################################################
## Merging Networks:  EM -- SM algorithm
###########################################################################

AdjoinNetwork <- function (sm, em, setname=character()) {
  emnodes <- NetworkAllNodes(em)
  smnodes <- NetworkAllNodes(sm)
  newnodes <- CopyNodes(emnodes,newnet=sm)
  if (is.NeticaNode(newnodes)) {
    ## Singleton response from copy-nodes might have been unlisted.
    newnodes <- list (newnodes)
  }
  allstubs <- list()
  for (i in 1:length(newnodes)) {
    node <- newnodes[[i]]
    stubs <- sapply(NodeParents(node),NodeKind) == "Stub"
    if (any(stubs)) {
      allstubs <- c(NodeParents(node)[stubs],allstubs)
      NodeParents(node)[stubs] <- smnodes[NodeInputNames(node)[stubs]]
      if (any(sapply(NodeParents(node),NodeKind) == "Stub")) {
        warning("Node ",as.character(node)," has unresolved stub parents.")
      }
    }
    ## Node may have been renamed.
    enode <- emnodes[[i]]
    NodeSets(node) <- c(setname,NodeSets(enode))
  }
  ## Remove stubs
  for (stub in allstubs) {
    if (is(stub,"NeticaNode")) {
      ##cat("Removing stub ",stub$Name,"\n")
      stub$deactivate()
      rm(list=stub$Name,envir=sm$nodes)
    }
  }
  names(newnodes) <- sapply(newnodes,NodeName)
  newnodes
}


NetworkFootprint <- function(net) {
  allnodes <- NetworkAllNodes(net)
  result <- character()
  for (node in allnodes) {
    stubs <- sapply(NodeParents(node),NodeKind) == "Stub"
    if (any(stubs)) {
      result <- unique(c(result,NodeInputNames(node)[stubs]))
    }
  }
  result
}






