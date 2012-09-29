##
## Edges.R -- These functions are related to links between nodes and
## conditional probability tables.


AddLink <- function (parent, child) {
  if (length(parent)>1 || !is.NeticaNode(parent) || !is.active(parent)) {
    stop ("Parent is not an active Netica node", parent)
  }
  if (length(child)>1 || !is.NeticaNode(child) || !is.active(child)) {
    stop ("Child is not an active Netica node", child)
  }
  handle <- .Call("RN_AddLink",parent,child)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("AddLink: Netica Errors Encountered, see console for details.")
  }
  invisible(handle)
}

DeleteLink <- function (parent, child) {
  if (length(parent)>1 || !is.NeticaNode(parent) || !is.active(parent)) {
    stop ("Parent is not an active Netica node", parent)
  }
  if (length(child)>1 || !is.NeticaNode(child) || !is.active(child)) {
    stop ("Child is not an active Netica node", child)
  }
  handle <- .Call("RN_DeleteLink",parent,child)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("DeleteLink: Netica Errors Encountered, see console for details.")
  }
  invisible(handle)
}

ReverseLink <- function (parent, child) {
  if (length(parent)>1 || !is.NeticaNode(parent) || !is.active(parent)) {
    stop ("Parent is not an active Netica node", parent)
  }
  if (length(child)>1 || !is.NeticaNode(child) || !is.active(child)) {
    stop ("Child is not an active Netica node", child)
  }
  handle <- .Call("RN_ReverseLink",parent,child)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("ReverseLink: Netica Errors Encountered, see console for details.")
  }
  invisible(handle)
}

NodeChildren <- function (parent) {
  if (length(parent)>1 || !is.NeticaNode(parent) || !is.active(parent)) {
    stop ("Parent is not an active Netica node", parent)
  }
  handle <- .Call("RN_GetNodeChildren",parent)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("NodeChildren: Netica Errors Encountered, see console for details.")
  }
  handle
}


NodeParents <- function (child) {
  if (length(child)>1 || !is.NeticaNode(child) || !is.active(child)) {
    stop ("Child is not an active Netica node", child)
  }
  handle <- .Call("RN_GetNodeParents",child)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("NodeParents: Netica Errors Encountered, see console for details.")
  }
  handle
}

"NodeParents<-" <- function (child, value) {
  if (length(child)>1 || !is.NeticaNode(child) || !is.active(child)) {
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
  value <- lapply(value, function(x) if(is.list(x)) x[[1]] else x)
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
  newindex <- 1:length(value)
  oldindex <- match(value, oldpar)
  if( any(newindex<oldindex,na.rm=TRUE)) {
    cat("Reorder nulling. \n")
    ##Okay, safest thing to do is to set to a vector of NULLs first,
    ##Then add new links.
    oldnames <- names(oldpar)
    oldpar <- vector("list",length(oldpar))
    .Call("RN_SetNodeParents",child, oldpar)
    ## This sets names to old parent names, which is probabily not
    ## what was wanted, so clear names.
    .Call("RN_SetNodeInputNames",child, oldnames)
  }
  
  handle <- .Call("RN_SetNodeParents",child, value)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("NodeParents: Netica Errors Encountered, see console for details.")
  }
  invisible(handle)
}

NodeInputNames <- function (node) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  names <- .Call("RN_GetNodeInputNames",node)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("NodeInputNamess: Netica Errors Encountered, see console for details.")
  }
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
  handle <- .Call("RN_SetNodeInputNames",node, value);
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("NodeInputNames: Netica Errors Encountered, see console for details.")
  }
  invisible(handle)
}

AbsorbNodes <- function (nodes) {
  if (is.NeticaNode(nodes) && length(nodes) ==1) {
    nodes <- list(nodes)
  }
  if (any(!sapply(nodes,is.NeticaNode))) {
    stop("Expected a list of Netica nodes, got, ",nodes)
  }
  handles <- .Call("RN_AbsorbNodes",nodes)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("AbsorbNodes: Netica Errors Encountered, see console for details.")
  }
  if (length(handles)==1) handles <- handles[[1]]
  invisible(handles)
}



is.NodeRelated <- function (node1, node2, relation="connected") {
  if (length(node1)>1 || !is.NeticaNode(node1) || !is.active(node1)) {
    stop ("Node1 is not an active Netica node", node1)
  }
  if (length(node2)>1 || !is.NeticaNode(node2) || !is.active(node2)) {
    stop ("Node2 is not an active Netica node", node2)
  }
  relation <- as.character(relation)
  if (is.na(relation[1])) {
    stop("Bad relation.")
  }
  if (length(relation) >1) {
    warning("Relation has length > 1, only first value is used.")
  }
  handle <- .Call("RN_IsNodeRelated",node1,relation,node2)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("IsNodeRelated: Netica Errors Encountered, see console for details.")
  }
  handle
}

GetRelatedNodes <- function (nodelist, relation="connected") {
  if (is.NeticaNode(nodelist) && length(nodelist) ==1) {
    nodelist <- list(nodelist)
  }
  if (any(!sapply(nodelist,is.NeticaNode))) {
    stop("Expected a list of Netica nodes, got, ",nodelist)
  }
  relation <- as.character(relation)
  if (is.na(relation[1])) {
    stop("Bad relation.")
  }
  if (length(relation) >1) {
    warning("Relation has length > 1, only first value is used.")
  }
  handle <- .Call("RN_GetRelatedNodes",nodelist,relation)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("IsNodeRelated: Netica Errors Encountered, see console for details.")
  }
  handle
}
