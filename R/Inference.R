##
## Inferences.R -- These functions are related to calculating beliefs
## after entering findigns.   

#####################################################################
## Network Level
####################################################################

CompileNetwork <- function (net) {
  if (!is.NeticaBN(net) || !is.active(net)) {
    stop("Expected an active Netica network, got, ",net)
  }
  .Call("RN_CompileNet",net)
    ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("CompileNetwork: Netica Errors Encountered, see console for details.")
  }
  invisible(net)
}

UncompileNetwork <- function (net) {
  if (!is.NeticaBN(net) || !is.active(net)) {
    stop("Expected an active Netica network, got, ",net)
  }
  .Call("RN_UncompileNet",net)
    ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("UncompileNetwork: Netica Errors Encountered, see console for details.")
  }
  invisible(net)
}

RetractNetFindings <-function (net) {
  if (!is.NeticaBN(net) || !is.active(net)) {
    stop("Expected an active Netica network, got, ",net)
  }
  .Call("RN_RetractNetFindings",net)
    ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("RetractNetFindings: Netica Errors Encountered, see console for details.")
  }
  invisible(net)
}

NodeFinding <- function (node) {
  if (length(node)>1 || !is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  result <- .Call("RN_GetNodeFinding",node)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("NodeFinding: Netica Errors Encountered, see console for details.")
  }
  if (is.numeric(result) && !is.na(result)) {
    result <- NodeStates(node)[result+1] ## Convert to name.
  }
  result
}

"NodeFinding<-" <- function (node,value) {
  if (length(node)>1 || !is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  if (length(value) >1) {
    stop("Node must be set to a single value.")
  }
  val <- value
  if (is.character(val)) {
    val <- match(val,NodeStates(node))
  }
  val <- as.integer(val) -1L
  if (is.na(val)) {
    stop("Value ", value, " not legal for node ",node)
  }
  handle <- .Call("RN_SetNodeFinding",node,val)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("NodeFinding: Netica Errors Encountered, see console for details.")
  }
  invisible(handle)
}


EnterNegativeFinding <- function(node, eliminatedVals) {
  if (length(node)>1 || !is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  if (length(eliminatedVals) >= NodeNumStates(node)) {
    stop("Can't eliminate all of the ",NodeNumStates(node),
  " possible states.")
  }
  val <- eliminatedVals
  if (is.character(val)) {
    val <- match(val,NodeStates(node))
  }
  if (!is.numeric(val) || any(is.na(val))) {
    stop("Values ", eliminatedValues, " not legal for node ",node)
  }
  val <- as.integer(val) - 1L
  handle <- .Call("RN_SetNodeFindingNot",node,val)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("EnterNegativeFinding: Netica Errors Encountered, see console for details.")
  }
  invisible(handle)
}

RetractNodeFinding <- function (node) {
  if (length(node)>1 || !is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  handle <- .Call("RN_RetractNodeFinding",node)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("RetractNodeFinding: Netica Errors Encountered, see console for details.")
  }
  invisible(handle)
}

IsBeliefUpdated <- function (node) {
  if (length(node)>1 || !is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  handle <- .Call("RN_IsBeliefUpdated",node)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("IsBeliefUpdated: Netica Errors Encountered, see console for details.")
  }
  handle
}

NodeBeliefs <- function (node) {
  if (!is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  result <- .Call("RN_GetNodeBeliefs",node)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("GetNodeBeliefs: Netica Errors Encountered, see console for details.")
  }
  names(result) <- NodeStates(node)
  result
}

NodeLikelihood <- function (node) {
  if (!is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  result <- .Call("RN_GetNodeLikelihood",node)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("GetNodeLikelihood: Netica Errors Encountered, see console for details.")
  }
  names(result) <- NodeStates(node)
  result
}

"NodeLikelihood<-" <- function (node, value) {
  if (!is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  if (!is.numeric(value) || length(value) != NodeNumStates(node)) {
    stop("Expected numeric vector of length ",NodeNumStates(node))
  }
  if (any(value>1) || any(value <0) || sum(value)==0) {
    stop("Expected values between 0 and 1 with at least one positive value.")
  }
  handle <- .Call("RN_SetNodeLikelihood",node,value)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("GetNodeLikelihood: Netica Errors Encountered, see console for details.")
  }
  invisible(handle)
}


