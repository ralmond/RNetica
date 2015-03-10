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
  .Call("RN_CompileNet",net,PACKAGE="RNetica")
    ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  invisible(net)
}

UncompileNetwork <- function (net) {
  if (!is.NeticaBN(net) || !is.active(net)) {
    stop("Expected an active Netica network, got, ",net)
  }
  .Call("RN_UncompileNet",net,PACKAGE="RNetica")
    ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  invisible(net)
}

RetractNetFindings <-function (net) {
  if (!is.NeticaBN(net) || !is.active(net)) {
    stop("Expected an active Netica network, got, ",net)
  }
  .Call("RN_RetractNetFindings",net,PACKAGE="RNetica")
    ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  invisible(net)
}

NodeFinding <- function (node) {
  if (length(node)>1 || !is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  result <- .Call("RN_GetNodeFinding",node,PACKAGE="RNetica")
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
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
  handle <- .Call("RN_SetNodeFinding",node,val,PACKAGE="RNetica")
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
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
    stop("Values ", eliminatedVals, " not legal for node ",node)
  }
  val <- as.integer(val) - 1L
  handle <- .Call("RN_SetNodeFindingNot",node,val,PACKAGE="RNetica")
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  invisible(handle)
}

RetractNodeFinding <- function (node) {
  if (length(node)>1 || !is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  handle <- .Call("RN_RetractNodeFinding",node,PACKAGE="RNetica")
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  invisible(handle)
}

EnterFindings <- function(net,findings) {
  if (is.null(names(findings))) {
    stop("Findings need names to indicate which nodes to set.")
  }
  WithoutAutoUpdate(net,
    for (i in 1:length(findings)) {
      node <- NetworkFindNode(net,names(findings)[i])
      if (is.null(node)) {
        warning("No node found for name ",names(findings)[i])
      } else {
        NodeFinding(node) <- findings[i]
      }
    })
  invisible(net)
}

IsBeliefUpdated <- function (node) {
  if (length(node)>1 || !is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  handle <- .Call("RN_IsBeliefUpdated",node,PACKAGE="RNetica")
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  handle
}

NodeBeliefs <- function (node) {
  if (!is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  result <- .Call("RN_GetNodeBeliefs",node,PACKAGE="RNetica")
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  names(result) <- NodeStates(node)
  result
}

NodeLikelihood <- function (node) {
  if (!is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  result <- .Call("RN_GetNodeLikelihood",node,PACKAGE="RNetica")
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
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
  handle <- .Call("RN_SetNodeLikelihood",node,value,PACKAGE="RNetica")
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  invisible(handle)
}


JointProbability <- function(nodelist) {
  if (any(!sapply(nodelist,function (nd) {is.NeticaNode(nd) &&
                                          is.active(nd)}))) {
    stop("Expected a list of Netica nodes, got, ",nodelist)
  }
  result <- .Call("RN_JointProbability",nodelist,PACKAGE="RNetica")
  ecount <- ReportErrors()
  if (ecount[1L]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  dnames <- lapply(nodelist,NodeStates)
  names(dnames) <- sapply(nodelist,NodeName)
  dimnames(result) <- dnames
  result
}

FindingsProbability <- function(net) {
  if (!is.NeticaBN(net) || !is.active(net)) {
    stop("Expected an active Netica network, got, ",net)
  }
  result <- .Call("RN_FindingsProbability",net,PACKAGE="RNetica")
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  result
}

MostProbableConfig <- function(net, nth=0) {
  if (!is.NeticaBN(net) || !is.active(net)) {
    stop("Expected a list of Netica nodes, got, ",nodelist)
  }
  config <- .Call("RN_MostProbableConfig",net,as.integer(nth),PACKAGE="RNetica")
  ecount <- ReportErrors()
  if (ecount[1L]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  nodelist <- NetworkAllNodes(net)
  result <- vector(mode="list",length=length(nodelist))
  for (i in 1:length(nodelist)) {
    result[[i]] <- factor(config[i],NodeStates(nodelist[[i]]))
  }
  names(result) <- sapply(nodelist,NodeName)
  as.data.frame(result,row.names="Most Probable")
}

JunctionTreeReport <- function (net) {
  if (!is.NeticaBN(net) || !is.active(net)) {
    stop("Expected an active Netica network, got, ",net)
  }
  report <- .Call("RN_JunctionTreeReport",net,PACKAGE="RNetica")
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  ##TODO Parse the report into a table.
  report
}

NetworkCompiledSize <- function(net) {
  if (!is.NeticaBN(net) || !is.active(net)) {
    stop("Expected an active Netica network, got, ",net)
  }
  result <- .Call("RN_SizeCompiledNetwork",net,PACKAGE="RNetica")
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  result
}

## Currently, I'm abusing the function SizeCompiledNetwork, which
## raises an error but sesnsibly returns a negative value if the net
## is not compiled.
is.NetworkCompiled <- function(net) {
  if (!is.NeticaBN(net) || !is.active(net)) {
    stop("Expected an active Netica network, got, ",net)
  }
  result <- .Call("RN_SizeCompiledNetwork",net,PACKAGE="RNetica")
  ClearAllErrors("ERROR_ERR")
  result > 0
}



EliminationOrder <- function (net) {
  if (!is.NeticaBN(net) || !is.active(net)) {
    stop("Expected an active Netica network, got, ",net)
  }
  result <- .Call("RN_GetEliminationOrder",net,PACKAGE="RNetica")
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  result
}

"EliminationOrder<-" <- function (net, value) {
  if (!is.NeticaBN(net) || !is.active(net)) {
    stop("Expected an active Netica network, got, ",net)
  }
  if (is.null(value) || length(value)==0) {
    value <- NULL
  } else {
    if (length(value) != length(NetworkAllNodes(net))) {
      stop("All nodes must be included in network order.")
    }
    if (!all(sapply(value,is.active))) {
      stop("All elements of value must be active nodes.")
    }
  }
  handle <- .Call("RN_SetEliminationOrder",net,value, PACKAGE="RNetica")
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  invisible(handle)
}
