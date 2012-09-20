## Node.R
## Basic functions Relating to Nodes.

## Disrete and continuous info is stashed as an attribute, as it cannot change.

######################################################################
## NeticaNode class
######################################################################

is.discrete <- function (node) {
  if (is.NeticaNode(node)) {
    return (attr(node,"node_discrete"))
  }
  return (NA)
}
is.continuous <- function (node) {
  if (is.NeticaNode(node)) {
    return (!attr(node,"node_discrete"))
  }
  return (NA)
}


toString.NeticaNode <- function(x,...) {
  if (is.active(x))
    if (is.continuous(x)) {
      paste("<Continuous Node:",as.character(x),">")
    } else {
      paste("<Discrete Node:",as.character(x),">")
    }
  else 
    paste("<Deleted Netica Node:",as.character(x),">")
}
  
print.NeticaNode <- function(x, ...) {
  cat(toString(x),"\n")
}

is.NeticaNode <- function (x) {
  is(x,"NeticaNode")
}

Ops.NeticaNode <- function(e1, e2) {
  ok <- switch(.Generic, "=="=0, "!=" =1, -1)
  if (ok<0) {
    warning(.Generic, " not implemented for Netica networks.")
    return(NA)
  }
  truth <- (ok == 0)  ## inversts sign of truth for !=
  bothdeleted <- !is.active(e1) && !is.active(e2)
  if (is.na(bothdeleted)) return(!truth) ## At least one non-bn
  if (bothdeleted) {
    ## Both deleted, use cached names.
    return(ifelse(as.character(e1)==as.character(e2),truth,!truth))
  }
  ## Okay have two valid NeticaNodes or one valid one and one inactive.
  ## Either way we can get by by comparing pointers.
  return(ifelse(identical(attr(e1,"Netica_node"),attr(e2,"Netica_node")),
                truth,!truth))
}

########################################################################
## Creation and Destruction
#######################################################################

NewContinuousNode <- function (net, names) {
  if (!is.NeticaBN(net) || !is.active(net)) {
    stop("Expected an active Bayes net, got ",net)
  }
  if (!is.character(names) || length(names) == 0) {
    stop("Node names not supplied.")
  }
  goodNames <- is.IDname(names)
  if (any(!goodNames)) {
    stop("Illegal Netica Names, ",names[!goodNames])
  }
  handles <- .Call("RN_NewContinuousNodes",net,names)
  if (length(handles)==1) handles <- handles[[1]]
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("NewContinuousNodes: Netica Errors Encountered, see console for details.")
  }
  handles
}

NewDiscreteNode <- function (net, names, states) {
  if (!is.NeticaBN(net) || !is.active(net)) {
    stop("Expected an active Bayes net, got ",net)
  }
  if (!is.character(names) || length(names) == 0) {
    stop("Node names not supplied.")
  }
  goodNames <- is.IDname(names)
  if (any(!goodNames)) {
    stop("Illegal Netica Names, ",names[!goodNames])
  }
  nslist <- -1
  statelist <- NA
  if (is.character(states)) {
    ## We got a single state list, so all nodes are going to have
    ## identical states.
    nslist = rep(length(states),length(names)) 
    statelist = rep(paste(states,collapse=", "),length(names))
  } else if (is.list(states)) {
    ## We got a vector, different set of states for each node.
    if (length(states) != length(names)) {
      stop("Name list and state list are of different lengths.")
    }
    nslist <- sapply(states,length)
    statelist <- sapply(states,paste,collapse=", ")
  } else {
    stop("Unexpected object for states")
  }
    
  handles <- .Call("RN_NewDiscreteNodes",net,names,nslist,statelist)
  if (length(handles)==1) handles <- handles[[1]]
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("NewContinuousNodes: Netica Errors Encountered, see console for details.")
  }
  handles
}


DeleteNodes <- function (nodes) {
  if (is.NeticaNode(nodes) && length(nodes) ==1) {
    nodes <- list(nodes)
  }
  if (any(!sapply(nodes,is.NeticaNode))) {
    stop("Expected a list of Netica nodes, got, ",nodes)
  }
  handles <- .Call("RN_Delete_Nodes",nodes)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("DeleteNodes: Netica Errors Encountered, see console for details.")
  }
  if (length(handles)==1) handles <- handles[[1]]
  invisible(handles)
}

## options should be one of no_links, or no_tables.
CopyNodes <- function (nodes, newnamelist=NULL, newnet=NULL,
                       options=character(0)) {
  if (is.NeticaNode(node) && length(node) ==1) nodes <-list(nodes)
  if (!all(sapply(nets,is.NeticaNode))) {
    stop("Expected a list of Netica nodes, got, ",nets)
  }
  options <- paste(options,collapse=",")
  if (is.null(newnet)) {
    newnet = NodeNet(nodes[[1]])
  }
  if (!is.NeticaBN(newnet) || !is.active(newnet)) {
    stop("Expected an active Bayes net, got ",newnet)
  }
  handles <- .Call("RN_Copy_Nets",newnet, nodes, newnamelist,options)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("CopyNetworks: Netica Errors Encountered, see console for details.")
  }

  if(!is.null(newnamelist)) {
    ## New names supplied, rename.
    if (length(handles)!=length(newnamelist)) {
      stop("Number of new names doesn't match number of old nodes")
    }
    newnamelist <- as.character(newnamelist)
    for (n in 1:length(handles)) {
      if (!is.IDname(newnamelist[i])) {
        stop("String ",newnamelist[i]," is not a legal Netica Name.")
      }
      NodeName(handles[[n]]) <- newnamelist[n]
    }
  }

  if (length(handles)==1) handles <- handles[[1]]
  handles
}

#########################################################################
## Utility level not operations.
#########################################################################

NetworkFindNode <- function (net,name) {
  if (length(net)>1 || !is.NeticaBN(net) || !is.active(net)) {
    stop ("Expected an active Netica network, got ",net)
  }
  if (length(name) > 0) {
    return (lapply(name,function(n) FindNode(net,n)))
  } else {
    if(!is.IDname(name)) {
      stop("Expected a Netica name, got ",name)
    }
    handle <- .Call("RN_Find_Node",net,name)
    ecount <- ReportErrors()
    if (ecount[1]>0) {
      stop("FindNode: Netica Errors Encountered, see console for details.")
    }
    handle
  }
}
    
NetworkAllNodes <- function(net) {
  if (length(net)>1 || !is.NeticaBN(net) || !is.active(net)) {
    stop ("Expected an active Netica network, got ",net)
  }
  handles <- .Call("RN_Netwok_AllNodes",net)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("NetworkAllNodes: Netica Errors Encountered, see console for details.")
  }
  handles
}
  
NodeNet <- function(node) {
  if (length(node)>1 || !is.NeticaNode(node) || !is.active(node)) {
    stop ("Expected an active Netica node, got ",node)
  }
  handle <- .Call("RN_NodeNet",node)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("NodeNet: Netica Errors Encountered, see console for details.")
  }
  handle
}
  
NodeName <- function (node) {
  if (!is.NeticaNode(node) || !is.active(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  name <- .Call("RN_GetNodeName",node)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("NodeName: Netica Errors Encountered, see console for details.")
  }
  name
}

"NodeName<-" <- function (node, value) {
  if (!is.NeticaNode(node) || !is.active(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  if (length(value)>1 || !is.IDname(value)) {
    stop("Illegal Netica Name, ",value)
  }
  handle <- .Call("RN_SetNodeName",node,value)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("SetNodeName: Netica Errors Encountered, see console for details.")
  }
  handle
}

################################################################
## Getters and Setters for High Level Net properities
################################################################
