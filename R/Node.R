## Node.R
## Basic functions Relating to Nodes.

## Disrete and continuous info is stashed as an attribute, as it cannot change.

######################################################################
## NeticaNode class
######################################################################

NeticaNode <-
  setRefClass("NeticaNode",
              fields=c(Name="character",
                       Netica_Node="externalptr",
                       Net="NeticaBN",
                       discrete="logical"),
              methods=list(
                  initialize = function(Name=".Prototype",
                                        Net,discrete=TRUE,...){
                    node <- callSuper(Name=Name,Net=Net,
                                      Netica_Node=externalptr(),
                                      discrete=discrete,
                                     ...)
                    node
                  },
                  isActive = function() {
                    .Call("RN_isNodeActive",.self,PACKAGE=RNetica)
                  },
                  reportErrors = function(maxreport=9,clear=TRUE,
                                          call = sys.call(sys.parent())) {
                    Net$reportErrors(maxreport,clear,call)
                  },
                  signalErrors = function(maxreport=9,clear=TRUE,
                                          call = sys.call(sys.parent())) {
                    e <- reportErrors(maxreport, clear, call)
                    if (inherits(e,"error")) stop(e)
                    if (inherits(e,"warning")) warn(e)
                    if (inherits(e,"condition")) signalCondition(e)
                  },
                  clearErrors = function(severity="XXX_ERR") {
                    Net$clearErrors(severity)
                  },
                  deactivate = function() {
                    .Call("RN_DeactivateNode",.self,PACKAGE=RNetica)
                  },
                  show = function() {
                    cat(ifelse(discrete,"Discrete ","Continuous"),
                        "Netica Node named ",Name,"in network ",Net$Name,"\n")
                    if (isActive()) {
                      cat("  Node is currently active.\n")
                      if (discrete) {
                        cat("States are: ",
                            paste(NodeStates(.self),collapse=", "),
                            "\n")
                      }
                    } else {
                       cat("  Node is not currently active.\n")
                     }
                  }
              ))




is.discrete <- function (node) {
  if (is.NeticaNode(node)) {
    return (node$discrete)
  }
  return (NA)
}
is.continuous <- function (node) {
  if (is.NeticaNode(node)) {
    return (!node$discrete)
  }
  return (NA)
}


setMethod("toString","NeticaNode", function(x,...) {
  if (is.active(x))
    if (is.continuous(x)) {
      paste("<Continuous Node:",x$Name,">")
    } else {
      paste("<Discrete Node:",x$Name,">")
    }
  else
    paste("<Deleted Netica Node:",x$Name,">")
})

setMethod("print","NeticaNode",function(x, ...) {
  cat(toString(x),"\n")
})

setMethod("as.character", "NeticaNode", function(x, ...) {
  toString(x)
})

## Need this function so RStudio won't choke on Node objects.
str.NeticaNode <- function (object, ...) {
    object$show()
    invisible(object)
}


is.NeticaNode <- function (x) {
  is(x,"NeticaNode")
}

setMethod("Compare","NeticaNode", function(e1, e2) {
  ok <- switch(.Generic, "=="=0, "!=" =1, -1)
  if (ok<0) {
    warning(.Generic, " not implemented for Netica nodes.")
    return(NA)
  }
  truth <- (ok == 0)  ## inversts sign of truth for !=
  bothdeleted <- !is.active(e1) && !is.active(e2)
  if (is.na(bothdeleted)) return(!truth) ## At least one non-bn
  bothdeleted <- !is.active(e1) && !is.active(e2)
  if (is.na(bothdeleted)) return(!truth) ## At least one non-bn
  if (bothdeleted) {
    ## Both deleted, use cached names.
    return(ifelse(e1$Name==e2$Name & e1$Net==e2$Net,truth,!truth))
  }
  ## Okay have two valid NeticaNodes or one valid one and one inactive.
  ## Either way we can get by by comparing pointers.
  return(ifelse(identical(e1$Netica_Node,e2$Netica_Node),
                truth,!truth))
})

setMethod("is.element",c("NeticaNode","list"),
          function (el,set) is.element(list(el),set))

setMethod("is.active","NeticaNode",function(x) x$isActive())

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
  handles <- .Call("RN_NewContinuousNodes",net,names,PACKAGE=RNetica)
  if (length(handles)==1) handles <- handles[[1]]
  net$signalErrors()
  handles
}

NewDiscreteNode <- function (net, names, states = c("Yes","No")) {
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

  handles <- .Call("RN_NewDiscreteNodes",net,names,nslist,statelist,PACKAGE=RNetica)
  if (length(handles)==1) handles <- handles[[1]]
  net$signalErrors()
  handles
}

DeleteNodes <- function (nodes) {
  if (is.NeticaNode(nodes) && length(nodes) ==1) {
    nodes <- list(nodes)
  }
  net <- nodes[[1]]$Net
  if (any(!sapply(nodes,is.NeticaNode))) {
    stop("Expected a list of Netica nodes, got, ",nodes)
  }
  handles <- .Call("RN_Delete_Nodes",nodes,PACKAGE=RNetica)
  net$signalErrors()
  for (nd in handles) {
    if (is(nd,"NeticaNode")) {
      rm(list=nd$Name,envir=net$nodes)
    }
  }
  if (length(handles)==1) handles <- handles[[1]]
  invisible(handles)
}

## options should be one of no_links, or no_tables.
CopyNodes <- function (nodes, newnamelist=NULL, newnet=NULL,
                       options=character(0)) {
  if (is.NeticaNode(nodes) && length(nodes) ==1) nodes <-list(nodes)
  if (!all(sapply(nodes,is.NeticaNode))) {
    stop("Expected a list of Netica nodes, got, ",nodes)
  }
  options <- paste(options,collapse=",")
  if (is.null(newnet)) {
    newnet = NodeNet(nodes[[1]])
  }
  if (!is.NeticaBN(newnet) || !is.active(newnet)) {
    stop("Expected an active Bayes net, got ",newnet)
  }
  handles <- .Call("RN_Copy_Nodes",newnet, nodes, options,PACKAGE=RNetica)
  newnet$signalErrors()

  if(!is.null(newnamelist)) {
    ## New names supplied, rename.
    if (length(handles)!=length(newnamelist)) {
      stop("Number of new names doesn't match number of old nodes")
    }
    newnamelist <- as.character(newnamelist)
    for (n in 1:length(handles)) {
      if (!is.IDname(newnamelist[n])) {
        stop("String ",newnamelist[n]," is not a legal Netica Name.")
      }
      NodeName(handles[[n]]) <- newnamelist[n]
    }
  }

  if (length(handles)==1) {
    handles[[1]]
  } else {
    names(handles) <- sapply(handles,NodeName)
    handles
  }
}

#########################################################################
## Utility level node operations.
#########################################################################

NetworkFindNode <- function (net,name) {
  if (length(net)>1 || !is.NeticaBN(net) || !is.active(net)) {
    stop ("Expected an active Netica network, got ",net)
  }
  if (length(name) > 1) {
    result <- lapply(name,function(n) NetworkFindNode(net,n))
    names(result) <- name
    return (result)
  } else {
    if(!is.IDname(name)) {
      stop("Expected a Netica name, got ",name)
    }
    handle <- .Call("RN_Find_Node",net,name,PACKAGE=RNetica)
    net$signalErrors()
    handle
  }
}

NetworkAllNodes <- function(net) {
  if (length(net)>1 || !is.NeticaBN(net) || !is.active(net)) {
    stop ("Expected an active Netica network, got ",net)
  }
  handles <- .Call("RN_Network_AllNodes",net,PACKAGE=RNetica)
  net$signalErrors()
  names(handles) <- sapply(handles,NodeName)
  handles
}

NodeNet <- function(node, internal=FALSE) {
  if (length(node)>1 || !is.NeticaNode(node)) {
    stop ("Expected an active Netica node, got ",node)
  }
  if (!is.active(node)) return(NULL)
  if (internal) {
    handle <- .Call("RN_NodeNet",node,node$Net$Session,PACKAGE=RNetica)
    node$signalErrors()
    handle
  } else {
    node$Net
  }
}

NodeName <- function (node, internal=FALSE) {
  if (!is.NeticaNode(node) || !is.active(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  if (internal) {
    name <- .Call("RN_GetNodeName",node,PACKAGE=RNetica)
    node$signalErrors()
    name
  } else {
    node$Name
  }
}

"NodeName<-" <- function (node, value) {
  if (!is.NeticaNode(node) || !is.active(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  if (length(value)>1 || !is.IDname(value)) {
    stop("Illegal Netica Name, ",value)
  }
  oldname <- NodeName(node)
  if (oldname==value) {
    mes <- simpleMessage(paste("Node is already named ",oldname,"skipping"),
                         call = sys.call())
    signalCondition(mes)
    return(node)
  }
  net <- node$Net
  handle <- .Call("RN_SetNodeName",node,value,PACKAGE=RNetica)
  node$signalErrors()
  handle$Name <- value
  rm(list=oldname,envir=net$nodes)
  net$nodes[[value]] <- handle
  handle
}

################################################################
## Getters and Setters for High Level Net properities
################################################################

NodeTitle <- function (node) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  title <- .Call("RN_GetNodeTitle",node,PACKAGE=RNetica)
  node$signalErrors()
  title
}

"NodeTitle<-" <- function (node, value) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  if (length(value)>1) {
    warning("Only first element used as title.")
  }
  value <- as.character(value)

  handle <- .Call("RN_SetNodeTitle",node,value,PACKAGE=RNetica)
  node$signalErrors()
  invisible(handle)
}

NodeDescription <- function (node) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  description <- .Call("RN_GetNodeComment",node,PACKAGE=RNetica)
  node$signalErrors()
  description
}

"NodeDescription<-" <- function (node, value) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  value <- as.character(value)
  if (any(is.na(value))) {
    stop("Non-character titles in ", value)
  }
  value <- paste(value,collapse="\n")
  handle <- .Call("RN_SetNodeComment",node,value,PACKAGE=RNetica)
  node$signalErrors()
  invisible(handle)
}


NodeUserField <- function (node, fieldname) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  if (length(fieldname)>1 || !is.IDname(fieldname)) {
    stop("Illegal Netica Field Name, ",fieldname)
  }
  value <- .Call("RN_GetNodeUserField",node,fieldname,PACKAGE=RNetica)
  node$signalErrors()
  if (!is.na(value) && value=="NA_character_") value <- NA_character_
  value
}

"NodeUserField<-" <- function (node, fieldname, value) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  if (length(fieldname)>1 || !is.IDname(fieldname)) {
    stop("Illegal Netica Field Name, ",fieldname)
  }
  value <- as.character(value)
  if (is.na(value)) value <- "NA_character_"
  if (length(value)>1 || is.na(value)) {
    stop("Illegal field value.")
  }
  handle <- .Call("RN_SetNodeUserField",node,fieldname,value,PACKAGE=RNetica)
  node$signalErrors()
  handle
}

NodeAllUserFields <- function (node) {
  if (!is.NeticaNode(node)) {
    stop("Expected a Netica node, got, ",node)
  }
  values <- .Call("RN_GetAllNodeUserFields",node,PACKAGE=RNetica)
  node$signalErrors()
  values
}


NodeUserObj <- function (node, fieldname) {
  str <- NodeUserField(node,fieldname)
  if (is.na(str)) return(NULL)
  dgetFromString(str)
}

"NodeUserObj<-" <- function (node, fieldname, value) {
  sval <- dputToString(value)
  ## Sometimes R "helpfully" breaks this into multiple lines.
  if (length(sval) > 1)
    sval <- paste(sval,collapse=" ")
  NodeUserField(node,fieldname) <- sval
  node
}


NodeKind <- function (node) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  kind <- .Call("RN_GetNodeKind",node,PACKAGE=RNetica)
  node$signalErrors()
  kind
}

"NodeKind<-" <- function (node, value) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  value <- as.character(value)
  if (any(is.na(value))) {
    stop("Non-character kinds in ", value)
  }
  if (length(value) >1) {
    warning("Value has length greater than 1, only first value used.")
  }
  handle <- .Call("RN_SetNodeKind",node,value,PACKAGE=RNetica)
  node$signalErrors()
  invisible(handle)
}

NodeVisStyle <- function (node) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  style <- .Call("RN_GetNodeVisStyle",node,PACKAGE=RNetica)
  node$signalErrors()
  style
}

"NodeVisStyle<-" <- function (node, value) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  value <- as.character(value)
  if (any(is.na(value))) {
    stop("Non-character styles in ", value)
  }
  if (length(value) >1) {
    warning("Value has length greater than 1, only first value used.")
  }
  handle <- .Call("RN_SetNodeVisStyle",node,value,PACKAGE=RNetica)
  node$signalErrors()
  invisible(handle)
}

NodeVisPos <- function (node) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  pos <- .Call("RN_GetNodeVisPos",node,PACKAGE=RNetica)
  node$signalErrors()
  pos
}

"NodeVisPos<-" <- function (node, value) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  value <- as.numeric(value)
  if (length(value) >2 || any(is.na(value))) {
    stop("Expected a vector of 2 numbers, got", value)
  }
  handle <- .Call("RN_SetNodeVisPos",node,value,PACKAGE=RNetica)
  node$signalErrors()
  invisible(handle)
}

NodeNumStates <- function (node) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  nstates <- .Call("RN_GetNodeNumStates",node,PACKAGE=RNetica)
  node$signalErrors()
  nstates
}


NodeStates <- function (node) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  states <- .Call("RN_GetNodeStates",node,PACKAGE=RNetica)
  node$signalErrors()
  states
}

"NodeStates<-" <- function (node, resize=FALSE, value) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  value <- as.character(value)
  if (any(is.na(value)) || any(!is.IDname(value))) {
    stop("Illegal state names: ", value)
  }
  if (!resize && length(value) != NodeNumStates(node)) {
     stop("Expected exactly ", NodeNumStates(node), " states, or resize=TRUE.")
  }
  handle <- .Call("RN_SetNodeStates",node, paste(value,collapse=","),
                  length(value),PACKAGE=RNetica)
  node$signalErrors()
  invisible(handle)
}

NodeStateTitles <- function (node) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  titles <- .Call("RN_GetNodeStateTitles",node,PACKAGE=RNetica)
  node$signalErrors()
  titles
}

"NodeStateTitles<-" <- function (node, value) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  value <- as.character(value)
  if (any(is.na(value))) {
    stop("Illegal state names: ", value)
  }
  if (length(value) != NodeNumStates(node)) {
     stop("Expected exactly ", NodeNumStates(node), " titles")
  }
  handle <- .Call("RN_SetNodeStateTitles",node, value,PACKAGE=RNetica)
  node$signalErrors()
  invisible(handle)
}


NodeStateComments <- function (node) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  comments <- .Call("RN_GetNodeStateComments",node,PACKAGE=RNetica)
  node$signalErrors()
  comments
}

"NodeStateComments<-" <- function (node, value) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  value <- as.character(value)
  if (any(is.na(value))) {
    stop("Illegal state names: ", value)
  }
  if (length(value) != NodeNumStates(node)) {
     stop("Expected exactly ", NodeNumStates(node), " comments")
  }
  handle <- .Call("RN_SetNodeStateComments",node, value,PACKAGE=RNetica)
  node$signalErrors()
  invisible(handle)
}

NodeLevels <- function (node) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  if (is.discrete(node)) {
    ## Returns a named vector of length NodeNumStates(node)
    levels <- .Call("RN_GetNodeLevelsDiscrete",node,PACKAGE=RNetica)
  } else {
    ## Returns an unnamed vector of length NodeNumStates(node)+1
    levels <- .Call("RN_GetNodeLevelsContinuous",node,PACKAGE=RNetica)
  }
  node$signalErrors()
  levels
}

"NodeLevels<-" <- function (node, value) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  value <- as.numeric(value)
  if (any(is.na(value))) {
    stop("Illegal levels: ", paste(value,collapse=", "))
  }
  ## Different rules for error checking in the two cases.
  if (is.discrete(node)) {
    if (length(value) != NodeNumStates(node)) {
      stop("Expected exactly ", NodeNumStates(node), " levels")
    }
  } else {
    if (is.unsorted(value) && is.unsorted(rev(value))) {
      stop("Level cut points must be in increasing or decreasing order.")
    }
  }
  handle <- .Call("RN_SetNodeLevels",node, value,PACKAGE=RNetica)
  node$signalErrors()
  invisible(handle)
}


###########################################################################
##  Node Sets
###########################################################################

NetworkNodeSets <- function(net, incSystem=FALSE) {
  if (!is.NeticaBN(net) || !is.active(net)) {
    stop("Expected an active Netica network, got, ",net)
  }
  incSystem <- as.logical(incSystem)[1]
  sets <- .Call("RN_NetworkNodeSets",net,incSystem, PACKAGE=RNetica)
  net$signalErrors()
  sets
}

NodeSets <- function(node, incSystem=FALSE) {
  if (!is.NeticaNode(node) || !is.active(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  incSystem <- as.logical(incSystem)[1]
  sets <- .Call("RN_GetNodeSets",node, incSystem, PACKAGE=RNetica)
  node$signalErrors()
  ## Strip out zero length strings.
  sets[nchar(sets)>0]
}

"NodeSets<-" <- function(node, value) {
  if (!is.NeticaNode(node) || !is.active(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  value <- as.character(value)
  if (length(grep(",",value)) > 0) {
    stop("Commas not allowed in node set names.")
  }
  sysvals <- grep("^:",value)
  if (length(sysvals) >0) {
    warning("Ignoring node sets  begining with ':' (reserved for Netica use).")
    value <- value[-sysvals]
  }
  .Call("RN_SetNodeSets",node,value,PACKAGE=RNetica)
  node$signalErrors()
  invisible(node)
}

AddNodeToSets <- function (node,sets) {
  NodeSets(node) <- union(NodeSets(node),sets)
  invisible(node)
}

RemoveNodeFromSets <- function (node,sets) {
  NodeSets(node) <- setdiff(NodeSets(node),sets)
  invisible(node)
}


NetworkNodesInSet <- function(net, setname) {
  if (!is.NeticaBN(net) || !is.active(net)) {
    stop("Expected an active Netica network, got, ",net)
  }
  if (length(setname)>1) {
    warning("NetworkNodeSets: Ignoring all but first set name.")
  }
  setname <- as.character(setname)[1]
  nodes <- .Call("RN_NetworkNodesInSet",net,setname, PACKAGE=RNetica)
  net$signalErrors()
  nodes <- as.list(nodes)
  names(nodes) <- sapply(nodes,NodeName)
  nodes
}

"NetworkNodesInSet<-" <- function(net, setname, value) {
  oldset <- NetworkNodesInSet(net,setname)
  oldsetNames <- names(oldset)
  newsetNames <- sapply(value,NodeName)
  ## setdiff doesn't work with objects, so need to do this the hard way
  for (nd in oldset) {
    if (!(NodeName(nd) %in% newsetNames))
      RemoveNodeFromSets(nd,setname)
  }
  for (nd in value) {
    if (!(NodeName(nd) %in% oldsetNames))
      AddNodeToSets(nd,setname)
  }
  invisible(net)
}

NetworkSetPriority <- function(net, setlist) {
  if (!is.NeticaBN(net) || !is.active(net)) {
    stop("Expected an active Netica network, got, ",net)
  }
  setlist <- as.character(setlist)
  .Call("RN_NetworkSetPriority",net,paste(setlist,collapse=","),
        PACKAGE=RNetica)
  net$signalErrors()
  invisible(net)
}

NetworkNodeSetColor <- function(net, setname, newcolor) {
  if (!is.NeticaBN(net) || !is.active(net)) {
    stop("Expected an active Netica network, got, ",net)
  }
  if (length(setname)>1) {
    warning("NetworkNodeSetColor: Ignoring all but first set name.")
  }
  setname <- as.character(setname)[1]
  if (missing(newcolor)) {
    ## No replacement, must be a request
    result <- .Call("RN_NetworkNodeGetColor",net,setname, PACKAGE=RNetica)
  } else {
    if (length(newcolor)>1) {
      warning("NetworkNodeSetColor: Ignoring all but first color.")
    }
    if (is.na(newcolor)) {
      col <- -2L
    } else {
      col <- as.numeric(col2rgb(newcolor))
      col <- as.integer(sum(col*256^(2:0)))
    }
    result <- .Call("RN_NetworkNodeSetColor",net,setname, col,
                    PACKAGE=RNetica)
  }
  net$signalErrors()
  if (result==-2) {
    result <- NA_character_
  } else {
    result <- paste("#",as.hexmode(result),sep="")
  }
  result
}


str.NeticaNode <- function(object, ...) {
  ## Main function is to put this in a list,
  ## this will make notes not accessable in RStudio Environment tab,
  ## but it will suppress the warning.
  str(toString(object),...)
}
