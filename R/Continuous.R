##
## Inferences.R -- These functions are related to continuous nodes an
## multual information.

## This is a real value as opposed to the node finding
NodeValue <- function (node) {
  if (length(node)>1 || !is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  result <- .Call("RN_GetNodeValue",node,PACKAGE=RNetica)
  node$signalErrors()
  result
}

"NodeValue<-" <- function (node,value) {
  if (length(node)>1 || !is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  if (length(value) >1) {
    stop("Node must be set to a single value.")
  }
  val <- as.numeric(value)
  if (is.character(val) || is.na(val)) {
    stop("Value ", value, " not legal for node ",node)
  }
  handle <- .Call("RN_SetNodeValue",node,val,PACKAGE=RNetica)
  node$signalErrors()
  invisible(handle)
}

"EnterGaussianFinding" <- function (node,mean,sem,retractFirst=TRUE) {
  if (length(node)>1 || !is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  if (length(mean) >1 || length(sem)>1) {
    stop("Mean and sd must be scalars.")
  }
  if (is.character(mean) || is.na(mean)) {
    stop("Mean value ", mean, " is not numeric.")
  }
  if (is.character(sem) || is.na(sem)) {
    stop("SEM value ", sem, " is not numeric.")
  }
  resetFirst <- as.logical(retractFirst)
  ## Netica bug, calculate likelihood manually.
  ## handle <- .Call("RN_SetNodeGaussian",node,mean,sem,resetFirst,PACKAGE=RNetica)
  ## node$signalErrors()
  ## invisible(handle)
  lik <- diff(pnorm(NodeLevels(node),mean,sem))
  if (resetFirst) RetractNodeFinding(node)
  NodeLikelihood(node) <- lik
}

"EnterIntervalFinding" <- function (node,low,high,retractFirst=TRUE) {
  if (length(node)>1 || !is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  if (length(low) >1 || length(high)>1) {
    stop("Low and high must be scalars.")
  }
  hv <- high
  lv <- low
  if (is.discrete(node)) {
    if (is.character(hv)) {
      hv <- match(hv,NodeStates(node))
    }
    hv <- as.integer(hv) -1L
    if (is.na(hv)) {
      stop("High value ", high, " not legal for node ",node)
    }
    if (is.character(lv)) {
      lv <- match(lv,NodeStates(node))
    }
    lv <- as.integer(lv) -1L
    if (is.na(lv)) {
      stop("Low value ", low, " not legal for node ",node)
    }
  } else { ## Continuous node
    if (is.character(low) || is.na(low)) {
      stop("Low value ", low, " is not numeric.")
    }
    if (is.character(high) || is.na(high)) {
      stop("High value ", high, " is not numeric.")
    }
    if (high <= low) {
      stop("High value must be greater than low value.")
    }
  }
  resetFirst <- as.logical(retractFirst)
  ## BUG in Netica, work around
  ## handle <- .Call("RN_SetNodeInterval",node,lv,hv,resetFirst,PACKAGE=RNetica)
  ## node$signalErrors()
  ## invisible(handle)
  lik <- diff(punif(NodeLevels(node),lv,hv))
  if (resetFirst) RetractNodeFinding(node)
  NodeLikelihood(node) <- lik
}

NodeExpectedValue <- function (node) {
  if (length(node)>1 || !is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  result <- .Call("RN_GetNodeExpectedValue",node,PACKAGE=RNetica)
  node$signalErrors()
  result
}

NodeExpectedUtils <- function (node) {
  if (length(node)>1 || !is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  result <- .Call("RN_GetNodeExpectedUtils",node,PACKAGE=RNetica)
  node$signalErrors()
  names(result) <- NodeStates(node)
  result
}

CalcNodeState <- function (node) {
  if (length(node)>1 || !is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  result <- .Call("RN_CalcNodeState",node,PACKAGE=RNetica)
  ecount <- node$signalErrors()
  if (is.numeric(result) && !is.na(result)) {
    result <- NodeStates(node)[result+1] ## Convert to name.
  }
  result
}

CalcNodeValue <- function (node) {
  if (length(node)>1 || !is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  result <- .Call("RN_CalcNodeValue",node,PACKAGE=RNetica)
  node$signalErrors()
  result
}

MutualInfo <- function (target, nodelist) {
  if (length(target)>1L || !is.NeticaNode(target) || !is.active(target)) {
    stop ("Target is not an active Netica node", target)
  }
  if(!is.list(nodelist) || length(nodelist) == 0) {
    stop("Nodelist must be list of Netica nodes")
  }
  if (!all(sapply(nodelist,
                  function (n) is.NeticaNode(n) & is.active(n)))) {
    stop("Nodelist must be list of active Netica Nodes")
  }
  result <- .Call("RN_GetMutalInfo",target,nodelist,PACKAGE=RNetica)
  target$signalErrors()
  if (!is.null(names(nodelist))) {
    names(result) <- names(nodelist)
  } else {
    names(result) <- sapply(nodelist,NodeName)
  }
  result
}

VarianceOfReal <- function (target, nodelist) {
  if (length(target)>1L || !is.NeticaNode(target) || !is.active(target)) {
    stop ("Target is not an active Netica node", target)
  }
  if(!is.list(nodelist) || length(nodelist) == 0) {
    stop("Nodelist must be list of Netica nodes")
  }
  if (!all(sapply(nodelist,
                  function (n) is.NeticaNode(n) & is.active(n)))) {
    stop("Nodelist must be list of active Netica Nodes")
  }
  result <- .Call("RN_GetVarianceOfReal",target,nodelist,PACKAGE=RNetica)
  target$signalErrors()
  if (!is.null(names(nodelist))) {
    names(result) <- names(nodelist)
  } else {
    names(result) <- sapply(nodelist,NodeName)
  }
  result
}

woe <- function (enodes,hnodes,hstatelists) {
  if (!is.list(enodes)) enodes <- list(enodes)
  if (any(sapply(enodes,function (nd)
    !is.NeticaNode(nd) || !is.active(nd))))
    stop("Expected a list of active netica nodes, got",enodes)
  if (!is.list(hnodes))
    hnodes <- list(hnodes)
  if (!is.list(hstatelists))
    hstatelists <- list(hstatelists)
  if (any(sapply(hnodes,function (nd)
    !is.NeticaNode(nd) || !is.active(nd)))) {
    stop("Expected a list of Netica nodes, got ",hnodes)
  }
  if (length(hstatelists) > length(hnodes)) {
    stop("More statelists than nodes.")
  }
  net <- NodeNet(hnodes[[1]])

  ## Not sure whether hypothesis or negation is compound, so set up
  ## likelihood based on hypothesis being true and use virtual evidence.
  hlikes <- mapply(function(hnode,hstatelist) {
    stnames <- NodeStates(hnode)
    hlike <- rep(0,length(stnames))
    names(hlike) <- stnames
    hlike[hstatelist] <- 1
    hlike
  }, hnodes,hstatelists,SIMPLIFY=FALSE)
  elikes <- lapply(enodes,NodeLikelihood)

  tryCatch({
    for (i in 1:length(hnodes)) {
      RetractNodeFinding(hnodes[[i]])
      NodeLikelihood(hnodes[[i]]) <- hlikes[[i]]
    }
    p_Htrue.e <- FindingsProbability(net)

    for (i in 1:length(hnodes)) {
      RetractNodeFinding(hnodes[[i]])
      NodeLikelihood(hnodes[[i]]) <- 1-hlikes[[i]]
    }
    p_Hfalse.e <- FindingsProbability(net)

    tryCatch({
      lapply(enodes,RetractNodeFinding)
      for (i in 1:length(hnodes)) {
        RetractNodeFinding(hnodes[[i]])
        NodeLikelihood(hnodes[[i]]) <- hlikes[[i]]
      }
      p_Htrue <- FindingsProbability(net)

      for (i in 1:length(hnodes)) {
        RetractNodeFinding(hnodes[[i]])
        NodeLikelihood(hnodes[[i]]) <- 1-hlikes[[i]]
      }
      p_Hfalse <- FindingsProbability(net)
      100*(log10(p_Htrue.e/p_Hfalse.e) - log10(p_Htrue/p_Hfalse))
    }, finally = mapply(function(enode,elik) {
      NodeLikelihood(enode) <-elik
    },enodes,elikes))

  }, finally = sapply(hnodes,RetractNodeFinding))

}


ewoe <- function (enodes,hnodes,hstatelists) {
  if (!is.list(enodes))
    enodes <- list(enodes)
  if (!all(sapply(enodes,is.NeticaNode))) {
    stop("Expected a list of Netica nodes, got ",enodes)
  }
  if (!is.list(hnodes))
    hnodes <- list(hnodes)
  if (!all(sapply(hnodes,is.NeticaNode))) {
    stop("Expected a list of Netica nodes, got ",hnodes)
  }
  if (!is.list(hstatelists))
    hstatelists <- list(hstatelists)
  if (length(hstatelists) > length(hnodes)) {
    stop("More statelists than nodes.")
  }

  ## Not sure whether hypothesis or negation is compound, so set up
  ## likelihood based on hypothesis being true and use virtual evidence.
  hlikes <- mapply(function(hnode,hstatelist) {
    stnames <- NodeStates(hnode)
    hlike <- rep(0,length(stnames))
    names(hlike) <- stnames
    hlike[hstatelist] <- 1
    hlike
  }, hnodes,hstatelists,SIMPLIFY=FALSE)

  tryCatch({
    for (i in 1:length(hnodes)) {
      RetractNodeFinding(hnodes[[i]])
      NodeLikelihood(hnodes[[i]]) <- hlikes[[i]]
    }
    p_Htrue <- lapply(enodes,NodeBeliefs)

    for (i in 1:length(hnodes)) {
      RetractNodeFinding(hnodes[[i]])
      NodeLikelihood(hnodes[[i]]) <- 1-hlikes[[i]]
    }
    p_Hfalse <- lapply(enodes,NodeBeliefs)

    ewoes <- mapply(function (p_H,p_notH) {100*sum(log10(p_H/p_notH)*p_H)},
                    p_Htrue,p_Hfalse)
    if (length(names(enodes)) > 0) {
      names(ewoes) <- names(enodes)
    } else {
      names(ewoes) <- sapply(enodes,NodeName)
    }
    ewoes
  }, finally = sapply(hnodes,RetractNodeFinding))

}


##########
## Equations


NodeEquation <- function (node) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  equation <- .Call("RN_GetNodeEquation",node,PACKAGE=RNetica)
  node$signalErrors()
  equation
}

"NodeEquation<-" <- function (node, autoconvert=TRUE, value) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  value <- as.character(value)
  if (any(is.na(value))) {
    stop("Non-character titles in ", value)
  }
  value <- paste(value,collapse="\n")
  handle <- .Call("RN_SetNodeEquation",node,value,PACKAGE=RNetica)
  node$signalErrors()
  if (autoconvert)
    EquationToTable(node)
  invisible(handle)
}

EquationToTable <- function (node, numSamples=25, sampUnc=TRUE,
                             addExist=FALSE) {
  if (!is.NeticaNode(node)) {
    stop("Expected an active Netica node, got, ",node)
  }
  numSamples <- as.integer(numSamples)
  if (is.na(numSamples) || numSamples < 1) {
    stop("Number of samples must be a positive integer.")
  }
  sampUnc <- as.logical(sampUnc)
  addExist <- as.logical(addExist)
  if (is.na(sampUnc) || is.na(addExist)) {
    stop("Flag sampUnc and addExist must be logical values.")
  }
  handle <- .Call("RN_EquationToTable",node,numSamples,sampUnc,addExist,
                  PACKAGE=RNetica)
  node$signalErrors()
  invisible(handle)
}

