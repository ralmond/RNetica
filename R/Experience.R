## Experience -- functions dealing with learning CPTs from data.

NodeExperience <- function (node) {
  if (length(node)>1L || !is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  parnames <- ParentStates(node)
  statecounts <- sapply(parnames,length)


  if (length(statecounts)>0) {
    result <- array(NA_real_,dim=statecounts,dimnames=parnames)
    config <- -1L
    while (!is.na((config <- nextconfig(config,statecounts))[1L])) {
      val <- .Call("RN_GetNodeExperience",node,config,PACKAGE="RNetica")
      if (!is.null(val))
        result[matrix(config,1)] <- val
    }
  } else { ## Prior node, no parents.
    result<- .Call("RN_GetNodeExperience",node,NULL,PACKAGE="RNetica")
  }
  ecount <- ReportErrors()
  if (ecount[1L]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  result
}

"NodeExperience<-" <- function (node,value) {
  if (length(node)>1L || !is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  if (!is.numeric(value)) {
    stop("Value must be numeric")
  }
  parnames <- ParentStates(node)
  statecounts <- sapply(parnames,length)
  if (length(statecounts) == 0L) {
    if (length(value) !=1L) {
      stop("Dimensions not correct for this node.")
    }
  } else {
    if (length(value) == 1L) { ## Scalar, set all values the same.
      value <- array(value,statecounts)
    }
  }

  if (length(statecounts) == 1L) {
    if (length(value) != statecounts) {
      stop("Dimensions not correct for this node.")
    }
  } else {
    if (length(statecounts) != length(dim(value)) ||
        any (dim(value) != statecounts)) {
      stop("Dimensions not correct for this node.")
    }
  }

  if (length(statecounts)>0) {
    config <- -1L
    while (!is.na((config <- nextconfig(config,statecounts))[1L])) {
      .Call("RN_SetNodeExperience",node,config,
            value[matrix(config,1)],PACKAGE="RNetica")
    }
  } else { ## Prior node, no parents.
    .Call("RN_SetNodeExperience",node,NULL,value,PACKAGE="RNetica")
  }
  ecount <- ReportErrors()
  if (ecount[1L]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  invisible(node)
}

FadeCPT <- function (node, degree=0.2) {
  if (length(node)>1L || !is.NeticaNode(node) || !is.active(node)) {
    stop ("Node is not an active Netica node", node)
  }
  degree <- as.double(degree)
  if (degree <0 || degree >1) {
    stop("Degree must be between 0 and 1")
  }
  handle <- .Call("RN_FadeCPT",node,as.double(degree),PACKAGE="RNetica")
  ecount <- ReportErrors()
  if (ecount[1L]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  invisible(handle)
}


LearnFindings <- function (nodes, weight=1.0) {
  if (is.NeticaNode(nodes) && length(nodes) ==1) {
    nodes <- list(nodes)
  }
  if (any(!sapply(nodes,is.NeticaNode))) {
    stop("Expected a list of Netica nodes, got, ",nodes)
  }
  handles <- .Call("RN_LearnFindings",nodes,as.double(weight),PACKAGE="RNetica")
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  if (length(handles)==1) handles <- handles[[1]]
  invisible(handles)
}


LearnCases <- function(caseStream, nodelist, weight=1.0) {
  if (is.NeticaNode(nodelist) && length(nodelist) ==1L) {
    nodelist <- list(nodelist)
  }
  if (any(!sapply(nodelist,function (nd) {is.NeticaNode(nd) &&
                                          is.active(nd)}))) {
    stop("Expected a list of Netica nodes, got, ",nodelist)
  }
  weight <- as.numeric(weight)
  if (length(weight) >1) {
    warning("LearnCases:  Only the first value of weight will be used.")
  }
  ## If caseStream objected is coerceable into a stream, do that.
  label <- deparse(substitute(caseStream))
  if (is.character(caseStream)) {
    stream <- CaseFileStream(caseStream)
  } else if (is.data.frame(caseStream)) {
    stream <- MemoryCaseStream(caseStream,label)
  } else if (is.NeticaCaseStream(caseStream)) {
    stream <- caseStream
  } else {
    stop("Expected a Case stream or a filename or a data frame to make a case stream.")
  }
  WithOpenCaseStream(stream,
   {
     .Call("RN_LearnCaseStream",stream,nodelist,weight, PACKAGE="RNetica")
     ecount <- ReportErrors()
     if (ecount[1]>0) {
       stop("Netica Errors Encountered, see console for details.")
     }
  invisible(stream)
  })
}

LearnCPTs <- function(caseStream, nodelist, method="COUNTING",
                      maxIters=1000L, maxTol=1.0e-6,
                      weight=1.0) {
  if (is.NeticaNode(nodelist) && length(nodelist) ==1L) {
    nodelist <- list(nodelist)
  }
  if (any(!sapply(nodelist,function (nd) {is.NeticaNode(nd) &&
                                          is.active(nd)}))) {
    stop("Expected a list of Netica nodes, got, ",nodelist)
  }
  weight <- as.numeric(weight)
  if (length(weight) >1) {
    warning("LearnCPTs:  Only the first value of weight will be used.")
  }
  if (is.na(weight[])) {
    stop("Weight must be numeric.")
  }
  maxTol <- as.numeric(maxTol)
  if (length(maxTol) >1) {
    warning("LearnCPTs:  Only the first value of maxTol will be used.")
  }
  if (is.na(maxTol[])) {
    stop("maxTol must be a positive number.")
  }
  maxIters <- as.integer(maxIters)
  if (length(maxIters) >1) {
    warning("LearnCPTs:  Only the first value of maxIters will be used.")
  }
  if (is.na(maxIters[])) {
    stop("maxIters must be a postive integer.")
  }
  if (!is.character(method) || length(method) > 1L) {
    stop("Method must be one of 'GRADIENT', 'EM', or 'COUNTING'")
  }
  method <- toupper(method)
  if (!(method == "GRADIENT" || method == "EM" || method == "COUNTING")) {
    stop("Method must be one of 'GRADIENT', 'EM', or 'COUNTING'")
  }
  ## If caseStream objected is coerceable into a stream, do that.
  label <- deparse(substitute(caseStream))
  if (is.character(caseStream)) {
    stream <- CaseFileStream(caseStream)
  } else if (is.data.frame(caseStream)) {
    stream <- MemoryCaseStream(caseStream,label)
  } else if (is.NeticaCaseStream(caseStream)) {
    stream <- caseStream
  } else {
    stop("Expected a Case stream or a filename or a data frame to make a case stream.")
  }
  WithOpenCaseStream(stream,
   {
     result <- .Call("RN_LearnCPTs",stream,nodelist,method,
                     maxIters,maxTol,weight, PACKAGE="RNetica")
     ecount <- ReportErrors()
     if (ecount[1]>0) {
       stop("Netica Errors Encountered, see console for details.")
     }
     result
   })
}
