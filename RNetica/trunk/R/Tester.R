###
## Network Tester class.

NetworkTester <-
  setClass("NetworkTester",
           list(Net="NeticaBN",
                targetNodes="list",
                ignoreNodes="list",
                data="list",
                errorRate="numeric",
                logLoss="numeric",
                quadraticLoss="numeric",
                confusion="list"))

testerNet <- function(tester) tester@Net
testerTarget <- function(tester) tester@targetNodes
testerIgnore <- function(tester) tester@ignoreNodes

testerErrorRate <- function(tester,node=NULL) {
  if (is.null(node)) return(tester@errorRate)
  if (is.NeticaNode(node)) node <- NodeName(node)
  tester@errorRate[node]
}
testerLogLoss <- function(tester,node=NULL) {
  if (is.null(node)) return(tester@logLoss)
  if (is.NeticaNode(node)) node <- NodeName(node)
  tester@logLoss[node]
}
testerQuadraticLoss <- function(tester,node=NULL) {
  if (is.null(node)) return(tester@quadraticLoss)
  if (is.NeticaNode(node)) node <- NodeName(node)
  tester@quadraticLoss[node]
}
testerConfusion <- function(tester,node=NULL) {
  if (is.null(node)) return(tester@confusion)
  if (is.NeticaNode(node)) node <- NodeName(node)
  tester@confusion[[node]]
}

testerKappa <- function(tester,node=NULL,weights=c("None","Linear","Quadratic"),
                        W=NULL) {
  if (is.null(node)) {
    result <- sapply(testerTarget(tester),
                     function (nd) testerKappa(tester,nd,weights,W))
    names(result) <- names(tester@confusion)
  } else {
    if (is.null(W))
      result <- fcKappa(testerConfusion(tester,node),weights)
    else
      result <- fcKappa(testerConfusion(tester,node),weights,W)
  }
  result
}

testerLambda <- function(tester,node=NULL,weights=c("None","Linear","Quadratic"),
                        W=NULL) {
  if (is.null(node)) {
    result <- sapply(testerTarget(tester),
                     function (nd) testerLambda(tester,nd,weights,W))
    names(result) <- names(tester@confusion)
  } else {
    if (is.null(W))
      result <- gkLambda(testerConfusion(tester,node),weights)
    else
      result <- gkLambda(testerConfusion(tester,node),weights,W)
  }
  result
}

summary.NetworkTester <- function(object, ...) {
  data.frame(ErrorRate=testerErrorRate(object),
             LogLoss=testerLogLoss(object),
             QuadraticLoss=testerQuadraticLoss(object),
             kappa=testerKappa(object),
             QWK=testerKappa(object,weights="Quadratic"),
             lambda=testerLambda(object),
             LinearLambda=testerLambda(object,weights="Linear"))
}



############################
## Netica Calls

testNetwork <- function(targetNodes, dataStreams, ignoreNodes=list()) {
  if (is.NeticaNode(targetNodes)) targetNodes <- list(targetNodes)
  if (!is.list(targetNodes) ||
      any(sapply(targetNodes,function(nd) !is.NeticaNode(nd) || !is.active(nd)))) {
    stop ("Target nodes must be a list of active NeticaNodes.")
  }
  if (length(targetNodes)<1L)
    stop ("No target nodes.")
  net <- NodeNet(targetNodes[[1]])
  if (!is.list(ignoreNodes) ||
      any(sapply(ignoreNodes,function(nd) !is.NeticaNode(nd) || !is.active(nd)))) {
    stop ("Ignored nodes must be a list of active NeticaNodes.")
  }
  if (!is.list(dataStreams)) dataStreams <- list(dataStreams)
  if (any(sapply(dataStreams, function(ds)
    !is.NeticaCaseStream(ds) || !is.active(ds)))) {
    stop("Expected a list of active Netica Case Streams")
  }
  tnNames <- sapply(targetNodes,NodeName)
  errorRate <- rep(NA_real_,length(tnNames))
  names(errorRate) <- tnNames
  logLoss <- rep(NA_real_,length(tnNames))
  names(logLoss) <- tnNames
  quadraticLoss <- rep(NA_real_,length(tnNames))
  names(quadraticLoss) <- tnNames
  confusion <- lapply(targetNodes,
                      function (node) {
                        ns <- NodeStates(node)
                        matrix(NA_real_,length(ns),length(ns),
                               dimnames=list("predicted"=ns,"actual"=ns))
                      })
  names(confusion) <- tnNames
  tester <- new("NetworkTester",Net=net,targetNodes=targetNodes,ignoreNodes=ignoreNodes,
                data=dataStreams,errorRate=errorRate,logLoss=logLoss,
                quadraticLoss=quadraticLoss,confusion=confusion)
  handle <- .Call("RN_TestNetwork",tester,PACKAGE=RNetica)
  ecount <- net$reportErrors()
  if (ecount[1L]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  invisible(handle)
}

