###
## Network Tester class.

NetworkTester <-
  setClass("NetworkTester",
           list(net="NeticaBN",
                targetNodes="list",
                ignoreNodes="list",
                data="list",
                errorRate="numeric",
                logLoss="numeric",
                quadraticLoss="numeric",
                confusion="list"))

testerNet <- function(tester) tester@net
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

############################
## Netica Calls

testNetwork <- function(net, targetNodes, dataStreams, ignoreNodes=list()) {
  if (!is.NetiaBN(net) || !is.active(net)) {
    stop ("Expected an active Netica network, got ",net)
  }
  if (!is.list(targetNodes) ||
      any(sapply(targetNodes,function(nd) !is.NeticaNode(nd) || !is.active(nd)))) {
    stop ("Target nodes must be a list of active NeticaNodes.")
  }
  if (!is.list(ignoreNodes) ||
      any(sapply(ignoreNodes,function(nd) !is.NeticaNode(nd) || !is.active(nd)))) {
    stop ("Ignored nodes must be a list of active NeticaNodes.")
  }
  if (!is.list(dataStreams)) dataStreams <- list(dataStreams)
  if (any(sapply(dataStreams), function(ds)
    !is.NeticaCaseStream(ds) || !is.active(ds))) {
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
  tester <- new("NetworkTester",net=net,targetNodes=targetNodes,ignoreNodes=ignoreNodes,
                data=dataStreams,errorRate=errorRate,logLoss=logLoss,
                quadraticLoss=quadraticLoss,confusion=confusion)
  handle <- .Call("RN_TestNetwork",tester,PACKAGE=RNetica)
  ecount <- child$reportErrors()
  if (ecount[1L]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  invisible(handle)
}

