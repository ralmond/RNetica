## Random.R -- Functions related to random generation


###############################################################
## Random Generator Objects
## These should generally be short lived, but we may need to create
## one to generate several sets of case files.

setRefClass("NeticaRNG",fields=c(Name="character",
                                 Session="NeticaSession",
                                 Netica_RNG="externalptr",
                                 Seed="integer"),
            methods=list(
                initialize=function(Name=".Prototype",
                                    Session=NeticaSession(SessionName=".prototype"),
                                    Seed=as.integer(runif(1,0,1000000000)),
                                    ...) {
                  callSuper(Name=Name,Session=Session,
                            Netica_RNG=externalptr(),Seed=Seed,...)
                },
                isActive = function() {
                  .Call("RN_isRNGActive",stream,PACKAGE=RNetica)
                },
                reportErrors = function(maxreport=9,clear=TRUE) {
                  Session$reportErrors(maxreport,clear)
                },
                clearErrors = function(severity="XXX_ERR") {
                  Session$clearErrors(severity)
                },
                free=function() {
                  .Call("RN_FreeRNG",.self,PACKAGE=RNetica)
                  ecount <- Session$reportErrors()
                  if (ecount[1]>0) {
                    stop("Netica Errors Encountered, see console for details.")
                  }
                },
                show=function() {
                  if (isActive()) {
                    cat("Active Netica RNG ",Name,"\n")
                  } else {
                    cat("Inactive Netica RNG ",Name,"\n")
                  }
                }))

## Global variable used to generate names for RNGS
RNGCount <- 0

NewNeticaRNG <- function (seed=runif(1,0,1000000000),
                          session=getDefaultSession()) {
  seed <- abs(as.integer(seed))
  if (is.null(seed) || is.na(seed)) {
    stop("Seed must be an integer")
  }
  RNGCount <<- RNGCount +1
  name <- paste("NeticaRNG",RNGCount,sep=".")
  rng <- NeticaRNG$new(Name=Name,Session=Session,seed=seed)
  rng <-
    .Call("RN_NewRandomGenerator",as.character(seed),rng,PACKAGE="RNeticaXR")
  ecount <- session$reportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  rng
}

FreeNeticaRNG <- function (rng) {
  if (!is.NeticaRNG(rng))
    stop("Trying to free a non-rng object.")
  if (!isNeticaRNGActive(rng)) {
    warning("Netica RNG already freed.")
    return (rng)
  }
  rng$free()
  rng
}

setMethod("toString","NeticaRNG", function (x, ...) {
  status <- ifelse(isNeticaRNGActive(x),"Active","Freed")
  paste("<",status,class(x),":",x$name,">")
})

setMethod("print","NeticaRNG",function(x, ...) {
  cat(toString(x),"\n")
})


is.NeticaRNG <- function (x) {
  is(x,"NeticaRNG")
}

isNeticaRNGActive <- function (rng) {
  if (!is.NeticaRNG(rng)) return (NA_integer_)
  rng$isActive()
}

WithRNG <- function (rng,expr) {
  if (!isNeticaRNGActive(rng)) {
    stop("RNG is not active.")
  }
  tryCatch(expr,
           finally = FreeNeticaRNG(rng))
}


NetworkSetRNG <- function (net, seed=runif(1,0,1000000000)) {
  if (!is.NeticaBN(net)) {
    stop("Net must be a Netica BN.")
  }
  seed <- abs(as.integer(seed))
  if (is.null(seed) || is.na(seed)) {
    stop("Seed must be an integer.")
  }
  seed <- as.character(seed)
  result <- .Call("RN_SetNetRandomGen",net,seed,net$session,PACKAGE="RNeticaXR")
  ecount <- net$reportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  result
}

GenerateRandomCase <- function (nodelist, method="Default",
                                timeout=100.0,rng=NULL) {
  if (is.NeticaNode(nodelist) && length(nodelist) ==1L) {
    nodelist <- list(nodelist)
  }
  if (!is.list(nodelist) || any(!sapply(nodelist,is.NeticaNode))) {
    stop("Expected a list of Netica nodes, got, ",nodelist)
  }
  if (!is.character(method) && !(toupper(substr(method,1,1)) %in%
                                 c("D","J","F"))) {
    stop("Method must be one of Join_Tree_Sampling, Forward_Sampling, or Default_Sampling")
  } else {
    method <- toupper(method)
  }
  if (!is.null(rng) && (!is.NeticaRNG(rng) || !isNeticaRNGActive(rng))) {
    stop("The rng argument must be an active NeticaRNG or NULL.")
  }
  timeout <- as.double(timeout)
  if (is.na(timeout) && timeout <= 0) {
    stop("Timeout must be a postive number.")
  }
  session <- NodeNet(nodelist[[1]])$session
  result <-
    .Call("RN_GenerateRandomCase",nodelist,method,timeout,rng,
          session,PACKAGE=RNetica)
  ecount <- session$reportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  if (result < 0) {
    warning("Random number generation not successful.")
  }
  invisible(result)
}

