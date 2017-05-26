## Random.R -- Functions related to random generation


###############################################################
## Random Generator Objects
## These should generally be short lived, but we may need to create
## one to generate several sets of case files.

NewNeticaRNG <- function (seed=runif(1,0,1000000000)) {
  seed <- abs(as.integer(seed))
  if (is.null(seed) || is.na(seed)) {
    stop("Seed must be an integer")
  }
  rng <-
    .Call("RN_NewRandomGenerator",as.character(seed),PACKAGE="RNetica")
  ecount <- ReportErrors()
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
  rng <- .Call("RN_FreeRNG",rng,PACKAGE="RNetica")
  rng
}

toString.NeticaRNG <- function (x, ...) {
  status <- ifelse(isNeticaRNGActive(x),"Active","Freed")
  src <- toString(unclass(x))
  paste("<",status,class(x)[1],":",src,">")
}

print.NeticaRNG <- function(x, ...) {
  cat(toString(x),"\n")
}


is.NeticaRNG <- function (x) {
  is(x,"NeticaRNG")
}

isNeticaRNGActive <- function (rng) {
  if (!is.NeticaRNG(rng)) return (NA_integer_)
  .Call("RN_isRNGActive",rng,PACKAGE="RNetica")
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
  result <- .Call("RN_SetNetRandomGen",net,seed,PACKAGE="RNetica")
  ecount <- ReportErrors()
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
  result <-
    .Call("RN_GenerateRandomCase",nodelist,method,timeout,rng,
          PACKAGE="RNetica")
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  if (result < 0) {
    warning("Random number generation not successful.")
  }
  invisible(result)
}

