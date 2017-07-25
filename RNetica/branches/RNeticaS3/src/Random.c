/**
 * Random Numbers and Testing.
 */

#include <string.h>
#include <R.h>
#include <Rdefines.h>
#include <RNetica.h>


/**
 * Much of the next bit follows 
 * http://www.stat.uiowa.edu/~luke/R/references/weakfinex.html
 *
 * This is not well documented in Luke's web page, but I've put the
 * stream pointer in as the "key" of the weak pointer list, and the
 * NeticaCaseStream object as the "value".  This allows me to set the
 * pointer to NULL when I'm closing all open streams, say on detatch.
 */
/**
 * Finalizer:  calling this multiple times should be harmless.
 */
SEXP RNGFree (SEXP rngptr) {
  randgen_ns *rng_handle;
  if (rngptr == NULL || isNull(rngptr)) {
    //Already closed, nothing to do
  } else {
    if (TYPEOF(rngptr) != EXTPTRSXP || 
        R_ExternalPtrTag(rngptr) != rngatt) {
      warning("Trying to free a non-rng pointer");
    } else {
      rng_handle = (randgen_ns*) R_ExternalPtrAddr(rngptr);
      if (rng_handle != NULL) {
        DeleteRandomGen_ns(rng_handle);
        R_ClearExternalPtr(rngptr);
      }
    }
  }
  return R_NilValue;
}

void AddRNGRef(SEXP ref) {
  SEXP r, rngs, next=NULL, last=NULL;
  rngs = CDR(RngList);
  for (r = rngs; r != R_NilValue; r = next) {
    SEXP rr = CAR(r);
    SEXP key = R_WeakRefKey(rr);
    next = CDR(r);
    if (key == R_NilValue || R_ExternalPtrAddr(key)==NULL) {
      if (last == NULL) rngs = next;
      else SETCDR(last,next);
    } else {
      last = r;
    }
  }
  SETCDR(CaseStreamList, CONS(ref,rngs));
}

void FreeRNGs () {
  SEXP r, rngs, next=NULL, last=NULL;
  rngs = CDR(RngList);
  for (r = rngs; r != R_NilValue; r = next) {
    SEXP rr = CAR(r);
    SEXP key = R_WeakRefKey(rr);
    SEXP rng = R_WeakRefValue(rr);
    next = CDR(r);
    if (key != R_NilValue) {
      RNGFree(key);
      if (rng && rng != R_NilValue) {
        setAttrib(rng,rngatt,R_NilValue);
      }
    }
  }
}


SEXP RN_isRNGActive(SEXP rng) {
  SEXP rngPtr, result;
  PROTECT(result=allocVector(LGLSXP,1));
  LOGICAL(result)[0]=FALSE;
  PROTECT(rngPtr = getAttrib(rng,rngatt));
  if (!isNull(rngPtr) && R_ExternalPtrAddr(rngPtr)) {
    LOGICAL(result)[0] = TRUE;
  }
  UNPROTECT(2);
  return result;
}

SEXP RN_NewRandomGenerator (SEXP seed) {
  const char* seedstring=CHAR(STRING_ELT(seed,0));
  randgen_ns* rng =  NewRandomGenerator_ns (seedstring,RN_netica_env, NULL);
  if (rng == NULL ) 
    return R_NilValue;
  else {
    SEXP rngsexp, rngPtr, ref;
    //Allocate new rng object
    PROTECT(rngsexp = allocVector(STRSXP,1));
    SET_STRING_ELT(rngsexp,0,mkChar(seedstring));
    SET_CLASS(rngsexp,rngclass);
    PROTECT(rngPtr = R_MakeExternalPtr(rng,rngatt, R_NilValue));
    setAttrib(rngsexp,rngatt,rngPtr);
    PROTECT(ref = R_MakeWeakRefC(rngPtr,rngsexp,
                                 (R_CFinalizer_t) &RNGFree, 
                                 TRUE));
    AddRNGRef(ref);
    UNPROTECT(3);
    return rngsexp;
  }

}

/**
 * Tests whether or not an object is a Netica RNG.
 */
int isNeticaRNG(SEXP obj) {
  SEXP klass;
  int result = FALSE;
  PROTECT(klass = getAttrib(obj,R_ClassSymbol));
  R_len_t k, kk=length(klass);
  for (k=0; k<kk; k++) {
    if(strcmp(RNGClass,CHAR(STRING_ELT(klass,k))) == 0) {
      result =TRUE;
      break;
    } else {
    }
  }
  UNPROTECT(1);
  return result;
}

SEXP RN_FreeRNG (SEXP rng) {

  if (!isNeticaRNG(rng)) {
    warning("Trying to free a non-RNG object.");
  }
  RNGFree(getAttrib(rng,rngatt));
  setAttrib(rng,rngatt,R_NilValue);
  return(rng);
}



SEXP RN_SetNetRandomGen(SEXP net, SEXP seed) {
  net_bn* netica_handle = GetNeticaHandle(net);
  const char* seedstring=NULL;
  randgen_ns* rng = NULL; 
  if (isNeticaRNG(seed)) {
    rng = GetRNG_Handle(seed);
    if (!rng) {
      error("Could not find Random Number Generator.");
    }
  } else if (!isNull(seed)) {
    seedstring = CHAR(STRING_ELT(seed,0)); 
    rng = NewRandomGenerator_ns(seedstring,RN_netica_env,NULL);
    if (!rng) {
      error("Could not create Random Number Generator.");
    }
  }
  if (netica_handle) {
    SetNetRandomGen_bn(netica_handle,rng,TRUE);
  } else {
    warning("Could not find network %s.",BN_NAME(net));
  }
  return(net);
}

SEXP RN_GenerateRandomCase(SEXP nodelist, SEXP method, 
                           SEXP timeout, SEXP seed) {
  const nodelist_bn* nodes = RN_AS_NODELIST(nodelist,NULL);
  sampling_bn meth = DEFAULT_SAMPLING;
  const char* methstring=CHAR(STRING_ELT(method,0));
  if (methstring[0]=='J') meth=JOIN_TREE_SAMPLING;
  if (methstring[0]=='F') meth=FORWARD_SAMPLING;

  double time=REAL(timeout)[0];

  const char* seedstring=NULL;
  randgen_ns* rng=NULL;
  int result = -1;
  int newrng = FALSE;
  if (isNeticaRNG(seed)) {
    rng = GetRNG_Handle(seed);
    if (!rng) {
      error("Could not find Random Number Generator.");
    }
  } else if (!isNull(seed)) {
    seedstring = CHAR(STRING_ELT(seed,0)); 
    rng = NewRandomGenerator_ns(seedstring,RN_netica_env,NULL);
    newrng = TRUE;
    if (!rng) {
      error("Could not create Random Number Generator.");
    }
  }

  result = GenerateRandomCase_bn(nodes,meth,time,rng);

  if (rng && newrng) {
    DeleteRandomGen_ns(rng);
  }
  DeleteNodeList_bn(nodes);
  return ScalarInteger(result);
}
