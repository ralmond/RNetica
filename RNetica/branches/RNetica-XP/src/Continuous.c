/**
 * Continuous.c --- This file contains functions for continuous nodes
 * and decision analysis.
 */

#include <string.h>
#include <R.h>
#include <Rdefines.h>
#include <RNetica.h>

SEXP RN_GetNodeValue(SEXP node) {
  node_bn* node_handle;
  node_handle = GetNodeHandle(node);
  if (node_handle) {
    double val = GetNodeValueEntered_bn(node_handle);
    if (val==UNDEF_DBL) val = R_NaReal;
    return ScalarReal(val);
  } else {
    error("Could not find node %s.",NODE_NAME(node));
  }
}

SEXP RN_SetNodeValue(SEXP node, SEXP value) {
  node_bn* node_handle;

  node_handle = GetNodeHandle(node);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(node));
  } else {
    net_bn* net = GetNodeNet_bn (node_handle);
    int saved = SetNetAutoUpdate_bn (net, 0);    //for efficiency 
    RetractNodeFindings_bn (node_handle);
    EnterNodeValue_bn (node_handle, REAL(value)[0]);
    SetNetAutoUpdate_bn (net, saved);            
  }
  return node;
}

SEXP RN_SetNodeGaussian(SEXP node, SEXP mean, SEXP sd, 
     SEXP reset_first) {
  node_bn* node_handle;
  node_handle = GetNodeHandle(node);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(node));
  } else {
    net_bn* net = GetNodeNet_bn (node_handle);
    if (LOGICAL(reset_first)[0]) { // Retract first
      int saved = SetNetAutoUpdate_bn (net, 0);    //for efficiency 
      RetractNodeFindings_bn (node_handle);
      EnterGaussianFinding_bn (node_handle, REAL(mean)[0], REAL(sd)[0]);
      SetNetAutoUpdate_bn (net, saved);            
    } else { // Don't retract first
      EnterGaussianFinding_bn (node_handle, REAL(mean)[0], REAL(sd)[0]);
    }
  }
  return node;
}

SEXP RN_SetNodeInterval(SEXP node, SEXP low, SEXP high, 
     SEXP reset_first) {
  node_bn* node_handle;
  node_handle = GetNodeHandle(node);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(node));
  } else {
    net_bn* net = GetNodeNet_bn (node_handle);
    if (LOGICAL(reset_first)[0]) { // Retract first
      int saved = SetNetAutoUpdate_bn (net, 0);    //for efficiency 
      RetractNodeFindings_bn (node_handle);
      EnterIntervalFinding_bn (node_handle, REAL(low)[0], REAL(high)[0]);
      SetNetAutoUpdate_bn (net, saved);            
    } else { // Don't retract first
      EnterIntervalFinding_bn (node_handle, REAL(low)[0], REAL(high)[0]);
    }
  }
  return node;
}

SEXP RN_GetNodeExpectedValue(SEXP node) {
  node_bn* node_handle;
  SEXP result;
  node_handle = GetNodeHandle(node);
  if (node_handle) {
    double sd;
    double val = GetNodeExpectedValue_bn(node_handle, &sd, NULL, NULL);
    if (val==UNDEF_DBL) val = R_NaReal;
    if (sd==UNDEF_DBL) sd = R_NaReal;
    PROTECT(result = ScalarReal(val));
    setAttrib(result,sdatt,ScalarReal(sd));
    UNPROTECT(1);
    return result;
  } else {
    error("Could not find node %s.",NODE_NAME(node));
  }
}

SEXP RN_GetNodeExpectedUtils(SEXP node) {
  node_bn* node_handle;

  node_handle = GetNodeHandle(node);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(node));
    return(R_NilValue);
  } else {
    return RN_AS_PROBSXP(GetNodeExpectedUtils_bn(node_handle),
                         GetNodeNumberStates_bn(node_handle));
  }
}

SEXP RN_CalcNodeState(SEXP node) {
  node_bn* node_handle;
  state_bn result;
  node_handle = GetNodeHandle(node);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(node));
  } else {
    result = CalcNodeState_bn(node_handle);
    if (result == UNDEF_STATE) return ScalarInteger(R_NaInt);
    if (result >=0) return ScalarInteger(result);
  }
  return ScalarInteger(R_NaInt);
}

SEXP RN_CalcNodeValue(SEXP node) {
  node_bn* node_handle;
  double result;
  node_handle = GetNodeHandle(node);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(node));
  } else {
    result = CalcNodeValue_bn(node_handle);
    if (result == UNDEF_DBL) return ScalarReal(R_NaReal);
    ScalarReal(result);
  }
  return ScalarReal(R_NaReal);
}

//////////////
// Mutual information.
// Need to first create a Senstivity to findings object and then call
// the actual mutual info calls.

SEXP RN_GetMutualInfo(SEXP target, SEXP nodelist) {
  R_len_t n, nn = length(nodelist);
  node_bn* target_handle = GetNodeHandle(target);
  nodelist_bn* vnodes = RN_AS_NODELIST(nodelist,NULL);
  SEXP result = R_NilValue;

  if (target_handle==NULL) {
    error("Node not found.");
    return(R_NilValue);
  }
  if (nodelist==NULL) {
    error("No nodes to query.");
    return(R_NilValue);
  }
  sensv_bn* stf = NewSensvToFinding_bn(target_handle,vnodes,ENTROPY_SENSV);
  if (stf==NULL) {
    error("Error creating sensitivity to findings.");
    return(R_NilValue);
  }
  PROTECT(result = allocVector(REALSXP,nn));
  for (n=0; n <nn; n++) {
    double val = GetMutualInfo_bn(stf,NthNode_bn(vnodes,n));
    if (val==UNDEF_DBL) {
      REAL(result)[n] = NA_REAL;
    } else {
      REAL(result)[n] = val;
    }
  }

  DeleteSensvToFinding_bn(stf);
  UNPROTECT(1);
  return(result);
}

SEXP RN_GetVarianceOfReal(SEXP target, SEXP nodelist) {
  R_len_t n, nn = length(nodelist);
  node_bn* target_handle = GetNodeHandle(target);
  nodelist_bn* vnodes = RN_AS_NODELIST(nodelist,NULL);
  SEXP result = R_NilValue;

  if (target_handle==NULL) {
    error("Node not found.");
    return(R_NilValue);
  }
  if (nodelist==NULL) {
    error("No nodes to query.");
    return(R_NilValue);
  }
  sensv_bn* stf = NewSensvToFinding_bn(target_handle,vnodes,
                                       REAL_SENSV+VARIANCE_SENSV);
  if (stf==NULL) {
    error("Error creating sensitivity to findings.");
    return(R_NilValue);
  }
  PROTECT(result = allocVector(REALSXP,nn));
  for (n=0; n <nn; n++) {
    double val = GetVarianceOfReal_bn(stf,NthNode_bn(vnodes,n));
    if (val==UNDEF_DBL) {
      REAL(result)[n] = NA_REAL;
    } else {
      REAL(result)[n] = val;
    }
  }

  DeleteSensvToFinding_bn(stf);
  UNPROTECT(1);
  return(result);
}
    
////////////////////////////////////////////////////////////
// Adding minimal equations functionality because I need it for
// testing


SEXP RN_GetNodeEquation(SEXP nd) {
  const char *equation;
  node_bn* node_handle;
  SEXP result;

  PROTECT(result = allocVector(STRSXP,1));

  node_handle = GetNodeHandle(nd);

  if (node_handle) {
    equation = GetNodeEquation_bn(node_handle);
    SET_STRING_ELT(result,0,mkChar(equation));
  } else {
    SET_STRING_ELT(result,0,NA_STRING);
    warning("Could not find node %s.",NODE_NAME(nd));
  }
  UNPROTECT(1);
  return(result);
}

SEXP RN_SetNodeEquation(SEXP nd, SEXP newequation) {
  const char *equation;
  node_bn* node_handle;

  node_handle = GetNodeHandle(nd);

  if (node_handle) {
    equation = CHAR(STRING_ELT(newequation,0));
    SetNodeEquation_bn(node_handle,equation);
  } else {
    warning("Could not find node %s.",NODE_NAME(nd));
  }
  return(nd);
}

SEXP RN_EquationToTable(SEXP nd, SEXP numSamples, SEXP sampUnc, SEXP addExist) {
  node_bn* node_handle = GetNodeHandle(nd);

  if (node_handle) {
    EquationToTable_bn(node_handle,INTEGER(numSamples)[0],
                       LOGICAL(sampUnc)[0],LOGICAL(addExist)[0]);
  } else {
    warning("Could not find node %s.",NODE_NAME(nd));
  }
  return(nd);
}

