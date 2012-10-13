/**
 * Inference.c --- This files contains functions for compiling the
 * Bayes net and performing inference tasks.
 */

#include <string.h>
#include <R.h>
#include <Rdefines.h>
#include <RNetica.h>

SEXP RN_CompileNet(SEXP net) {
  net_bn* netica_handle;
  netica_handle = GetNeticaHandle(net);
  if (netica_handle) {
    CompileNet_bn(netica_handle);
  } else {
    warning("CompileNetwork: Could not find network %s.",BN_NAME(net));
  }
  return(net);
}

SEXP RN_UncompileNet(SEXP net) {
  net_bn* netica_handle;
  netica_handle = GetNeticaHandle(net);
  if (netica_handle) {
    UncompileNet_bn(netica_handle);
  } else {
    warning("CompileNetwork: Could not find network %s.",BN_NAME(net));
  }
  return(net);
}

SEXP RN_RetractNetFindings(SEXP net) {
  net_bn* netica_handle;
  netica_handle = GetNeticaHandle(net);
  if (netica_handle) {
    RetractNetFindings_bn(netica_handle);
  } else {
    warning("RetractNetworkFindings: Could not find network %s.",BN_NAME(net));
  }
  return(net);
}


SEXP RN_GetNodeFinding(SEXP node) {
  node_bn* node_handle;
  state_bn result;
  node_handle = GetNodeHandle(node);
  if (!node_handle) {
    error("GetNodeFinding: Could not find node %s.",NODE_NAME(node));
  } else {
    result = GetNodeFinding_bn(node_handle);
    if (result >=0) return ScalarInteger(result);
    if (result == NO_FINDING) return mkString("@NO FINDING");
    if (result == NEGATIVE_FINDING) return mkString("@NEGATIVE FINDINGS");
    if (result == LIKELIHOOD_FINDING) return mkString("@LIKELIHOOD");
  }
  return ScalarInteger(R_NaInt);
}

SEXP RN_RetractNodeFinding(SEXP node) {
  node_bn* node_handle;
  node_handle = GetNodeHandle(node);
  if (!node_handle) {
    error("RetractNodeFinding: Could not find node %s.",NODE_NAME(node));
  } else {
    RetractNodeFindings_bn(node_handle);
  }
  return node;
}

SEXP RN_SetNodeFinding(SEXP node, SEXP value) {
  node_bn* node_handle;

  node_handle = GetNodeHandle(node);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(node));
  } else {
    net_bn* net = GetNodeNet_bn (node_handle);
    int saved = SetNetAutoUpdate_bn (net, 0);    //for efficiency 
    RetractNodeFindings_bn (node_handle);
    EnterFinding_bn (node_handle, INTEGER(value)[0]);
    SetNetAutoUpdate_bn (net, saved);            
  }
  return node;
}

SEXP RN_SetNodeFindingNot(SEXP node, SEXP value) {
  R_len_t n, nn = length(value);
  node_bn* node_handle;

  node_handle = GetNodeHandle(node);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(node));
  } else {
    net_bn* net = GetNodeNet_bn (node_handle);
    int saved = SetNetAutoUpdate_bn (net, 0);    //for efficiency 
    RetractNodeFindings_bn (node_handle);
    for (n=0; n<nn; n++) {
      EnterFindingNot_bn (node_handle, INTEGER(value)[n]);
    }
    SetNetAutoUpdate_bn (net, saved);            
  }
  return node;
}

SEXP RN_IsBeliefUpdated(SEXP n1) {
  node_bn* n1_handle = GetNodeHandle(n1);

  if(n1_handle) {
    return ScalarLogical(IsBeliefUpdated_bn(n1_handle));
  } else {
    error("IsBeliefUpdated:  Naughty node %s\n",NODE_NAME(n1));
    return ScalarInteger(R_NaInt);
  }
}

SEXP RN_GetNodeBeliefs(SEXP node) {
  node_bn* node_handle;

  node_handle = GetNodeHandle(node);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(node));
    return(R_NilValue);
  } else {
    return RN_AS_PROBSXP(GetNodeBeliefs_bn(node_handle),
                         GetNodeNumberStates_bn(node_handle));
  }
}


SEXP RN_GetNodeLikelihood(SEXP node) {
  node_bn* node_handle;

  node_handle = GetNodeHandle(node);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(node));
    return(R_NilValue);
  } else {
    return RN_AS_PROBSXP(GetNodeLikelihood_bn(node_handle),
                         GetNodeNumberStates_bn(node_handle));
  }
}

SEXP RN_SetNodeLikelihood(SEXP node, SEXP value) {
  node_bn* node_handle;

  node_handle = GetNodeHandle(node);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(node));
  } else {
    net_bn* net = GetNodeNet_bn (node_handle);
    int saved = SetNetAutoUpdate_bn (net, 0);    //for efficiency 
    RetractNodeFindings_bn (node_handle);

    EnterNodeLikelihood_bn(node_handle, RN_AS_PROB_BN(value));
    SetNetAutoUpdate_bn (net, saved);
  }
  return(node);
}


