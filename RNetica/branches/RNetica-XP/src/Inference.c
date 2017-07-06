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

//Freely adapted from NeticaEx.c
bool_ns RN_NextStates (state_bn* config, state_bn* max_states, int nn){
  int i;
  for (i = 0; i< nn; i++) {
    if (++config[i] < max_states[i])
      return TRUE;
    config[i] = 0;
  }
  return FALSE;
}


SEXP RN_JointProbability(SEXP nodelist) {
  nodelist_bn *nodes = RN_AS_NODELIST(nodelist,NULL);
  int i, n, size=1, nn=length(nodelist);
  state_bn *config, *max_states;
  SEXP result, dim;
  double *rresult;
  if (!nodes) {
    return R_NilValue;
  }
  //Calculate sizes and initialize arrays.
  PROTECT(dim=allocVector(INTSXP,nn));
  config = (state_bn *) R_alloc(nn,sizeof(state_bn));
  max_states = (state_bn *) R_alloc(nn,sizeof(state_bn));
  for (i=0; i<nn; i++) {
    max_states[i] = GetNodeNumberStates_bn (NthNode_bn (nodes, i));
    size = size*max_states[i];
    INTEGER(dim)[i]=max_states[i];
    config[i]=0;
  }
  PROTECT(result=allocArray(REALSXP,dim));
  rresult = REAL(result);

  n=0;
  do {
    rresult[n++] = JointProbability_bn(nodes,config);
  } while (RN_NextStates(config,max_states,nn));

  UNPROTECT(2);
  return result;
}

SEXP RN_FindingsProbability(SEXP net) {
  net_bn* netica_handle;
  netica_handle = GetNeticaHandle(net);
  if (netica_handle) {
    return ScalarReal(FindingsProbability_bn(netica_handle));
  } else {
    warning("FindingProbability: Could not find network %s.",BN_NAME(net));
    return ScalarReal(R_NaReal);
  }
}

SEXP RN_MostProbableConfig(SEXP net, SEXP nth) {
  net_bn *net_handle = GetNeticaHandle(net);
  if (!net_handle) {
    error("Could not find network for", BN_NAME(net));
    return R_NilValue;
  }
  const nodelist_bn *nodes = GetNetNodes_bn(net_handle);
  int i, nn=LengthNodeList_bn(nodes);
  state_bn *config;
  const char *state;
  SEXP result, names;

  if (!nodes) {
    return R_NilValue;
  }
  config = (state_bn *) R_alloc(nn,sizeof(state_bn));
  MostProbableConfig_bn(nodes,config,INTEGER(nth)[0]);
  PROTECT(result=allocVector(STRSXP,nn));
  PROTECT(names=allocVector(STRSXP,nn));
  for (i=0; i<nn; i++) {
    SET_STRING_ELT(names,i,mkChar(GetNodeName_bn(NthNode_bn(nodes,i))));
    state = GetNodeStateName_bn(NthNode_bn(nodes,i),config[i]);
    SET_STRING_ELT(result,i,mkChar(state));
  }
  namesgets(result,names);
  UNPROTECT(2);
  return result;
}

SEXP RN_SizeCompiledNetwork(SEXP net) {
  net_bn* netica_handle;
  netica_handle = GetNeticaHandle(net);
  if (netica_handle) {
    return ScalarReal(SizeCompiledNet_bn(netica_handle,0));
  } else {
    warning("SizeCompiledNetwork: Could not find network %s.",BN_NAME(net));
    return ScalarReal(R_NaReal);
  }
}

SEXP RN_GetEliminationOrder(SEXP net) {
  net_bn* netica_handle;
  netica_handle = GetNeticaHandle(net);
  if (netica_handle) {
    const nodelist_bn *order = GetNetElimOrder_bn(netica_handle);
    if (order) {
      return RN_AS_RLIST(order);
    } else {
      return R_NilValue;
    }
  } else {
    warning("GetEliminationOrder: Could not find network %s.",BN_NAME(net));
    return R_NilValue;
  }
}

SEXP RN_SetEliminationOrder(SEXP net, SEXP order) {
  net_bn* netica_handle;
  netica_handle = GetNeticaHandle(net);
  if (netica_handle) {
    if (isNull(order)) {
      SetNetElimOrder_bn(netica_handle,NULL);
    } else {
      SetNetElimOrder_bn(netica_handle,RN_AS_NODELIST(order,netica_handle));
    }
  } else {
    warning("GetEliminationOrder: Could not find network %s.",BN_NAME(net));
  }
  return net;
}

SEXP RN_SplitReport(const char *report) {
  if (report==NULL || strlen(report)==0) {
    // Trap for zero length
    return (allocVector(STRSXP,0));
  }
  char *draft = R_alloc(strlen(report),sizeof(char));
  draft = strcpy(draft,report);
  SEXP result;
  int i, nlines =1;
  
  for (i=0; i<strlen(draft); i++) {
    if (draft[i]=='\n') nlines++;
  }

  PROTECT(result=allocVector(STRSXP,nlines));
  char *line =strtok(draft,"\n");
  i = 0;
  while (line) {
    SET_STRING_ELT(result,i++,mkChar(line));
    line=strtok(NULL,"\n");
  }
  UNPROTECT(1);
  return (result);
}

SEXP RN_JunctionTreeReport(SEXP net) {
  net_bn* netica_handle;
  netica_handle = GetNeticaHandle(net);
  if (netica_handle) {
    return RN_SplitReport(ReportJunctionTree_bn(netica_handle));
  } else {
    warning("SizeCompiledNetwork: Could not find network %s.",BN_NAME(net));
    return ScalarReal(R_NaReal);
  }
}

