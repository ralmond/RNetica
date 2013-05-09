/**
 * Node.c --- This file contains functions for creating,
 * destroying, and modifying states of nodes.
 */

#include <string.h>
#include <R.h>
#include <Rdefines.h>
#include <RNetica.h>

SEXP RN_GetNodeExperience(SEXP node, SEXP states) {
  node_bn* node_handle;

  node_handle = GetNodeHandle(node);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(node));
    return(R_NilValue);
  } else {
    return ScalarReal(GetNodeExperience_bn(node_handle, 
                                           RN_AS_STATE_BN(states)));
  }
}

SEXP RN_SetNodeExperience(SEXP node, SEXP states, SEXP weight) {
  node_bn* node_handle;

  node_handle = GetNodeHandle(node);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(node));
  } else {
    SetNodeExperience_bn(node_handle, RN_AS_STATE_BN(states),
                         REAL(weight)[0]);
  }
  return node;
}

SEXP RN_FadeCPT(SEXP node, SEXP weight) {
  node_bn* node_handle;

  node_handle = GetNodeHandle(node);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(node));
  } else {
    FadeCPTable_bn(node_handle, REAL(weight)[0]);
  }
  return node;
}

SEXP RN_LearnFindings(SEXP nodelist, SEXP weight) {
  nodelist_bn* targets = RN_AS_NODELIST(nodelist,NULL);
  ReviseCPTsByFindings_bn(targets,0,REAL(weight)[0]);
  DeleteNodeList_bn(targets);
  return R_NilValue;
}
