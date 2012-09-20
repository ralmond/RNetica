/**
 * Nodes.c --- These files describe functions for creating,
 * destroying, and modifying states of nodes.
 */

#include <string.h>
#include <R.h>
#include <Rdefines.h>
#include <RNetica.h>

/***************************************************************************
 * Low Level Node Utilities
 ***************************************************************************/

/**
 * This is a small utility function meant to be used from within
 * toString to determine if the pointer is live or not.
 */
SEXP RN_isNodeActive(SEXP node) {
  SEXP nodePtr, result;
  PROTECT(result=allocVector(LGLSXP,1));
  LOGICAL(result)[0]=FALSE;
  PROTECT(nodePtr = getAttrib(node,nodeatt));
  if (nodePtr && R_ExternalPtrAddr(nodePtr)) {
    LOGICAL(result)[0] = TRUE;
  }
  UNPROTECT(2);
  return result;
}

/**
 * This function allocates a back pointer R object
 * for a newly created net.
 */
SEXP MakeNode_RRef(node_bn* node, const char* name, int isDiscrete) {
  SEXP nd, ndhandle;
  
  nd = allocVector(STRSXP,1);
  R_PreserveObject(nd);
  /* Return the network name */
  SET_STRING_ELT(nd,0,mkChar(name));
  /* Set the handle as an attribute. */
  PROTECT(ndhandle = R_MakeExternalPtr(node,nodeatt, R_NilValue));
  setAttrib(nd,nodeatt,ndhandle);
  setAttrib(nd,nodediscatt,isDiscrete ? TRUEV : FALSEV);
  SET_CLASS(nd,nodeclass);
  
  /* Set a back pointer to the R object in the Netica Object */
  SetNode_RRef(node,nd);
  UNPROTECT(1);
  return nd;
}

/**
 * This function allows for the lazy creation of node objects
 * associated with a network.
 */
SEXP GetNode_RRef(node_bn *node) {
  SEXP nd = FastGetNode_RRef(node);
  if (nd) return nd;            /* Already got one */
  const char *name = GetNodeName_bn(node);
  int isDiscrete = (int) (GetNodeType_bn(node) == DISCRETE_TYPE);
  return MakeNode_RRef(node,name,isDiscrete);
}

/**
 * This function removes the R handles from a node so it can be safely
 * deleted. 
 */
void RN_Free_Node(node_bn* node_handle) {
  SEXP node, nodehandle;
  if (!node_handle) return; //Void pointer, nothing to do.

  node = GetNodeUserData_bn(node_handle,0);
  if (!node) return; //No R object, created nothing to do.
  PROTECT(node);
  PROTECT(nodehandle = getAttrib(node,nodeatt));

  /* Clear the handle */
  R_ClearExternalPtr(nodehandle);
  setAttrib(node,nodeatt,nodehandle); //Probably not needed.
  R_ReleaseObject(node); //Let R garbage collect it when all
  //references are gone.

  UNPROTECT(2);
  return;
}

void RN_Free_Nodes(const nodelist_bn* nodelist) {
  int k, kk=LengthNodeList_bn(nodelist);
  for (k=0; k<kk; k++) {
    RN_Free_Node(NthNode_bn(nodelist,k));
  }
}

/*******************************
 * Node Lists
 *
 * Node lists are just containers for nodes.  For the most part,
 * we want to use R lists which are more flexible, so we force the
 * conversion.
 * Note that when converting to R vectors, we force the creation of 
 * NeticaNode objects if necessary.
 */

/**
 * Calling function should probably portect result.
 */
SEXP RN_AS_RLIST(nodelist_bn* nodelist) {
  int k, kk=LengthNodeList_bn(nodelist);
  SEXP result;

  PROTECT(result = allocVector(VECSXP,kk));
  for (k=0; k<kk; k++) {
    SET_VECTOR_ELT(result,k,GetNode_RRef(NthNode_bn(nodelist,k)));
  }
  UNPROTECT(1);
  return result;
}

nodelist_bn* RN_AS_NODELIST(SEXP nodes, net_bn* net_handle) {
  R_len_t n, nn = length(nodes);
  SEXP node;
  nodelist_bn* result = NewNodeList2_bn(nn,net_handle);
  
  for (n=0; n<nn; n++) {
    PROTECT(node = VECTOR_ELT(nodes,n));
    SetNthNode_bn(result,n,GetNodeHandle(node));
    UNPROTECT(1);
  }
  return result;
}



/***************************************************************************
 * Creating and Destroying Nodes
 ***************************************************************************/


SEXP RN_NewDiscreteNodes(SEXP net, SEXP namelist, SEXP nslist, SEXP statelist) {

  R_len_t n, nn = length(namelist);
  const char *name, *states;
  int nstates;
  net_bn* net_handle;
  node_bn* node_handle;
  SEXP result, node;

  net_handle = GetNeticaHandle(net);
  if (!net_handle) {
    error("Network %s is not valid",BN_NAME(net));
  }
  
  PROTECT(result = allocVector(VECSXP,nn));
  for (n=0; n < nn; n++) {
    name = CHAR(STRING_ELT(namelist,n));
    states = CHAR(STRING_ELT(statelist,n));
    nstates = INTEGER(nslist)[n];
    node_handle = GetNodeNamed_bn(name,net_handle);
    if (node_handle) {
      warning("Node named %s already exists.",name);
      // Return existing node without changes
      SET_VECTOR_ELT(result,n,GetNode_RRef(node_handle));
    } else {
      node_handle = NewNode_bn(name,nstates,net_handle);
      /* Autoset the states at creation time */
      SetNodeStateNames_bn(node_handle,states);
      node = MakeNode_RRef(node_handle,name,TRUE);
      /* Finally, stick it in array */
      SET_VECTOR_ELT(result,n,node);
    }
  }
  UNPROTECT(1);
  return(result);
}

SEXP RN_NewContinuousNodes(SEXP net, SEXP namelist) {

  R_len_t n, nn = length(namelist);
  const char *name;
  net_bn* net_handle;
  node_bn* node_handle;
  SEXP result, node;

  net_handle = GetNeticaHandle(net);
  if (!net_handle) {
    error("Network %s is not valid",BN_NAME(net));
  }
  
  PROTECT(result = allocVector(VECSXP,nn));
  for (n=0; n < nn; n++) {
    name = CHAR(STRING_ELT(namelist,n));
    node_handle = GetNodeNamed_bn(name,net_handle);
    if (node_handle) {
      warning("Node named %s already exists.",name);
      // Return existing node without changes
      SET_VECTOR_ELT(result,n,GetNode_RRef(node_handle));
    } else {
      node_handle = NewNode_bn(name,0,net_handle);
      node = MakeNode_RRef(node_handle,name,FALSE);
      SET_VECTOR_ELT(result,n,node);
    }
  }

  UNPROTECT(1);
  return(result);
}

SEXP RN_Delete_Nodes(SEXP nodelist) {

  R_len_t n, nn = length(nodelist);
  const char* name;
  node_bn* node_handle;
  SEXP node, result;

  PROTECT(result = allocVector(VECSXP,nn));

  for (n=0; n < nn; n++) {
    PROTECT(node = VECTOR_ELT(nodelist,n));
    node_handle = GetNodeHandle(node);
    if (node_handle) {
      RN_Free_Node(node_handle);
      DeleteNode_bn(node_handle);
    } else {
      SET_VECTOR_ELT(result,n,R_NilValue);
      warning("Did not find a node named %s.", NODE_NAME(node));
    }
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return(result);
}


SEXP RN_Find_Node(SEXP net, SEXP namesxp) {
  const char* name;
  net_bn* net_handle;
  node_bn* node_handle;

  name = CHAR(STRING_ELT(namesxp,0));
  net_handle = GetNeticaHandle(net);
  if (net_handle) {
    node_handle = GetNodeNamed_bn(name,net_handle);
    if (node_handle) { /* Found */
      /* Return the node object. */
      return GetNode_RRef(node_handle);
    } else {       /* Not found */
      return R_NilValue;
    }
  } else { /*Error */
    warning("Did not find a network named %s.",BN_NAME(net));
    return NAV;
  }
}

SEXP RN_Network_AllNodes(SEXP net) {
  const char* name;
  net_bn* net_handle;
  nodelist_bn* foundNodes;

  net_handle = GetNeticaHandle(net);
  if (net_handle) {
    foundNodes = GetNetNodes_bn(net_handle);
    if (foundNodes) { /* Found */
      /* Return the node object. */
      return RN_AS_RLIST(foundNodes);
    } else {       /* Null Nodelist, I don't think this should happen */
      warning("All nodes return NULL value for net %s",BN_NAME(net));
      return R_NilValue;
    }
  } else { /*Error */
    warning("Did not find a network named %s.",BN_NAME(net));
    return R_NilValue;
  }
}

/**
 * options should be one of no_links, or no_tables.
 */
SEXP RN_Copy_Nodes(SEXP destNet, SEXP nodelist, SEXP options) {
  R_len_t nn = length(nodelist);
  net_bn *new_net, *old_net;
  char *opt;
  nodelist_bn *old_nodes, *new_nodes;

  new_net = GetNeticaHandle(destNet);
  if (!new_net) {
    error("CopyNodes: Destination network %s is not valid.", BN_NAME(destNet));
  }

  opt = CHAR(STRING_ELT(options,0));
  if (nn) {
    old_net=GetNeticaHandle(VECTOR_ELT(nodelist,0));
  } else {
    old_net = new_net;
  }
  old_nodes = RN_AS_NODELIST(nodelist, old_net);
  new_nodes = CopyNodes_bn(old_nodes, new_net, opt);

  return RN_AS_RLIST(new_nodes);
}

SEXP RN_NodeNet(SEXP node) {
  return GetNet_RRef(GetNodeNet_bn(GetNodeHandle(node)));
}

//////////////////////////////////////////////////////////////////////////
// Getters and Setters for Global Node properties.
//////////////////////////////////////////////////////////////////////////

SEXP RN_GetNodeName(SEXP nd) {
  const char *nodename;
  stream_ns *file;
  node_bn* node_handle;
  SEXP result;

  PROTECT(result = allocVector(STRSXP,1));

  node_handle = GetNodeHandle(nd);
  if (node_handle) {
    nodename = GetNodeName_bn(node_handle);
    SET_STRING_ELT(result,0,mkChar(nodename));
  } else {
    SET_STRING_ELT(result,0,NA_STRING);
    warning("Could not find node %s.",NODE_NAME(nd));
  }
  UNPROTECT(1);
  return(result);
}

SEXP RN_SetNodeName(SEXP nd, SEXP newnames) {
  const char *newname;
  stream_ns *file;
  node_bn *node_handle, *other_node;
  SEXP result;

  node_handle = GetNodeHandle(nd);

  if (node_handle) {
    newname = CHAR(STRING_ELT(newnames,0)); 
    other_node = GetNodeNamed_bn(newname, GetNodeNet_bn(node_handle));
    if ( other_node && other_node != node_handle) {
      warning("There is already a node named %s.",newname);
    } else {
      SetNodeName_bn(node_handle,newname);
      // We need to change the nd object to reflect the new name.
      SET_STRING_ELT(nd,0,mkChar(newname));
      SetNode_RRef(node_handle,nd);
    }
  } else {
    warning("Could not find node %s.",NODE_NAME(nd));
  }
  return(nd);
}

