/**
 * Node.c --- This file contains functions for creating,
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
  if (nodePtr && nodePtr != R_NilValue && R_ExternalPtrAddr(nodePtr)) {
    LOGICAL(result)[0] = TRUE;
  }
  UNPROTECT(2);
  return result;
}

/**
 * Tests whether or not an object is a Netica Node.
 */
int isNeticaNode(SEXP obj) {
  SEXP klass;
  int result = FALSE;
  PROTECT(klass = getAttrib(obj,R_ClassSymbol));
  R_len_t k, kk=length(klass);
  for (k=0; k<kk; k++) {
    if(strcmp(NodeClass,CHAR(STRING_ELT(klass,k))) == 0) {
      result =TRUE;
      break;
    } else {
    }
  }
  UNPROTECT(1);
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
  if (nd && isNeticaNode(nd)==TRUE) return nd;            // Already got one
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
  if (nodehandle && nodehandle != R_NilValue) {
    R_ClearExternalPtr(nodehandle);
  }
  setAttrib(node,nodeatt,R_NilValue); //Probably not needed.
  R_ReleaseObject(node); //Let R garbage collect it when all
  //references are gone.
  SetNode_RRef(node_handle,NULL);
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
SEXP RN_AS_RLIST(const nodelist_bn* nodelist) {
  int k, kk=LengthNodeList_bn(nodelist);
  SEXP result;

  PROTECT(result = allocVector(VECSXP, (R_len_t) kk));
  for (k=0; k<kk; k++) {
    SET_VECTOR_ELT(result,(R_len_t) k,GetNode_RRef(NthNode_bn(nodelist,k)));
  }
  UNPROTECT(1);
  return result;
}

//If net_handle is null, will try to figure ot from first node in
//list.

nodelist_bn* RN_AS_NODELIST(SEXP nodes, net_bn* net_handle) {
  R_len_t n, nn = length(nodes);
  SEXP node;
  
  if (!net_handle) { 
    if (nn) {
      node_bn *node1 = GetNodeHandle(VECTOR_ELT(nodes,0));
      if (node1) {
        net_handle = GetNodeNet_bn(node1);
      } else {
        error("as.nodelist: Can't find source network.\n");
        return NULL;
      }
    } else {
      error("as.nodelist: Can't find source network.\n");
      return NULL;
    }
  }

  nodelist_bn* result = NewNodeList2_bn(nn,net_handle);
  
  for (n=0; n<nn; n++) {
    PROTECT(node = VECTOR_ELT(nodes,n));
    if (isNull(node)) {
      SetNthNode_bn(result,n,NULL);
    } else {
      SetNthNode_bn(result,n,GetNodeHandle(node));
    }
    UNPROTECT(1);
  }
  return result;
}

// Frees pointers from R-objects, assumes nodes have been deleted elsewhere.
void RN_Free_Nodelist(SEXP nodes) {
  R_len_t n, nn = length(nodes);
  SEXP node, nodehandle;
  
  for (n=0; n<nn; n++) {
    PROTECT(node = VECTOR_ELT(nodes,n));
    if (!isNull(node)) {
      PROTECT(nodehandle = getAttrib(node,nodeatt));
      /* Clear the handle */
      if (nodehandle && nodehandle != R_NilValue) {
        R_ClearExternalPtr(nodehandle);
      }
      setAttrib(node,nodeatt,R_NilValue); //Probably not needed.
      R_ReleaseObject(node); //Let R garbage collect it when all
      UNPROTECT(1);
    }
    UNPROTECT(1);
  }
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
  node_bn* node_handle;
  SEXP node, result;

  PROTECT(result = allocVector(VECSXP,nn));

  for (n=0; n < nn; n++) {
    PROTECT(node = VECTOR_ELT(nodelist,n));
    node_handle = GetNodeHandle(node);
    if (node_handle) {
      RN_Free_Node(node_handle);
      DeleteNode_bn(node_handle);
      SET_VECTOR_ELT(result,n,node);
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
  net_bn* net_handle;
  const nodelist_bn* foundNodes;

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
  net_bn *new_net;
  const char *opt;
  nodelist_bn *old_nodes;
  const nodelist_bn *new_nodes;

  new_net = GetNeticaHandle(destNet);
  if (!new_net) {
    error("CopyNodes: Destination network %s is not valid.", BN_NAME(destNet));
  }

  opt = CHAR(STRING_ELT(options,0));

  old_nodes = RN_AS_NODELIST(nodelist, NULL);
  new_nodes = CopyNodes_bn(old_nodes, new_net, opt);
  DeleteNodeList_bn(old_nodes);

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
  node_bn *node_handle, *other_node;

  node_handle = GetNodeHandle(nd);

  if (node_handle) {
    newname = CHAR(STRING_ELT(newnames,0)); 
    other_node = GetNodeNamed_bn(newname, GetNodeNet_bn(node_handle));
    if ( other_node ) {
      if (other_node != node_handle) {
        warning("There is already a node named %s.",newname);
      } else {
        //We are renaming this node to itself, probably to fix
        //a bad cached name.  Return the correct R object.
        nd = GetNode_RRef(other_node);
      }
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

SEXP RN_GetNodeTitle(SEXP nd) {
  const char *title;
  node_bn* node_handle;
  SEXP result;

  PROTECT(result = allocVector(STRSXP,1));

  node_handle = GetNodeHandle(nd);

  if (node_handle) {
    title = GetNodeTitle_bn(node_handle);
    SET_STRING_ELT(result,0,mkChar(title));
  } else {
    SET_STRING_ELT(result,0,NA_STRING);
    warning("Could not find node %s.",NODE_NAME(nd));
  }
  UNPROTECT(1);
  return(result);
}

SEXP RN_SetNodeTitle(SEXP nd, SEXP newtitle) {
  const char *title;
  node_bn* node_handle;

  node_handle = GetNodeHandle(nd);

  if (node_handle) {
    title = CHAR(STRING_ELT(newtitle,0));
    SetNodeTitle_bn(node_handle,title);
  } else {
    warning("Could not find node %s.",NODE_NAME(nd));
  }
  return(nd);
}

SEXP RN_GetNodeComment(SEXP nd) {
  const char *comment;
  node_bn* node_handle;
  SEXP result;

  PROTECT(result = allocVector(STRSXP,1));

  node_handle = GetNodeHandle(nd);
  if (node_handle) {
    comment = GetNodeComment_bn(node_handle);
    SET_STRING_ELT(result,0,mkChar(comment));
  } else {
    SET_STRING_ELT(result,0,NA_STRING);
    warning("Could not find node %s.",NODE_NAME(nd));
  }
  UNPROTECT(1);
  return(result);
}

SEXP RN_SetNodeComment(SEXP nd, SEXP newcomment) {
  const char *comment;
  node_bn* node_handle;

  node_handle = GetNodeHandle(nd);

  if (node_handle) {
    comment = CHAR(STRING_ELT(newcomment,0));
    SetNodeComment_bn(node_handle,comment);
  } else {
    warning("Could not find node %s.",NODE_NAME(nd));
  }
  return(nd);
}

SEXP RN_GetNodeUserField(SEXP nd, SEXP fieldnames) {
  const char *value, *fieldname;
  int valuelen;
  node_bn* node_handle;
  SEXP result;

  PROTECT(result = allocVector(STRSXP,1));

  node_handle = GetNodeHandle(nd);

  if (node_handle) {
    fieldname = CHAR(STRING_ELT(fieldnames,0));
    value = GetNodeUserField_bn(node_handle,fieldname,&valuelen,0);
    if (valuelen<0) { // No object returned.
      SET_STRING_ELT(result,0,NA_STRING);
    } else {
      SET_STRING_ELT(result,0,mkChar(value));
    }
  } else {
    SET_STRING_ELT(result,0,NA_STRING);
    warning("Could not find node %s.",NODE_NAME(nd));
  }
  UNPROTECT(1);
  return(result);
}

SEXP RN_GetAllNodeUserFields(SEXP nd) {
  R_len_t n, nn;
  const char *value, *fieldname;
  int valuelen;
  node_bn* node_handle;
  SEXP result, fieldnames;

  node_handle = GetNodeHandle(nd);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(nd));
    PROTECT(result=allocVector(STRSXP,1));
    SET_STRING_ELT(result,0,NA_STRING);
  } else {
    //Count number of fields.
    nn = 0;
    while (TRUE) {
      GetNodeNthUserField_bn(node_handle, nn, &fieldname, &value,
                         &valuelen, 0);
      if (strlen(fieldname) <1 && valuelen <0) break;
      nn++;
    }
    PROTECT(result = allocVector(STRSXP,nn));
    PROTECT(fieldnames = allocVector(STRSXP,nn));

    for (n=0; n < nn; n++) {
      GetNodeNthUserField_bn(node_handle, n, &fieldname, &value,
                         &valuelen, 0);
      SET_STRING_ELT(fieldnames,n,mkChar(fieldname));
      SET_STRING_ELT(result,n,mkChar(value));
    }
    namesgets(result,fieldnames);
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return(result);
}


SEXP RN_SetNodeUserField(SEXP nd, SEXP fieldnames, SEXP newvals) {
  const char *value, *fieldname;
  node_bn* node_handle;

  node_handle = GetNodeHandle(nd);

  if (node_handle) {
    fieldname = CHAR(STRING_ELT(fieldnames,0));
    value = CHAR(STRING_ELT(newvals,0));
    SetNodeUserField_bn(node_handle,fieldname,value, strlen(value),0);
  } else {
    warning("Could not find node %s.",NODE_NAME(nd));
  }
  return(nd);
}


SEXP RN_GetNodeKind(SEXP nd) {
  nodekind_bn kind;
  node_bn* node_handle;
  SEXP result;

  PROTECT(result = allocVector(STRSXP,1));

  node_handle = GetNodeHandle(nd);
  if (node_handle) {
    kind = GetNodeKind_bn(node_handle);
    SET_STRING_ELT(result,0,RN_KindToChar(kind));
  } else {
    SET_STRING_ELT(result,0,NA_STRING);
    warning("Could not find node %s.",NODE_NAME(nd));
  }
  UNPROTECT(1);
  return(result);
}

SEXP RN_SetNodeKind(SEXP nd, SEXP newkind) {
  node_bn* node_handle;

  node_handle = GetNodeHandle(nd);

  if (node_handle) {
    SetNodeKind_bn(node_handle,RN_CharToKind(STRING_ELT(newkind,0)));
  } else {
    warning("Could not find node %s.",NODE_NAME(nd));
  }
  return(nd);
}


SEXP RN_GetNodeVisStyle(SEXP nd) {
  const char *visStyle;
  node_bn* node_handle;
  SEXP result;

  PROTECT(result = allocVector(STRSXP,1));

  node_handle = GetNodeHandle(nd);
  if (node_handle) {
    visStyle = GetNodeVisStyle_bn(node_handle,NULL);
    SET_STRING_ELT(result,0,mkChar(visStyle));
  } else {
    SET_STRING_ELT(result,0,NA_STRING);
    warning("Could not find node %s.",NODE_NAME(nd));
  }
  UNPROTECT(1);
  return(result);
}

SEXP RN_SetNodeVisStyle(SEXP nd, SEXP newvisStyle) {
  const char *visStyle;
  node_bn* node_handle;

  node_handle = GetNodeHandle(nd);

  if (node_handle) {
    visStyle = CHAR(STRING_ELT(newvisStyle,0));
    SetNodeVisStyle_bn(node_handle,NULL,visStyle);
  } else {
    warning("Could not find node %s.",NODE_NAME(nd));
  }
  return(nd);
}

SEXP RN_GetNodeVisPos(SEXP nd) {
  double x,y;
  node_bn* node_handle;
  SEXP result;

  PROTECT(result = allocVector(REALSXP,2));
  namesgets(result,XYnames);

  node_handle = GetNodeHandle(nd);

  if (node_handle) {
    GetNodeVisPosition_bn(node_handle,NULL,&x,&y);
    REAL(result)[0] = x;
    REAL(result)[1] = y;
  } else {
    warning("Could not find node %s.",NODE_NAME(nd));
    REAL(result)[0] = R_NaReal;
    REAL(result)[1] = R_NaReal;
  }
  UNPROTECT(1);
  return(result);
}

SEXP RN_SetNodeVisPos(SEXP nd, SEXP newPos) {
  double x,y;
  node_bn* node_handle;
  
  PROTECT(newPos = AS_NUMERIC(newPos));

  node_handle = GetNodeHandle(nd);
  x = REAL(newPos)[0];
  y = REAL(newPos)[1];
  if (node_handle) {
    SetNodeVisPosition_bn(node_handle,NULL,x,y);
  } else {
    warning("Could not find node %s.",NODE_NAME(nd));
  }
  UNPROTECT(1);
  return(nd);
}

SEXP RN_GetNodeNumStates(SEXP nd) {
  node_bn* node_handle;
  node_handle = GetNodeHandle(nd);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(nd));
    return ScalarInteger(R_NaInt);
  } 
  return ScalarInteger(GetNodeNumberStates_bn(node_handle));
}

SEXP RN_GetNodeStates(SEXP nd) {
  R_len_t n, nn;
  const char *value, *statename;
  node_bn* node_handle;
  SEXP result, statenames;

  node_handle = GetNodeHandle(nd);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(nd));
    PROTECT(result=allocVector(STRSXP,1));
    SET_STRING_ELT(result,0,NA_STRING);
  } else {
    //Count number of fields.
    nn = GetNodeNumberStates_bn(node_handle);
    PROTECT(result = allocVector(STRSXP,nn));
    PROTECT(statenames = allocVector(STRSXP,nn));

    for (n=0; n < nn; n++) {
      statename = GetNodeStateName_bn(node_handle, n);
      value = statename;
      SET_STRING_ELT(statenames,n,mkChar(statename));
      SET_STRING_ELT(result,n,mkChar(value));
    }
    namesgets(result,statenames);
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return(result);
}


/*
 * Netica expects the state names as a comma separated list,
 * so if we are resizing, we need to pass in the new size.
 */
SEXP RN_SetNodeStates(SEXP nd, SEXP newvals, SEXP newsize) {
  const char *value;
  node_bn* node_handle;
  int n, ni, nn = INTEGER(newsize)[0];

  node_handle = GetNodeHandle(nd);

  if (node_handle) {
    value = CHAR(STRING_ELT(newvals,0));
    n = GetNodeNumberStates_bn(node_handle);
    if (n < nn) { //Too few add states
      AddNodeStates_bn(node_handle,-1,NULL,nn-n, -1.0);
    } else if (n > nn) { //Too long, remove states
      for (ni=n; ni > nn; ) {
        RemoveNodeState_bn(node_handle,--ni);
      }
    }
    SetNodeStateNames_bn(node_handle,value);
  } else {
    warning("Could not find node %s.",NODE_NAME(nd));
  }
  return(nd);
}

SEXP RN_GetNodeStateTitles(SEXP nd) {
  R_len_t n, nn;
  const char *value, *statename;
  node_bn* node_handle;
  SEXP result, statenames;

  node_handle = GetNodeHandle(nd);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(nd));
    PROTECT(result=allocVector(STRSXP,1));
    SET_STRING_ELT(result,0,NA_STRING);
  } else {
    //Count number of fields.
    nn = GetNodeNumberStates_bn(node_handle);
    PROTECT(result = allocVector(STRSXP,nn));
    PROTECT(statenames = allocVector(STRSXP,nn));

    for (n=0; n < nn; n++) {
      statename = GetNodeStateName_bn(node_handle, n);
      value = GetNodeStateTitle_bn(node_handle, n);
      SET_STRING_ELT(statenames,n,mkChar(statename));
      SET_STRING_ELT(result,n,mkChar(value));
    }
    namesgets(result,statenames);
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return(result);
}

SEXP RN_SetNodeStateTitles(SEXP nd, SEXP newvals) {
  R_len_t n, nn;
  const char *value;
  node_bn* node_handle;

  node_handle = GetNodeHandle(nd);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(nd));
    return (R_NilValue);
  } else {
    //Count number of fields.
    nn = GetNodeNumberStates_bn(node_handle);

    for (n=0; n < nn; n++) {
      value = CHAR(STRING_ELT(newvals,n));
      SetNodeStateTitle_bn(node_handle, n, value);
    }
  }
  return(nd);
}

SEXP RN_GetNodeStateComments(SEXP nd) {
  R_len_t n, nn;
  const char *value, *statename;
  node_bn* node_handle;
  SEXP result, statenames;

  node_handle = GetNodeHandle(nd);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(nd));
    PROTECT(result=allocVector(STRSXP,1));
    SET_STRING_ELT(result,0,NA_STRING);
  } else {
    //Count number of fields.
    nn = GetNodeNumberStates_bn(node_handle);
    PROTECT(result = allocVector(STRSXP,nn));
    PROTECT(statenames = allocVector(STRSXP,nn));

    for (n=0; n < nn; n++) {
      statename = GetNodeStateName_bn(node_handle, n);
      value = GetNodeStateComment_bn(node_handle, n);
      SET_STRING_ELT(statenames,n,mkChar(statename));
      SET_STRING_ELT(result,n,mkChar(value));
    }
    namesgets(result,statenames);
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return(result);
}

SEXP RN_SetNodeStateComments(SEXP nd, SEXP newvals) {
  R_len_t n, nn;
  const char *value;
  node_bn* node_handle;

  node_handle = GetNodeHandle(nd);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(nd));
    return (R_NilValue);
  } else {
    //Count number of fields.
    nn = GetNodeNumberStates_bn(node_handle);

    for (n=0; n < nn; n++) {
      value = CHAR(STRING_ELT(newvals,n));
      SetNodeStateComment_bn(node_handle, n, value);
    }
  }
  return(nd);
}

//R code switches between continuous and discrete versions.
SEXP RN_GetNodeLevelsDiscrete(SEXP nd) {
  R_len_t n, nn;
  const char *statename;
  node_bn* node_handle;
  const level_bn* levels;
  SEXP result, statenames;

  node_handle = GetNodeHandle(nd);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(nd));
    PROTECT(result=ScalarReal(R_NaReal));
  } else {
    //Count number of fields.
    nn = GetNodeNumberStates_bn(node_handle);
    PROTECT(result = allocVector(REALSXP,nn));
    PROTECT(statenames = allocVector(STRSXP,nn));
    levels = GetNodeLevels_bn(node_handle);
    for (n=0; n < nn; n++) {
      statename = GetNodeStateName_bn(node_handle, n);
      SET_STRING_ELT(statenames,n,mkChar(statename));
      if (levels == NULL) {
        REAL(result)[n] = R_NaReal;
      } else {
        REAL(result)[n] = RN_NnumToRnum(levels[n]);
      }
    }
    namesgets(result,statenames);
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return(result);
}
//Switching between discrete and continuous is done at R level.
SEXP RN_GetNodeLevelsContinuous(SEXP nd) {
  node_bn* node_handle;
  const level_bn* levels;
  SEXP result;
  R_len_t n, nn;

  node_handle = GetNodeHandle(nd);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(nd));
    PROTECT(result=ScalarReal(R_NaReal));
  } else {
    //Count number of fields.
    levels = GetNodeLevels_bn(node_handle);
    if (levels == NULL) {
      nn = 0;
    } else {
      nn = GetNodeNumberStates_bn(node_handle)+1;
    }
    PROTECT(result = allocVector(REALSXP,nn));
    for (n=0; n < nn; n++) {
      REAL(result)[n] = RN_NnumToRnum(levels[n]);
    }
  }
  UNPROTECT(1);
  return(result);
}


//Different error checking, but setting routine is similar.
SEXP RN_SetNodeLevels(SEXP nd, SEXP newvals) {
  R_len_t n, nn = length(newvals);
  node_bn* node_handle;
  level_bn* levels;

  node_handle = GetNodeHandle(nd);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(nd));
  } else {
    //Count number of fields.
    if (nn == 0 ) {
      levels = NULL;
    } else {
      levels = (level_bn *) R_alloc(nn,sizeof(level_bn));
      for (n=0; n < nn; n++) {
        levels[n] = RN_RnumToNnum(REAL(newvals)[n]);
      }
    }
    //Continuous have number of states equal to length(newvals)-1
    if (GetNodeType_bn(node_handle) == CONTINUOUS_TYPE && nn > 0) nn--; 
    SetNodeLevels_bn(node_handle, nn, levels);
  }
  return(nd);
}


//////////////////////////////////////////////////////////////////
// Node Sets

SEXP RN_ParseNodeSetString(net_bn *nt, bool_ns incSystem) {
  const char*rawsets=GetAllNodesets_bn(nt,incSystem,NULL);
  if (rawsets==NULL || strlen(rawsets)==0) {
    // Trap for zero length
    return (allocVector(STRSXP,0));
  }
  char *sets = R_alloc(strlen(rawsets),sizeof(char));
  sets = strcpy(sets,rawsets);
  SEXP result;
  int i, nsets = 1;
  
  for (i=0; i<strlen(sets); i++) {
    if (sets[i]==',') nsets++;
  }

  PROTECT(result=allocVector(STRSXP,nsets));
  char *setname =strtok(sets,",");
  i = 0;
  while (setname) {
    SET_STRING_ELT(result,i++,mkChar(setname));
    setname=strtok(NULL,",");
  }
  UNPROTECT(1);
  return (result);
}

SEXP RN_NetworkNodeSets(SEXP net, SEXP incSystem) {
  net_bn *nt = GetNeticaHandle(net);
  return RN_ParseNodeSetString(nt,LOGICAL(incSystem)[0]);
}




SEXP RN_GetNodeSets(SEXP node, SEXP incSystem) {
  node_bn *nd = GetNodeHandle(node);
  net_bn *nt = GetNodeNet_bn(nd);
  SEXP setList, result;
  const char* setname;
  int nsets,i,nfound=0;

  PROTECT(setList = RN_ParseNodeSetString(nt,LOGICAL(incSystem)[0]));
  nsets = length(setList);
  if (nsets==0) { //Empty list, escape!
    UNPROTECT(1);
    return (setList);
  }
  //I'm hoping R will do something sensible with the extra length
  //Set to "", need to trim in R code.
  PROTECT(result = allocVector(STRSXP,nsets));
  for (i=0; i<nsets; i++) {
    setname = CHAR(STRING_ELT(setList,i));
    if(IsNodeInNodeset_bn(nd,setname)) {
      SET_STRING_ELT(result,nfound++,STRING_ELT(setList,i));
    }
  }
  UNPROTECT(2);
  return (result);
}

SEXP RN_SetNodeSets(SEXP node, SEXP sets) {
  node_bn *nd = GetNodeHandle(node);
  net_bn *nt = GetNodeNet_bn(nd);
  SEXP setList;
  const char* setname;
  int i, nsets;

  PROTECT(setList = RN_ParseNodeSetString(nt, FALSE));
  nsets= length(setList);
  for (i=0; i<nsets; i++) {
    setname = CHAR(STRING_ELT(setList,i));
    if(IsNodeInNodeset_bn(nd,setname)) {
      RemoveNodeFromNodeset_bn(nd,setname);
    }
  }
  UNPROTECT(1);
  nsets= length(sets);
  for (i=0; i<nsets; i++) {
    setname = CHAR(STRING_ELT(sets,i));
    AddNodeToNodeset_bn(nd,setname);
  }
  return (node);
}

SEXP RN_NetworkNodesInSet(SEXP net, SEXP set) {
  net_bn *nt = GetNeticaHandle(net);
  SEXP result = R_NilValue;
  const char* setname = CHAR(STRING_ELT(set,0));
  const nodelist_bn *allNodes= GetNetNodes_bn(nt);
  int nnodes = LengthNodeList_bn(allNodes);
  int i;
  for (i=0; i<nnodes; i++) {
    node_bn *nd = NthNode_bn(allNodes,i);
    if (IsNodeInNodeset_bn(nd,setname)) {
      PROTECT(result);
      result = CONS(GetNode_RRef(nd),result);
      UNPROTECT(1);
    }
  }
  
  return (result);
  
}

SEXP RN_NetworkSetPriority(SEXP net, SEXP setlist) {
  net_bn *nt = GetNeticaHandle(net);
  ReorderNodesets_bn(nt,CHAR(STRING_ELT(setlist,0)),NULL);
  return (net);
}

SEXP RN_NetworkNodeSetColor(SEXP net, SEXP set, SEXP color) {
  net_bn *nt = GetNeticaHandle(net);
  const char* setname =CHAR(STRING_ELT(set,0));
  color_ns icol = (color_ns) INTEGER(color)[0];
  icol = SetNodesetColor_bn(setname,icol,nt,NULL);
  return ScalarInteger(icol);
}

SEXP RN_NetworkNodeGetColor(SEXP net, SEXP set) {
  net_bn *nt = GetNeticaHandle(net);
  const char* setname =CHAR(STRING_ELT(set,0));
  color_ns color = SetNodesetColor_bn(setname,QUERY_ns,nt,NULL);
  return ScalarInteger(color);
}
