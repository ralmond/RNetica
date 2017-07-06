/**
 * Edges.c --- This files contains functions for creating,
 * destroying, and modifying edges and conditional probability
 * tables. 
 */

#include <string.h>
#include <R.h>
#include <Rdefines.h>
#include <RNetica.h>

SEXP RN_AddLink(SEXP parent, SEXP child) {
  node_bn* parent_handle = GetNodeHandle(parent);
  node_bn* child_handle = GetNodeHandle(child);

  if(parent_handle && child_handle) {
    return ScalarInteger(AddLink_bn(parent_handle,child_handle));
  } else {
    if (parent_handle) {
      error("AddLink:  Bad parent %s\n",NODE_NAME(parent));
    } else {
      error("AddLink:  Naughty child %s\n",NODE_NAME(child));
    }
    return ScalarInteger(R_NaInt);
  }
}

SEXP RN_ReverseLink(SEXP parent, SEXP child) {
  node_bn* parent_handle = GetNodeHandle(parent);
  node_bn* child_handle = GetNodeHandle(child);

  if(parent_handle && child_handle) {
    ReverseLink_bn(parent_handle,child_handle);
    return R_NilValue;
  } else {
    if (parent_handle) {
      error("ReverseLink:  Bad parent %s\n",NODE_NAME(parent));
    } else {
      error("ReverseLink:  Naughty child %s\n",NODE_NAME(child));
    }
    return ScalarInteger(R_NaInt);
  }
}

//Unlike Netica version, we will go with the node object.
SEXP RN_DeleteLink(SEXP parent, SEXP child) {
  node_bn* parent_handle = GetNodeHandle(parent);
  node_bn* child_handle = GetNodeHandle(child);

  if(parent_handle && child_handle) {
    int pn= IndexOfNodeInList_bn(parent_handle,
                                 GetNodeParents_bn(child_handle),0);
    if (pn < 0) {
      error("DeleteLink: %s and %s are not connected.\n",
            NODE_NAME(parent), NODE_NAME(child));
      return ScalarInteger(R_NaInt);
    } else {
      DeleteLink_bn(pn,child_handle);
      return child;
    }
  } else {
    if (parent_handle) {
      error("DeleteLink:  Bad parent %s\n",NODE_NAME(parent));
    } else {
      error("DeleteLink:  Naughty child %s\n",NODE_NAME(child));
    }
    return ScalarInteger(R_NaInt);
  }
}

SEXP RN_GetNodeParents(SEXP node) {
  node_bn* node_handle = GetNodeHandle(node);
  const char* inputname;
  SEXP result, names;

  if(node_handle) {
    PROTECT(result = RN_AS_RLIST(GetNodeParents_bn(node_handle)));
    int n, nn=length(result);
    PROTECT(names = allocVector(STRSXP,nn));
    for (n = 0 ; n<nn; n++) {
      inputname = GetNodeInputName_bn(node_handle, n);
      SET_STRING_ELT(names,n,mkChar(inputname));
    }
    namesgets(result,names);
    UNPROTECT(2);
    return result;
  } else {
    error ("NodeParents: Bad node %s.\n", NODE_NAME(node));
    return (R_NilValue);
  }
}

SEXP RN_GetNodeChildren(SEXP node) {
  node_bn* node_handle = GetNodeHandle(node);

  if(node_handle) {
    return RN_AS_RLIST(GetNodeChildren_bn(node_handle));
  } else {
    error ("NodeChildren: Bad node %s.\n", NODE_NAME(node));
    return (R_NilValue);
  }
}
 

SEXP RN_SetNodeParents(SEXP node, SEXP value) {
  R_len_t n, nn, nv = length(value);
  SEXP parent;  
  node_bn *oldpar, *newpar;
  node_bn* node_handle = GetNodeHandle(node);
  
  if(node_handle) {
    const nodelist_bn* parlist = GetNodeParents_bn(node_handle);
    int np = LengthNodeList_bn(parlist);
    nn = (np < nv) ? np : nv;
    //Rprintf("np=%d, nv=%d, nn=%d\n",np,nv,nn);

    //Replace nodes
    for (n =0; n< nn; n++) {
      //Refetch this each time as it may have changed.
      parlist = GetNodeParents_bn(node_handle);
      oldpar = NthNode_bn(parlist,n);
      parent = VECTOR_ELT(value,n);
      if (isNull(parent)) {
        //Rprintf("Setting parent %d to NULL\n",n);
        newpar = NULL;
      } else {
        newpar = GetNodeHandle(parent);
        if (!newpar) {
          error("NodeParents: Bad parent %s.\n", NODE_NAME(node));
          return(node);
        }
        //Rprintf("Setting parent %d to %s\n",n,GetNodeName_bn(newpar));
      }
      if (newpar != oldpar) {
        //Rprintf("Switching parent %d\n",n);
        SwitchNodeParent_bn(n,node_handle,newpar);
      } else {
        //Rprintf("No need to swap %s and %s\n",GetNodeName_bn(oldpar),GetNodeName_bn(newpar));
      }
    }
    //If necessary, remove additional nodes
    if (np > nv) {
      //Delete Links from end, otherwise numbers will change.
      for (n=np; --n >= nv; ) {
        //Rprintf("Switching deleting parent %d\n",n);
        DeleteLink_bn(n,node_handle);
      }
    }
    //If necessary, add additional nodes
    if (nv > np) {
      for (n=np; n < nv; n++) {
        parent = VECTOR_ELT(value,n);
        if (isNull(parent)) {
          error("NodeParents: Padding with null values not allowed.");
          return(R_NilValue);
        } else {
          newpar = GetNodeHandle(parent);
          if (!newpar) {
            error("NodeParents: Bad parent %s.\n", NODE_NAME(node));
            return(node);
          }
        }
        //Rprintf("Adding parent %d\n",n);
        AddLink_bn(newpar,node_handle);
      }
    }
  } else {
    error ("NodeParents: Naughty Child %s.\n", NODE_NAME(node));
  }
  return(node);
}

SEXP RN_AbsorbNodes(SEXP nodelist) {
  nodelist_bn* deleteme = RN_AS_NODELIST(nodelist,NULL);
  int kk=LengthNodeList_bn(deleteme);
  Rprintf("Absorbing %d nodes.\n",kk);
  node_bn* node0 = NthNode_bn(deleteme,0);
  Rprintf("First node address %x.\n",(long) node0);
  const char *nodename;
  nodename = GetNodeName_bn(node0);
  Rprintf("Its name is %s.\n",nodename);

  if (deleteme) {
    AbsorbNodes_bn(deleteme);
    Rprintf("Absorbed.\n");
    RN_Free_Nodelist(nodelist);  //Free handles
  } else {
    error("AbsorbNodes: Could not find affected network.\n");
  }
  return R_NilValue;
}

SEXP RN_IsNodeRelated(SEXP n1, SEXP relation, SEXP n2) {
  node_bn* n1_handle = GetNodeHandle(n1);
  node_bn* n2_handle = GetNodeHandle(n2);
  const char* rel = CHAR(STRING_ELT(relation,0));

  if(n1_handle && n2_handle) {
    return ScalarLogical(IsNodeRelated_bn(n1_handle,rel,n2_handle));
  } else {
    if (n1_handle) {
      error("IsNodeRelated:  Bad node %s\n",NODE_NAME(n1));
    } else {
      error("IsNodeRelated:  Naughty node %s\n",NODE_NAME(n2));
    }
    return ScalarInteger(R_NaInt);
  }
}

//Only bothering with the Mult version.
SEXP RN_GetRelatedNodes(SEXP nodelist, SEXP relation) {
  const char* rel = CHAR(STRING_ELT(relation,0));
  const nodelist_bn* nodes;
  nodelist_bn* related;
  net_bn *net_handle=NULL;
  SEXP result;

  if (length(nodelist)) {
    node_bn *node1 = GetNodeHandle(VECTOR_ELT(nodelist,0));
    if (node1) {
      net_handle = GetNodeNet_bn(node1);
    } else {
      error("as.nodelist: Can't find source network.\n");
      return R_NilValue;
    }
  } else {
    error("as.nodelist: Can't find source network.\n");
  }
  nodes = RN_AS_NODELIST(nodelist,net_handle);
  related = NewNodeList2_bn(0,net_handle);

  GetRelatedNodesMult_bn(related,rel,nodes);
  PROTECT(result=RN_AS_RLIST(related));
  DeleteNodeList_bn(related);
  UNPROTECT(1);
  return result;
  
}

SEXP RN_GetNodeInputNames(SEXP nd) {
  R_len_t n, nn;
  const char *value;
  node_bn* node_handle;
  SEXP result;

  node_handle = GetNodeHandle(nd);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(nd));
    PROTECT(result=allocVector(STRSXP,1));
    SET_STRING_ELT(result,0,NA_STRING);
  } else {
    //Count number of fields.
    nn = LengthNodeList_bn(GetNodeParents_bn(node_handle));
    PROTECT(result = allocVector(STRSXP,nn));

    for (n=0; n < nn; n++) {
      value = GetNodeInputName_bn(node_handle, n);
      SET_STRING_ELT(result,n,mkChar(value));
    }
  }
  UNPROTECT(1);
  return(result);
}


SEXP RN_SetNodeInputNames(SEXP nd, SEXP newvals) {
  R_len_t n, nn = length(newvals);
  const char *value;
  node_bn* node_handle;

  node_handle = GetNodeHandle(nd);

  if (node_handle) {
    for (n=0; n < nn; n++) {
      value = CHAR(STRING_ELT(newvals,n));
      if (strlen(value)==0) value = NULL; //Netica wants NULL not ""
      SetNodeInputName_bn(node_handle, n, value);
    }
  } else {
    warning("Could not find node %s.",NODE_NAME(nd));
  }
  return(nd);
}

//This is a special node that forces all of the other variables into a
//clique.  Loosely patterned off the NeticaEx function FormCliqueWith
SEXP RN_MakeCliqueNode(SEXP nodelist) {
  net_bn* nt;
  node_bn *node_handle, *new_node;
  int i, num_nodes=length(nodelist);
  SEXP cliquenode;

  node_handle = GetNodeHandle(VECTOR_ELT(nodelist,0));
  if (!node_handle) {
    error("Could not find node %s.", NODE_NAME(VECTOR_ELT(nodelist,0)));
    return(R_NilValue);
  }
  nt = GetNodeNet_bn(node_handle);
  new_node = NewNode_bn("CliqueNode*",1,nt);
  for (i=0; i< num_nodes; i++) {
    AddLink_bn(GetNodeHandle(VECTOR_ELT(nodelist,i)),new_node);
  }
  PROTECT(cliquenode = GetNode_RRef(new_node));
  SET_CLASS(cliquenode,cliquenodeclass);
  setAttrib(cliquenode,cliqueatt,nodelist);
  UNPROTECT(1);
  return (cliquenode);
}

//////////////////////////////////////////////////////////////
// Probabilities
/////////////////////////////////////////////////////////////


//Expose this Netica constant to R.
SEXP RN_GetEveryState() {
  // We will later subtract 1 from this, so add 1 here.
  return ScalarInteger(EVERY_STATE+1);
}

state_bn *RN_AS_STATE_BN(SEXP states) {
  R_len_t n, nn=length(states);
  if (!nn) return NULL;

  state_bn *result = (state_bn *) R_alloc(nn,sizeof(state_bn));

  PROTECT(states = AS_INTEGER(states));
  for (n=0; n<nn; n++) {
    result[n] = (state_bn) INTEGER(states)[n]-1;
  }
  UNPROTECT(1);
  return result;
}

prob_bn *RN_AS_PROB_BN(SEXP vals) {
  R_len_t n, nn=length(vals);
  if (!nn) return NULL;

  prob_bn *result = (prob_bn *) R_alloc(nn,sizeof(prob_bn));
  PROTECT(vals = AS_NUMERIC(vals));
  for (n=0; n<nn; n++) {
    if (ISNA(REAL(vals)[n])) {
      return NULL;
    } else {
      result[n] = (prob_bn) REAL(vals)[n];
    }
  }
  UNPROTECT(1);
  return result;
}

SEXP RN_AS_PROBSXP(const prob_bn *vals, int nn) {
  int n;
  SEXP result; 

  if (!vals) return R_NilValue;
  
  PROTECT(result = allocVector(REALSXP,nn));
  for (n=0; n<nn; n++) {
    if (vals[n]==UNDEF_DBL) {
      REAL(result)[n] = NA_REAL;
    } else {
      REAL(result)[n] = vals[n];
    }
  }
  UNPROTECT(1);
  return result;
}


// Return without names, as we may be part of a loop, let 
// R code handle the names.
SEXP RN_GetNodeProbs(SEXP node, SEXP states) {
  node_bn* node_handle;

  node_handle = GetNodeHandle(node);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(node));
    return(R_NilValue);
  } else {
    return RN_AS_PROBSXP(GetNodeProbs_bn(node_handle,
                                         RN_AS_STATE_BN(states)),
                         GetNodeNumberStates_bn(node_handle));
  }
}

SEXP RN_SetNodeProbs(SEXP node, SEXP states, SEXP vals) {
  node_bn* node_handle;

  node_handle = GetNodeHandle(node);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(node));
  } else {
    SetNodeProbs_bn(node_handle, RN_AS_STATE_BN(states),
                    RN_AS_PROB_BN(vals));
  }
  return node;
}

SEXP RN_IsNodeDeterministic(SEXP n1) {
  node_bn* n1_handle = GetNodeHandle(n1);

  if(n1_handle) {
    return ScalarLogical(IsNodeDeterministic_bn(n1_handle));
  } else {
    error("IsNodeDeterministic:  Naughty node %s\n",NODE_NAME(n1));
    return ScalarInteger(R_NaInt);
  }
}

SEXP RN_HasNodeTable(SEXP n1) {
  node_bn* n1_handle = GetNodeHandle(n1);
  bool_ns complete;
  SEXP result;
  PROTECT(result = allocVector(LGLSXP,2));

  if(n1_handle) {
    LOGICAL(result)[0] = HasNodeTable_bn(n1_handle, &complete);
    LOGICAL(result)[1] = complete;
  } else {
    error("HasNodeTable:  Naughty node %s\n",NODE_NAME(n1));
    LOGICAL(result)[0] = R_NaInt;
    LOGICAL(result)[1] = R_NaInt;
  }
  UNPROTECT(1);
  return result;
}

SEXP RN_DeleteNodeTable(SEXP n1) {
  node_bn* n1_handle = GetNodeHandle(n1);

  if(n1_handle) {
    DeleteNodeTables_bn(n1_handle);
  } else {
    error("DeleteNodeTables:  Naughty node %s\n",NODE_NAME(n1));
    return ScalarInteger(R_NaInt);
  }
  return n1;
}


SEXP RN_GetNodeFuncState(SEXP node, SEXP states) {
  node_bn* node_handle;

  node_handle = GetNodeHandle(node);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(node));
    return(R_NilValue);
  } else {
    return ScalarInteger(GetNodeFuncState_bn(node_handle,
                                             RN_AS_STATE_BN(states))
                         +1);
  }
}

SEXP RN_SetNodeFuncState(SEXP node, SEXP states, SEXP val) {
  node_bn* node_handle;

  node_handle = GetNodeHandle(node);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(node));
  } else {
    SetNodeFuncState_bn(node_handle, RN_AS_STATE_BN(states),
                        INTEGER(val)[0]-1);
  }
  return node;
}


SEXP RN_GetNodeFuncReal(SEXP node, SEXP states) {
  node_bn* node_handle;

  node_handle = GetNodeHandle(node);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(node));
    return(R_NilValue);
  } else {
    return ScalarReal(GetNodeFuncReal_bn(node_handle,
                                             RN_AS_STATE_BN(states)));
  }
}

SEXP RN_SetNodeFuncReal(SEXP node, SEXP states, SEXP val) {
  node_bn* node_handle;

  node_handle = GetNodeHandle(node);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(node));
  } else {
    SetNodeFuncReal_bn(node_handle, RN_AS_STATE_BN(states),
                        REAL(val)[0]);
  }
  return node;
}
