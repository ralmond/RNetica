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
  int n, nn, nv = (int) length(value);
  SEXP parent;  
  node_bn *oldpar, *newpar;
  node_bn* node_handle = GetNodeHandle(node);
  
  if(node_handle) {
    const nodelist_bn* parlist = GetNodeParents_bn(node_handle);
    int np = LengthNodeList_bn(parlist);
    nn = (np < nv) ? np : nv;

    //Replace nodes
    for (n =0; n< nn; n++) {
      oldpar = NthNode_bn(parlist,n);
      parent = VECTOR_ELT(value,n);
      if (isNull(parent)) {
        newpar = NULL;
      } else {
        newpar = GetNodeHandle(parent);
        if (!newpar) {
          error("NodeParents: Bad parent %s.\n", NODE_NAME(node));
          return(node);
        }
      }
      if (newpar != oldpar) {
        SwitchNodeParent_bn(n,node_handle,newpar);
      } 
    }
    //If necessary, remove additional nodes
    if (np > nv) {
      //Delete Links from end, otherwise numbers will change.
      for (n=np; --n >= nv; ) {
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

  if (deleteme) {
    RN_Free_Nodes(deleteme);  //Will be deleted, free handles
    AbsorbNodes_bn(deleteme);
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
      return NULL;
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


//Expose this Netica constant to R.
SEXP RN_GetEveryState() {
  return ScalarInteger(EVERY_STATE);
}

state_bn *RN_AS_STATE_BN(SEXP states) {
  R_len_t n, nn=length(states);
  if (!nn) return NULL;

  state_bn *result = (state_bn *) R_alloc(nn,sizeof(state_bn));

  PROTECT(states = AS_INTEGER(states));
  for (n=0; n<nn; n++) {
    result[n] = (state_bn) INTEGER(states)[n];
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