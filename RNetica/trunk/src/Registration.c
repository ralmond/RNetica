/**
 * Registration.c --- These files register the .C and .Call methods
 * so R can finde them.
 */

#include <string.h>
#include <ctype.h>
#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <RNetica.h>


////////////////////////////////////////////////////////////////////////
// Common Symbols; defined here so we won't keep reallocating them.
///////////////////////////////////////////////////////////////////////

/**
 * Common Symbols so we don't need to keep redefining them.
 */
const char* NeticaClass = "NeticaBN";
const char* BNATT = "Netica_bn";
const char* NodeClass = "NeticaNode";
const char* NODEATT = "Netica_Node";
const char* SDATT = "std_dev";
const char* DISCRETEATT = "node_discrete";
const char* CliqueNodeClass = "CliqueNode";
const char* CLIQUEATT = "clique";
const char* EmptyString = "";
const char* CaseStreamClass = "NeticaCaseStream";
const char* MemoryStreamClass = "MemoryCaseStream";
const char* CaseFileStreamClass = "CaseFileStream";
const char* CASESTREAMATT = "Netica_Case_Stream";
const char* CASESTREAMPOSATT = "Case_Stream_Position";
const char* CASESTREAMPATHATT = "Case_Stream_Path";
const char* CASESTREAMLASTIDATT = "Case_Stream_Lastid";
const char* CASESTREAMLASTFREQATT = "Case_Stream_Lastfreq";
const char* CASESTREAMDFATT = "Case_Stream_DataFrame";
const char* CASESTREAMDFNAMEATT = "Case_Stream_DataFrameName";
const char* RNGClass = "NeticaRNG";
const char* RNGATT = "Netica_RNG";



SEXP bnclass=NULL;
SEXP nodeclass=NULL;
SEXP cliquenodeclass=NULL;
SEXP bnatt=NULL;
SEXP nodeatt=NULL;
SEXP sdatt=NULL;
SEXP nodediscatt=NULL;
SEXP cliqueatt=NULL;
SEXP TRUEV=NULL;
SEXP FALSEV=NULL;
SEXP NAV=NULL;
SEXP NodeKinds = NULL;
SEXP XYnames = NULL;
SEXP casestreamclass = NULL;
SEXP memorystreamclass = NULL;
SEXP casefilestreamclass = NULL;
SEXP casestreamatt = NULL;
SEXP casestreamposatt = NULL;
SEXP casestreampathatt = NULL;
SEXP casestreamlastidatt = NULL;
SEXP casestreamlastfreqatt = NULL;
SEXP casestreamdfatt = NULL;
SEXP casestreamdfnameatt = NULL;
SEXP CaseStreamList = NULL;
SEXP rngclass = NULL;
SEXP rngatt = NULL;
SEXP RngList = NULL;


static int symbolRegCount=0;

void RN_Define_Symbols() {
  //printf("RN_Defining_Symbols: %d.\n",symbolRegCount);
  if (bnclass==NULL) {
    bnclass = allocVector(STRSXP,1);
    R_PreserveObject(bnclass);
    SET_STRING_ELT(bnclass,0,mkChar(NeticaClass));
  }
  if (bnatt==NULL) { 
    R_PreserveObject(bnatt = install(BNATT));  
  } 
  if (nodeclass==NULL) {
    R_PreserveObject(nodeclass = allocVector(STRSXP,1));
    SET_STRING_ELT(nodeclass,0,mkChar(NodeClass));
  }
  if (nodeatt==NULL) { 
    R_PreserveObject(nodeatt = install(NODEATT));  
  } 
  if (nodediscatt==NULL) { 
    R_PreserveObject(nodediscatt = install(DISCRETEATT));  
  } 
  if (sdatt==NULL) { 
    R_PreserveObject(sdatt = install(SDATT));  
  } 
  if (TRUEV==NULL) {
    R_PreserveObject(TRUEV = allocVector(LGLSXP,1));
    LOGICAL(TRUEV)[0]=TRUE;
  }
  if (FALSEV==NULL) {
    R_PreserveObject(FALSEV = allocVector(LGLSXP,1));
    LOGICAL(FALSEV)[0]=FALSE;
  }
  if (NAV==NULL) {
    R_PreserveObject(NAV = allocVector(INTSXP,1));
    INTEGER(NAV)[0]=NA_INTEGER;
  }
  if (NodeKinds==NULL) {
    R_PreserveObject(NodeKinds = allocVector(STRSXP,5));
    SET_STRING_ELT(NodeKinds,0,mkChar("Nature"));
    SET_STRING_ELT(NodeKinds,1,mkChar("Decision"));
    SET_STRING_ELT(NodeKinds,2,mkChar("Utility"));
    SET_STRING_ELT(NodeKinds,3,mkChar("Constant"));
    SET_STRING_ELT(NodeKinds,4,mkChar("Stub"));
  }
  if (XYnames==NULL) {
    R_PreserveObject(XYnames = allocVector(STRSXP,2));
    SET_STRING_ELT(XYnames,0,mkChar("x"));
    SET_STRING_ELT(XYnames,1,mkChar("y"));
  }
  if (cliquenodeclass==NULL) {
    R_PreserveObject(cliquenodeclass = allocVector(STRSXP,2));
    SET_STRING_ELT(cliquenodeclass,1,mkChar(NodeClass));
    SET_STRING_ELT(cliquenodeclass,0,mkChar(CliqueNodeClass));
  }
  if (cliqueatt==NULL) { 
    R_PreserveObject(cliqueatt = install(CLIQUEATT));  
  } 
  if (casestreamclass==NULL) {
    casestreamclass = allocVector(STRSXP,1);
    R_PreserveObject(casestreamclass);
    SET_STRING_ELT(casestreamclass,0,mkChar(CaseStreamClass));
  }
  if (memorystreamclass==NULL) {
    memorystreamclass = allocVector(STRSXP,2);
    R_PreserveObject(memorystreamclass);
    SET_STRING_ELT(memorystreamclass,0,mkChar(MemoryStreamClass));
    SET_STRING_ELT(memorystreamclass,1,mkChar(CaseStreamClass));
  }
  if (casefilestreamclass==NULL) {
    casefilestreamclass = allocVector(STRSXP,2);
    R_PreserveObject(casefilestreamclass);
    SET_STRING_ELT(casefilestreamclass,0,mkChar(CaseFileStreamClass));
    SET_STRING_ELT(casefilestreamclass,1,mkChar(CaseStreamClass));
  }
  if (casestreamatt==NULL) { 
    R_PreserveObject(casestreamatt = install(CASESTREAMATT));  
  } 
  if (casestreamposatt==NULL) { 
    R_PreserveObject(casestreamposatt = install(CASESTREAMPOSATT));  
  } 
  if (casestreampathatt==NULL) { 
    R_PreserveObject(casestreampathatt = install(CASESTREAMPATHATT));  
  } 
  if (casestreamlastidatt==NULL) { 
    R_PreserveObject(casestreamlastidatt = install(CASESTREAMLASTIDATT));  
  } 
  if (casestreamlastfreqatt==NULL) { 
    R_PreserveObject(casestreamlastfreqatt = install(CASESTREAMLASTFREQATT));  
  } 
  if (casestreamdfatt==NULL) { 
    R_PreserveObject(casestreamdfatt = install(CASESTREAMDFATT));  
  } 
  if (casestreamdfnameatt==NULL) { 
    R_PreserveObject(casestreamdfnameatt = install(CASESTREAMDFNAMEATT));  
  } 
  if (CaseStreamList==NULL) { 
    R_PreserveObject(CaseStreamList = CONS(R_NilValue, R_NilValue));
  } 
  if (rngclass==NULL) {
    rngclass = allocVector(STRSXP,1);
    R_PreserveObject(rngclass);
    SET_STRING_ELT(rngclass,0,mkChar(RNGClass));
  }
  if (rngatt==NULL) { 
    R_PreserveObject(rngatt = install(RNGATT));  
  } 
  if (RngList==NULL) { 
    R_PreserveObject(RngList = CONS(R_NilValue, R_NilValue));
  } 
  //printf("RN_Defining_Symbols: done.\n");
  symbolRegCount++;
}

void RN_Free_Symbols() {
  //printf("RN_Free_Symbols: %d.\n",symbolRegCount);
  if (--symbolRegCount == 0) {
    if (CaseStreamList!=NULL) { 
      CloseOpenCaseStreams();
      R_ReleaseObject(CaseStreamList);
    } 
    if (RngList!=NULL) { 
      FreeRNGs();
      R_ReleaseObject(RngList);
    } 
    if (bnclass != NULL) {
      R_ReleaseObject(bnclass);
      bnclass = NULL;
    }
    if (bnatt != NULL) { 
      R_ReleaseObject(bnatt); 
      bnatt = NULL; 
    } 
    if (nodeclass != NULL) {
      R_ReleaseObject(nodeclass);
      nodeclass = NULL;
    }
    if (nodeatt != NULL) { 
      R_ReleaseObject(nodeatt); 
      nodeatt = NULL; 
    } 
    if (sdatt != NULL) { 
      R_ReleaseObject(sdatt); 
      sdatt = NULL; 
    } 
    if (nodediscatt != NULL) { 
      R_ReleaseObject(nodediscatt); 
      nodediscatt = NULL; 
    } 
    if (TRUEV != NULL) { 
      R_ReleaseObject(TRUEV); 
      TRUEV = NULL; 
    } 
    if (FALSEV != NULL) { 
      R_ReleaseObject(FALSEV); 
      FALSEV = NULL; 
    } 
    if (NAV != NULL) { 
      R_ReleaseObject(NAV); 
      NAV = NULL; 
    } 
    if (NodeKinds != NULL) { 
      R_ReleaseObject(NodeKinds); 
      NodeKinds = NULL; 
    } 
    if (XYnames != NULL) { 
      R_ReleaseObject(XYnames); 
      XYnames = NULL; 
    } 
    if (cliquenodeclass != NULL) {
      R_ReleaseObject(cliquenodeclass);
      cliquenodeclass = NULL;
    }
    if (cliqueatt != NULL) { 
      R_ReleaseObject(cliqueatt); 
      cliqueatt = NULL; 
    } 
    if (casestreamclass!=NULL) {
      R_ReleaseObject(casestreamclass);
    }
    if (memorystreamclass!=NULL) {
      R_ReleaseObject(memorystreamclass);
    }
    if (casefilestreamclass!=NULL) {
      R_ReleaseObject(casefilestreamclass);
    }
    if (casestreamatt!=NULL) { 
      R_ReleaseObject(casestreamatt);
    } 
    if (casestreamposatt!=NULL) { 
      R_ReleaseObject(casestreamposatt);
    } 
    if (casestreampathatt!=NULL) { 
      R_ReleaseObject(casestreampathatt);
    } 
    if (casestreamlastidatt!=NULL) { 
      R_ReleaseObject(casestreamlastidatt);
    } 
    if (casestreamlastfreqatt!=NULL) { 
      R_ReleaseObject(casestreamlastfreqatt);
    } 
    if (casestreamdfatt!=NULL) { 
      R_ReleaseObject(casestreamdfatt);
    } 
    if (casestreamdfnameatt!=NULL) { 
      R_ReleaseObject(casestreamdfnameatt);
    } 
    if (rngclass != NULL) {
      R_ReleaseObject(rngclass);
      rngclass = NULL;
    }
    if (rngatt != NULL) { 
      R_ReleaseObject(rngatt); 
      rngatt = NULL; 
    } 
  }
}

/*****************************************************************************
 * Starting and Stopping Netica
 *****************************************************************************/


/**
 * This is a global pointer to the Netica environment.
 * It is created once during a session.
 */
environ_ns* RN_netica_env = NULL;


/**
 * Launchs the Netica Environment.
 *  -- License:  Licence key from Norsys, or NULL for demo mode.
 *  -- Checking:  One of ("NO_CHECK", "QUICK_CHECK", "REGULAR_CHECK",
 *       "COMPLETE_CHECK")
 *  -- maxmem:  Mamximum size for Netica memory.  0 uses Netica
 * defaults.
 */
void RN_start_Netica(char** license, char** checking, double* maxmem) {

  char mesg[MESG_LEN_ns];
  int res;

  //Now called on library init.
  RN_Define_Symbols();

  if (RN_netica_env != NULL) {
    warning("Netica already running, use stopNetica before restarting Netica with new parameters.");
    return;
  }
  char* lic = NULL;
  if (license != NULL) {
    lic = license[0];
  }
  RN_netica_env = NewNeticaEnviron_ns(lic,NULL,NULL);

  if(!RN_netica_env) {
    error("Netica License Key not accepted. \n Make sure key starts with a + and ends with five digit security code.");
  }

  res = InitNetica2_bn(RN_netica_env,mesg);
  if (res < 0) {
    error("%s",mesg);
    return;
  }
  Rprintf("%s\n",mesg);

  if (checking != NULL) {
    checking_ns do_check = REGULAR_CHECK;
    if (strcmp(checking[0],"NO_CHECK")==0) {
      do_check = NO_CHECK;
    } else  if (strcmp(checking[0],"QUICK_CHECK")==0) {
      do_check = QUICK_CHECK;
    } else  if (strcmp(checking[0],"REGULAR_CHECK")==0) {
      do_check = REGULAR_CHECK;
    } else  if (strcmp(checking[0],"COMPLETE_CHECK")==0) {
      do_check = COMPLETE_CHECK;
    } else {
      warning("Unknown argument checking type %s",checking[0]);
    }
    ArgumentChecking_ns(do_check,RN_netica_env);
  }

  //It appears that even though I am passing NULL, it is showing up as
  //an array with a very low value.  I've just added a minimum
  //check
  if (maxmem != NULL && maxmem[0]>200000) {
    
    //[DEBUG] printf("Maximizing Memory, %e.\n",maxmem[0]);
    LimitMemoryUsage_ns(maxmem[0],RN_netica_env);
  }

  return;
}

/**
 * This function closes Netica cleanly.
 */
void RN_stop_Netica() {

  char mesg[MESG_LEN_ns];
  int res;


  if (RN_netica_env == NULL) {
    warning("Netica not running, nothing to do.");
    return;
  }


  Rprintf("Shut down any remaining nets.\n");
  int nth = 0;
  net_bn* net;
  SEXP bn, bnPointer;
  while (TRUE) {
    net = GetNthNet_bn (nth++, RN_netica_env);
    if (!net) break;
    RN_Free_Nodes(GetNetNodes_bn(net));
    PROTECT(bn = (SEXP) GetNetUserData_bn(net,0));
    PROTECT(bnPointer = getAttrib(bn,bnatt));
    R_ClearExternalPtr(bnatt);
    UNPROTECT(2);
  } 

  res = CloseNetica_bn(RN_netica_env,mesg);
  RN_netica_env = NULL; //Set to null no matter what.
  if (res < 0) {
    error("%s",mesg);
  }
  RN_Free_Symbols();
  return;
}


SEXP RN_Netica_Version() {
  SEXP result, vnum, vstring, names;
  const char *vs;
  PROTECT(result = allocVector(VECSXP,2));
  PROTECT(vnum = allocVector(INTSXP,1));
  PROTECT(names = allocVector(STRSXP,2));

  INTEGER(vnum)[0]= GetNeticaVersion_bn(RN_netica_env,&vs);
  PROTECT(vstring = allocVector(STRSXP,1));
  SET_STRING_ELT(vstring,0,mkChar(vs));

  SET_VECTOR_ELT(result,0,vnum);
  SET_STRING_ELT(names, 0,mkChar("number"));
  SET_VECTOR_ELT(result,1, vstring);
  SET_STRING_ELT(names, 1,mkChar("message"));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(4);
  return result;

}

/*****************************************************************************
 * Error Reporting
 *****************************************************************************/

/*
 *There is probably a more elegant interface using .Call which returns
 *all of the error messages.  This is a sufficient solution which does
 *not require switching between R and .C strings.
 */

/**
 * Prints the errors using Rprintf.
 * maxreport -- if supplied stops after reporting maxreport errors.
 * clear -- if supplied should be a boolean indicating if errors
 * should be cleared.  Default is true.
 * counts -- should be a vector of length 4, giving the number of
 * errors, warnings, notices and reports.
 */
void RN_report_errors(int* maxreport, int* clear, int* counts) {
  int maxerr = *maxreport;
  int clearit = *clear;

  report_ns* err = NULL;
  int ecount = 0;

  counts[0] = 0;
  while ((err = GetError_ns(RN_netica_env, XXX_ERR, err))!=NULL) {
    Rprintf("Fatal Netica error: %s\n",ErrorMessage_ns(err));
    ecount++;
    counts[0]++;
    if (clearit) ClearError_ns(err);
  }
  if (ecount >0) {
    error("Fatal errors encountered, recommend restarting Netica");
  }

  while ((err = GetError_ns(RN_netica_env, ERROR_ERR, err))!=NULL) {
    Rprintf("Netica error: %s\n",ErrorMessage_ns(err));
    counts[0]++;
    if (ecount++ > maxerr) return;
    if (clearit) ClearError_ns(err);
  }
  
  counts[1]=0;
  while ((err = GetError_ns(RN_netica_env, WARNING_ERR, err))!=NULL) {
    Rprintf("Netica warning: %s\n",ErrorMessage_ns(err));
    counts[1]++;
    if (ecount++ > maxerr) return;
    if (clearit) ClearError_ns(err);
  }

  counts[2]=0;
  while ((err = GetError_ns(RN_netica_env, NOTICE_ERR, err))!=NULL) {
    Rprintf("Netica warning: %s\n",ErrorMessage_ns(err));
    counts[2]++;
    if (ecount++ > maxerr) return;
    if (clearit) ClearError_ns(err);
  }

  counts[3] = 0;
  while ((err = GetError_ns(RN_netica_env, NOTICE_ERR, err))!=NULL) {
    Rprintf("Netica warning: %s\n",ErrorMessage_ns(err));
    counts[3]++;
    if (ecount++ > maxerr) return;
    if (clearit) ClearError_ns(err);
  }

  return;
}


/**
 * Clears all errors at a given severity (and lower?)
 * sev -- should be either NULL (all arguments) or a single character
 * string, one of "NOTHING_ERR", "REPORT_ERR", "NOTICE_ERR", 
 * "WARNING_ERR", "ERROR_ERR", or "XXX_ERR"
 */
void RN_ClearAllErrors(char** sev) {
  errseverity_ns etype = XXX_ERR;

  if (sev != NULL) {
    if (strcmp(sev[0],"NOTHING_ERR")==0) {
      etype = NOTHING_ERR;
    } else  if (strcmp(sev[0],"REPORT_ERR")==0) {
      etype = REPORT_ERR;
    } else  if (strcmp(sev[0],"NOTICE_ERR")==0) {
      etype = NOTICE_ERR;
    } else  if (strcmp(sev[0],"WARNING_ERR")==0) {
      etype = WARNING_ERR;
    } else  if (strcmp(sev[0],"ERROR_ERR")==0) {
      etype = ERROR_ERR;
    } else  if (strcmp(sev[0],"XXX_ERR")==0) {
      etype = XXX_ERR;
    } else {
      warning("Unknown error type %s, no errors cleared",sev[0]);
      etype = NOTHING_ERR;
    }
  }
  ClearErrors_ns(RN_netica_env,etype);

}

/////////////////////////////////////////////////////////////////
// Translation Utilities
/////////////////////////////////////////////////////////////////

SEXP RN_KindToChar (nodekind_bn kind) {
  switch (kind) {
  case NATURE_NODE:
    return STRING_ELT(NodeKinds,0);
  case DECISION_NODE:
    return STRING_ELT(NodeKinds,1);
  case UTILITY_NODE:
    return STRING_ELT(NodeKinds,2);
  case CONSTANT_NODE:
    return STRING_ELT(NodeKinds,3);
  case DISCONNECTED_NODE:
    return STRING_ELT(NodeKinds,4);
  default:
    error("Unknown node kind");
    return(NA_STRING);
  }

}

nodekind_bn RN_CharToKind (SEXP csxp) {
  const char* kind = CHAR(csxp);
  if (isNull(csxp)) {
    error("Illegal node kind");
    return 0;
  }
  switch (toupper(kind[0])) {
  case 'N':
    return NATURE_NODE;
  case 'D':
    return DECISION_NODE;
  case 'U':
    return UTILITY_NODE;
  case 'C':
    return CONSTANT_NODE;
  case 'S': //S for stub, rather than D for Disconnected
    return DISCONNECTED_NODE;
  default:
    error("Unknown node kind");
    return 0;
  }

}

level_bn RN_RnumToNnum (double x) {
  double result = (double) x;
  if (x == R_PosInf) result = INFINITY_ns;
  if (x == R_NegInf) result = -INFINITY_ns;
  return result;
}

double RN_NnumToRnum (level_bn x) {
  double result = (double) x;
  if (x == INFINITY_ns) result = R_PosInf;
  if (x == -INFINITY_ns) result = R_NegInf;
  return result;
}





/////////////////////////////////////////////////////////////////////
// .Call Methods
////////////////////////////////////////////////////////////////
// File = Networks.c
extern SEXP RN_isBNActive(SEXP net);
extern SEXP RN_New_Nets(SEXP namelist);
extern SEXP RN_Delete_Nets(SEXP netlist);
extern SEXP RN_Named_Nets(SEXP namelist);
extern SEXP RN_GetNth_Nets(SEXP nlist);
extern SEXP RN_Copy_Nets(SEXP nets, SEXP namelist, SEXP options);
extern SEXP RN_Read_Nets(SEXP filelist);
extern SEXP RN_Write_Nets(SEXP nets, SEXP filelist);
extern SEXP RN_GetNetFilename(SEXP bn);
extern SEXP RN_GetNetName(SEXP bn);
extern SEXP RN_SetNetName(SEXP bn, SEXP newnames);
extern SEXP RN_GetNetTitle(SEXP bn);
extern SEXP RN_SetNetTitle(SEXP bn, SEXP newtitle);
extern SEXP RN_GetNetComment(SEXP bn);
extern SEXP RN_SetNetComment(SEXP bn, SEXP newcomment);
extern SEXP RN_GetNetAutoUpdate(SEXP bn);
extern SEXP RN_SetNetAutoUpdate(SEXP bn, SEXP newflags);
extern SEXP RN_GetNetUserField(SEXP bn, SEXP fieldnames);
extern SEXP RN_GetAllNetUserFields(SEXP bn);
extern SEXP RN_SetNetUserField(SEXP bn, SEXP fieldnames, SEXP newvals);
extern SEXP RN_Undo(SEXP bn);
extern SEXP RN_Redo(SEXP bn);

// File = Nodes.c
extern SEXP RN_NewDiscreteNodes(SEXP net, SEXP namelist, SEXP nslist,
                               SEXP statelist);
extern SEXP RN_NewContinuousNodes(SEXP net, SEXP namelist);
extern SEXP RN_Delete_Nodes(SEXP nodelist);
extern SEXP RN_Find_Node(SEXP net, SEXP namesxp);
extern SEXP RN_Network_AllNodes(SEXP nodeist);
extern SEXP RN_Copy_Nodes(SEXP destNet, SEXP nodelist, SEXP options);
extern SEXP RN_NodeNet(SEXP node);
extern SEXP RN_GetNodeName(SEXP nd);
extern SEXP RN_SetNodeName(SEXP nd, SEXP newnames);
extern SEXP RN_GetNodeTitle(SEXP nd);
extern SEXP RN_SetNodeTitle(SEXP nd, SEXP newtitle);
extern SEXP RN_GetNodeComment(SEXP nd);
extern SEXP RN_SetNodeComment(SEXP nd, SEXP newcomment);
extern SEXP RN_GetNodeUserField(SEXP nd, SEXP fieldnames);
extern SEXP RN_GetAllNodeUserFields(SEXP nd);
extern SEXP RN_SetNodeUserField(SEXP nd, SEXP fieldnames, SEXP newvals);
extern SEXP RN_GetNodeKind(SEXP nd);
extern SEXP RN_SetNodeKind(SEXP nd, SEXP newKind);
extern SEXP RN_GetNodeVisStyle(SEXP nd);
extern SEXP RN_SetNodeVisStyle(SEXP nd, SEXP newStyle);
extern SEXP RN_GetNodeVisPos(SEXP nd);
extern SEXP RN_SetNodeVisPos(SEXP nd, SEXP newPos);
extern SEXP RN_GetNodeNumStates(SEXP nd);
extern SEXP RN_GetNodeStates(SEXP nd);
extern SEXP RN_SetNodeStates(SEXP nd, SEXP newvals, SEXP newsize);
extern SEXP RN_GetNodeStateTitles(SEXP nd);
extern SEXP RN_SetNodeStateTitles(SEXP nd, SEXP newvals);
extern SEXP RN_GetNodeStateComments(SEXP nd);
extern SEXP RN_SetNodeStateComments(SEXP nd, SEXP newvals);
extern SEXP RN_GetNodeLevelsDiscrete(SEXP nd);
extern SEXP RN_GetNodeLevelsContinuous(SEXP nd);
extern SEXP RN_SetNodeLevels(SEXP nd, SEXP newvals);
extern SEXP RN_NetworkNodeSets(SEXP net, SEXP incSystem);
extern SEXP RN_GetNodeSets(SEXP node, SEXP incSystem);
extern SEXP RN_SetNodeSets(SEXP node, SEXP sets);
extern SEXP RN_NetworkNodesInSet(SEXP net, SEXP set);
extern SEXP RN_NetworkSetPriority(SEXP net, SEXP setlist);
extern SEXP RN_NetworkNodeSetColor(SEXP net, SEXP set, SEXP color);
extern SEXP RN_NetworkNodeGetColor(SEXP net, SEXP set);

//Edges.c
extern SEXP RN_AddLink(SEXP parent, SEXP child);
extern SEXP RN_ReverseLink(SEXP parent, SEXP child);
extern SEXP RN_DeleteLink(SEXP parent, SEXP child);
extern SEXP RN_GetNodeParents(SEXP node);
extern SEXP RN_GetNodeChildren(SEXP node);
extern SEXP RN_SetNodeParents(SEXP node, SEXP value);
extern SEXP RN_GetNodeInputNames(SEXP nd);
extern SEXP RN_SetNodeInputNames(SEXP nd, SEXP newvals);
extern SEXP RN_AbsorbNodes(SEXP nodelist);
extern SEXP RN_IsNodeRelated(SEXP n1, SEXP relation, SEXP n2);
extern SEXP RN_GetRelatedNodes(SEXP nodelist, SEXP relation);
extern SEXP RN_GetEveryState();
extern SEXP RN_GetNodeProbs(SEXP node, SEXP states);
extern SEXP RN_SetNodeProbs(SEXP node, SEXP states, SEXP vals);
extern SEXP RN_IsNodeDeterministic(SEXP n1);
extern SEXP RN_HasNodeTable(SEXP n1);
extern SEXP RN_DeleteNodeTable(SEXP n1);
extern SEXP RN_MakeCliqueNode(SEXP nodelist);

//Inference.c
extern SEXP RN_CompileNet(SEXP net);
extern SEXP RN_UncompileNet(SEXP net);
extern SEXP RN_RetractNetFindings(SEXP net);
extern SEXP RN_GetNodeFinding(SEXP node);
extern SEXP RN_RetractNodeFinding(SEXP node);
extern SEXP RN_SetNodeFinding(SEXP node, SEXP value);
extern SEXP RN_SetNodeFindingNot(SEXP node, SEXP value);
extern SEXP RN_IsBeliefUpdated(SEXP n1);
extern SEXP RN_GetNodeBeliefs(SEXP node);
extern SEXP RN_GetNodeLikelihood(SEXP node);
extern SEXP RN_SetNodeLikelihood(SEXP node, SEXP value);
extern SEXP RN_MostProbableConfig(SEXP net, SEXP nth);
extern SEXP RN_FindingsProbability(SEXP net);
extern SEXP RN_JointProbability(SEXP nodelist);
extern SEXP RN_JunctionTreeReport(SEXP net);
extern SEXP RN_SetEliminationOrder(SEXP net, SEXP order);
extern SEXP RN_GetEliminationOrder(SEXP net);
extern SEXP RN_SizeCompiledNetwork(SEXP net);

// Cases.c
extern SEXP RN_CaseFileDelimiter(SEXP newchar);
extern SEXP RN_MissingCode(SEXP newchar);
extern SEXP RN_WriteFindings(SEXP nodes, SEXP pathOrStream, 
                             SEXP id, SEXP freq);
extern SEXP RN_ReadFindings(SEXP nodes, SEXP stream, 
                             SEXP pos, SEXP add);
extern SEXP RN_isCaseStreamActive(SEXP stream);
extern SEXP RN_OpenCaseFileStream (SEXP path, SEXP stream);
extern SEXP RN_OpenCaseMemoryStream (SEXP label, SEXP stream);
extern SEXP RN_CloseCaseStream(SEXP stream);
extern SEXP RN_SetMemoryStreamContents(SEXP stream, SEXP contents);
extern SEXP RN_GetMemoryStreamContents(SEXP stream);

// Experience.c
extern SEXP RN_GetNodeExperience(SEXP node, SEXP states);
extern SEXP RN_SetNodeExperience(SEXP node, SEXP states, SEXP weight);
extern SEXP RN_FadeCPT(SEXP node, SEXP degree);
extern SEXP RN_LearnFindings(SEXP nodelist, SEXP weight);
extern SEXP RN_LearnCaseStream(SEXP stream, SEXP nodelist, SEXP weight);
extern SEXP RN_LearnCPTs (SEXP caseStream, SEXP nodes, SEXP method, 
                          SEXP maxIters, SEXP maxTol, SEXP weight);

// Continuous.c
extern SEXP RN_GetNodeValue(SEXP node);
extern SEXP RN_SetNodeValue(SEXP node, SEXP value);
extern SEXP RN_SetNodeGaussian(SEXP node, SEXP mean, SEXP sd, 
                               SEXP reset_first);
extern SEXP RN_SetNodeInterval(SEXP node, SEXP low, SEXP high, 
                               SEXP reset_first);
extern SEXP RN_GetNodeExpectedUtils(SEXP node);
extern SEXP RN_GetNodeExpectedValue(SEXP node);
extern SEXP RN_CalcNodeState(SEXP node);
extern SEXP RN_CalcNodeValue(SEXP node);
extern SEXP RN_GetMutualInfo(SEXP target, SEXP nodelist);
extern SEXP RN_GetVarianceOfReal(SEXP target, SEXP nodelist);
extern SEXP RN_GetNodeEquation(SEXP nd);
extern SEXP RN_SetNodeEquation(SEXP nd, SEXP newequation);
extern SEXP RN_EquationToTable(SEXP nd, SEXP numSamples, 
                               SEXP sampUnc, SEXP addExist);


// Cases.c
extern SEXP RN_NewRandomGenerator (SEXP seed);
extern SEXP RN_FreeRNG (SEXP rng);
extern SEXP RN_isRNGActive(SEXP rng);
extern SEXP RN_SetNetRandomGen (SEXP net, SEXP seed);
extern SEXP RN_GenerateRandomCase(SEXP nodelist, SEXP method, 
                                  SEXP timeout, SEXP seed) ;


R_CallMethodDef callMethods[] = {
  {"RN_Netica_Version", (DL_FUNC) &RN_Netica_Version, 0},
  {"RN_isBNActive", (DL_FUNC) &RN_isBNActive, 1},
  {"RN_New_Nets", (DL_FUNC) &RN_New_Nets, 1},
  {"RN_Delete_Nets", (DL_FUNC) &RN_Delete_Nets, 1},
  {"RN_Named_Nets", (DL_FUNC) &RN_Named_Nets, 1},
  {"RN_GetNth_Nets", (DL_FUNC) &RN_GetNth_Nets, 1},
  {"RN_Copy_Nets", (DL_FUNC) &RN_Copy_Nets, 3},
  {"RN_Read_Nets", (DL_FUNC) &RN_Read_Nets, 1},
  {"RN_WriteNets", (DL_FUNC) &RN_Write_Nets, 2},
  {"RN_GetNetFilename", (DL_FUNC) &RN_GetNetFilename, 1},
  {"RN_GetNetName", (DL_FUNC) &RN_GetNetName, 1},
  {"RN_GetNetTitle", (DL_FUNC) &RN_GetNetTitle, 1},
  {"RN_GetNetComment", (DL_FUNC) &RN_GetNetComment, 1},
  {"RN_SetNetName", (DL_FUNC) &RN_SetNetName, 2},
  {"RN_SetNetTitle", (DL_FUNC) &RN_SetNetTitle, 2},
  {"RN_SetNetComment", (DL_FUNC) &RN_SetNetComment, 2},
  {"RN_GetNetAutoUpdate", (DL_FUNC) &RN_GetNetAutoUpdate, 1},
  {"RN_SetNetAutoUpdate", (DL_FUNC) &RN_SetNetAutoUpdate, 2},
  {"RN_GetNetUserField", (DL_FUNC) &RN_GetNetUserField, 2},
  {"RN_SetNetUserField", (DL_FUNC) &RN_SetNetUserField, 3},
  {"RN_GetAllNetUserFields", (DL_FUNC) &RN_GetAllNetUserFields, 1},
  {"RN_Undo", (DL_FUNC) &RN_Undo, 1},
  {"RN_Redo", (DL_FUNC) &RN_Redo, 1},
  {"RN_NewDiscreteNodes", (DL_FUNC) &RN_NewDiscreteNodes, 4},
  {"RN_NewContinuousNodes", (DL_FUNC) &RN_NewContinuousNodes, 2},
  {"RN_Delete_Nodes", (DL_FUNC) &RN_Delete_Nodes, 1},
  {"RN_Find_Node", (DL_FUNC) &RN_Find_Node, 2},
  {"RN_Network_AllNodes", (DL_FUNC) &RN_Network_AllNodes, 1},
  {"RN_Copy_Nodes", (DL_FUNC) &RN_Copy_Nodes, 3},
  {"RN_NodeNet", (DL_FUNC) &RN_NodeNet, 1},
  {"RN_GetNodeName", (DL_FUNC) &RN_GetNodeName, 1},
  {"RN_SetNodeName", (DL_FUNC) &RN_SetNodeName, 2},
  {"RN_GetNodeTitle", (DL_FUNC) &RN_GetNodeTitle, 1},
  {"RN_GetNodeComment", (DL_FUNC) &RN_GetNodeComment, 1},
  {"RN_SetNodeName", (DL_FUNC) &RN_SetNodeName, 2},
  {"RN_SetNodeTitle", (DL_FUNC) &RN_SetNodeTitle, 2},
  {"RN_GetNodeUserField", (DL_FUNC) &RN_GetNodeUserField, 2},
  {"RN_SetNodeUserField", (DL_FUNC) &RN_SetNodeUserField, 3},
  {"RN_GetAllNodeUserFields", (DL_FUNC) &RN_GetAllNodeUserFields, 1},
  {"RN_GetNodeKind", (DL_FUNC) &RN_GetNodeKind, 1},
  {"RN_SetNodeKind", (DL_FUNC) &RN_SetNodeKind, 2},
  {"RN_GetNodeVisStyle", (DL_FUNC) &RN_GetNodeVisStyle, 1},
  {"RN_SetNodeVisStyle", (DL_FUNC) &RN_SetNodeVisStyle, 2},
  {"RN_GetNodeVisPos", (DL_FUNC) &RN_GetNodeVisPos, 1},
  {"RN_SetNodeVisPos", (DL_FUNC) &RN_SetNodeVisPos, 2},
  {"RN_GetNodeStates", (DL_FUNC) &RN_GetNodeStates, 1},
  {"RN_GetNodeNumStates", (DL_FUNC) &RN_GetNodeNumStates, 1},
  {"RN_SetNodeStates", (DL_FUNC) &RN_SetNodeStates, 3},
  {"RN_GetNodeStateTitles", (DL_FUNC) &RN_GetNodeStateTitles, 1},
  {"RN_SetNodeStateTitles", (DL_FUNC) &RN_SetNodeStateTitles, 2},
  {"RN_GetNodeStateComments", (DL_FUNC) &RN_GetNodeStateComments, 1},
  {"RN_SetNodeStateComments", (DL_FUNC) &RN_SetNodeStateComments, 2},
  {"RN_GetNodeLevelsDiscrete", (DL_FUNC) &RN_GetNodeLevelsDiscrete, 1},
  {"RN_GetNodeLevelsContinuous", (DL_FUNC) &RN_GetNodeLevelsContinuous, 1},
  {"RN_SetNodeLevels", (DL_FUNC) &RN_SetNodeLevels, 2},
  {"RN_NetworkNodeSets", (DL_FUNC) &RN_NetworkNodeSets, 2},
  {"RN_GetNodeSets", (DL_FUNC) &RN_GetNodeSets, 2},
  {"RN_SetNodeSets", (DL_FUNC) &RN_SetNodeSets, 2},
  {"RN_NetworkNodesInSet", (DL_FUNC) &RN_NetworkNodesInSet, 2},
  {"RN_NetworkSetPriority", (DL_FUNC) &RN_NetworkSetPriority, 2},
  {"RN_NetworkNodeSetColor", (DL_FUNC) &RN_NetworkNodeSetColor, 3},
  {"RN_NetworkNodeGetColor", (DL_FUNC) &RN_NetworkNodeGetColor, 2},
  {"RN_AddLink", (DL_FUNC) &RN_AddLink, 2},
  {"RN_DeleteLink", (DL_FUNC) &RN_DeleteLink, 2},
  {"RN_ReverseLink", (DL_FUNC) &RN_ReverseLink, 2},
  {"RN_GetNodeChildren", (DL_FUNC) &RN_GetNodeChildren, 1},
  {"RN_GetNodeParents", (DL_FUNC) &RN_GetNodeParents, 1},
  {"RN_SetNodeParents", (DL_FUNC) &RN_SetNodeParents, 2},
  {"RN_GetNodeInputNames", (DL_FUNC) &RN_GetNodeInputNames, 1},
  {"RN_SetNodeInputNames", (DL_FUNC) &RN_SetNodeInputNames, 2},
  {"RN_AbsorbNodes", (DL_FUNC) &RN_AbsorbNodes, 1},
  {"RN_IsNodeRelated", (DL_FUNC) &RN_IsNodeRelated, 3},
  {"RN_GetRelatedNodes", (DL_FUNC) &RN_GetRelatedNodes, 2},
  {"RN_GetEveryState", (DL_FUNC) &RN_GetEveryState, 0},
  {"RN_GetNodeProbs", (DL_FUNC) &RN_GetNodeProbs, 2},
  {"RN_SetNodeProbs", (DL_FUNC) &RN_SetNodeProbs, 3},
  {"RN_IsNodeDeterministic", (DL_FUNC) &RN_IsNodeDeterministic, 1},
  {"RN_HasNodeTable", (DL_FUNC) &RN_HasNodeTable, 1},
  {"RN_DeleteNodeTable", (DL_FUNC) &RN_DeleteNodeTable, 1},
  {"RN_MakeCliqueNode", (DL_FUNC) &RN_MakeCliqueNode, 1},
  {"RN_CompileNet", (DL_FUNC) &RN_CompileNet, 1},
  {"RN_UncompileNet", (DL_FUNC) &RN_UncompileNet, 1},
  {"RN_RetractNetFindings", (DL_FUNC) &RN_RetractNetFindings, 1},
  {"RN_GetNodeFinding", (DL_FUNC) &RN_GetNodeFinding, 1},
  {"RN_RetractNodeFinding", (DL_FUNC) &RN_RetractNodeFinding, 1},
  {"RN_SetNodeFinding", (DL_FUNC) &RN_SetNodeFinding, 2},
  {"RN_SetNodeFindingNot", (DL_FUNC) &RN_SetNodeFindingNot, 2},
  {"RN_IsBeliefUpdated", (DL_FUNC) &RN_IsBeliefUpdated, 1},
  {"RN_GetNodeBeliefs", (DL_FUNC) &RN_GetNodeBeliefs, 1},
  {"RN_GetNodeLikelihood", (DL_FUNC) &RN_GetNodeLikelihood, 1},
  {"RN_SetNodeLikelihood", (DL_FUNC) &RN_SetNodeLikelihood, 2},
  {"RN_MostProbableConfig", (DL_FUNC) &RN_MostProbableConfig, 2},
  {"RN_FindingsProbability", (DL_FUNC) &RN_FindingsProbability, 1},
  {"RN_JointProbability", (DL_FUNC) &RN_JointProbability, 1},
  {"RN_JunctionTreeReport", (DL_FUNC) &RN_JunctionTreeReport, 1},
  {"RN_SetEliminationOrder", (DL_FUNC) &RN_SetEliminationOrder, 2},
  {"RN_GetEliminationOrder", (DL_FUNC) &RN_GetEliminationOrder, 1},
  {"RN_SizeCompiledNetwork", (DL_FUNC) &RN_SizeCompiledNetwork, 1},
  {"RN_CaseFileDelimiter", (DL_FUNC) &RN_CaseFileDelimiter, 1},
  {"RN_MissingCode", (DL_FUNC) &RN_MissingCode, 1},
  {"RN_WriteFindings", (DL_FUNC) &RN_WriteFindings, 4},
  {"RN_ReadFindings", (DL_FUNC) &RN_ReadFindings, 4},
  {"RN_isCaseStreamActive", (DL_FUNC) &RN_isCaseStreamActive, 1},
  {"RN_OpenCaseFileStream", (DL_FUNC) &RN_OpenCaseFileStream, 2},
  {"RN_OpenCaseMemoryStream", (DL_FUNC) &RN_OpenCaseMemoryStream, 2},
  {"RN_CloseCaseStream", (DL_FUNC) &RN_CloseCaseStream, 1},
  {"RN_SetMemoryStreamContents", (DL_FUNC) &RN_SetMemoryStreamContents, 2},
  {"RN_GetMemoryStreamContents", (DL_FUNC) &RN_GetMemoryStreamContents, 1},
  {"RN_GetNodeExperience", (DL_FUNC) &RN_GetNodeExperience, 2},
  {"RN_SetNodeExperience", (DL_FUNC) &RN_SetNodeExperience, 3},
  {"RN_FadeCPT", (DL_FUNC) &RN_FadeCPT, 2},
  {"RN_LearnFindings", (DL_FUNC) &RN_LearnFindings, 2},
  {"RN_LearnCaseStream", (DL_FUNC) &RN_LearnCaseStream, 3},
  {"RN_LearnCPTs", (DL_FUNC) &RN_LearnCPTs, 6},
  {"RN_GetNodeValue", (DL_FUNC) &RN_GetNodeValue, 1},
  {"RN_SetNodeValue", (DL_FUNC) &RN_SetNodeValue, 2},
  {"RN_SetNodeInterval", (DL_FUNC) &RN_SetNodeValue, 4},
  {"RN_SetNodeGaussian", (DL_FUNC) &RN_SetNodeValue, 4},
  {"RN_GetNodeExpectedValue", (DL_FUNC) &RN_GetNodeExpectedValue, 1},
  {"RN_GetNodeExpectedUtils", (DL_FUNC) &RN_GetNodeExpectedUtils, 1},
  {"RN_CalcNodeState", (DL_FUNC) &RN_CalcNodeState, 1},
  {"RN_CalcNodeValue", (DL_FUNC) &RN_CalcNodeValue, 1},
  {"RN_GetMutalInfo", (DL_FUNC) &RN_GetMutualInfo, 2},
  {"RN_GetVarianceOfReal", (DL_FUNC) &RN_GetVarianceOfReal, 2},
  {"RN_GetNodeEquation", (DL_FUNC) &RN_GetNodeEquation, 1},
  {"RN_SetNodeEquation", (DL_FUNC) &RN_SetNodeEquation, 2},
  {"RN_EquationToTable", (DL_FUNC) &RN_EquationToTable, 4},
  {"RN_NewRandomGenerator", (DL_FUNC) &RN_NewRandomGenerator, 1},
  {"RN_FreeRNG", (DL_FUNC) &RN_FreeRNG, 1},
  {"RN_isRNGActive", (DL_FUNC) &RN_isRNGActive,1},
  {"RN_SetNetRandomGen", (DL_FUNC) &RN_SetNetRandomGen, 2},
  {"RN_GenerateRandomCase", (DL_FUNC) &RN_GenerateRandomCase,4},
  {NULL, NULL, 0},
};

/////////////////////////////////////////////////////////////////////
// .C Methods
////////////////////////////////////////////////////////////////
// File = Networks.c
extern void RN_start_Netica(char** license, char** checking, double* maxmem);
extern void RN_stop_Netica();
extern void RN_report_errors(int* maxreport, int* clear, int* counts);
extern void RN_ClearAllErrors(char** sev);

R_CMethodDef cMethods[] = {
  {"RN_start_Netica", (DL_FUNC) &RN_start_Netica, 3, 
   (R_NativePrimitiveArgType[3]) {STRSXP, STRSXP, REALSXP}},
  {"RN_stop_Netica", (DL_FUNC) &RN_stop_Netica, 0},
  {"RN_report_errors",(DL_FUNC) &RN_report_errors, 3, 
   (R_NativePrimitiveArgType[3]) {INTSXP, INTSXP, INTSXP}},
  {"RN_ClearAllErrors",(DL_FUNC) &RN_ClearAllErrors, 1, 
   (R_NativePrimitiveArgType[1]) {STRSXP}},
  {NULL, NULL, 0}
};


void R_init_RNetica(DllInfo *info) {
  R_registerRoutines(info, cMethods, callMethods, NULL, NULL);
  RN_Define_Symbols();
}

void R_unload_RNetica(DllInfo *info) {
  RN_Free_Symbols();
}
