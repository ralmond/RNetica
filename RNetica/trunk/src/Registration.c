/**
 * Registration.c --- These files register the .C and .Call methods
 * so R can finde them.
 */

#include <string.h>
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
const char* DISCRETEATT = "node_discrete";
const char* EmptyString = "";

SEXP bnclass=NULL;
SEXP nodeclass=NULL;
SEXP bnatt=NULL;
SEXP nodeatt=NULL;
SEXP nodediscatt=NULL;
SEXP TRUEV=NULL;
SEXP FALSEV=NULL;
SEXP NAV=NULL;

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
  //printf("RN_Defining_Symbols: done.\n");
  symbolRegCount++;
}

void RN_Free_Symbols() {
  //printf("RN_Free_Symbols: %d.\n",symbolRegCount);
  if (--symbolRegCount == 0) {
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
  int maxerr = 999999;
  int clearit = 1;

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
    error("Fatal errors encountered, recomment restarting Netica");
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






R_CallMethodDef callMethods[] = {
  {"RN_Netica_Version", (DL_FUNC) &RN_Netica_Version, 0},
  {"RN_isBNActive", (DL_FUNC) &RN_Netica_Version, 1},
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


void R_init_RNEtica(DllInfo *info) {
  R_registerRoutines(info, cMethods, callMethods, NULL, NULL);
  RN_Define_Symbols();
}

void R_unload_RNetica(DllInfo *info) {
  RN_Free_Symbols();
}
