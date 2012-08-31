/**
 * Networks.c --- These files describe functions for creating,
 * destroying, saving and other high level functions on networks.
 */

#include <string.h>
#include <R.h>
#include <Rdefines.h>
#include <RNetica.h>

/*****************************************************************************
 * Starting and Stopping Netica
 *****************************************************************************/

/**
 * Common Symbols so we don't need to keep redefining them.
 */
char* NeticaClass = "NeticaBN";
char* DeletedClass = "DeletedNeticaBN";
char* NetPointer = "Netica_net_bn";


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

  RN_Define_Symbols();

  if (RN_netica_env != NULL) {
    warn("Netica already running, use stopNetica before restarting Netica with new parameters.");
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
      warn("Unknown argument checking type %s",checking[0]);
    }
    ArgumentChecking_ns(do_check,RN_netica_env);
  }

  if (maxmem != NULL) {
    LimitMemoryUsage(maxmem[0],RN_netica_env);
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
    warn("Netica not running, nothing to do.");
    return;
  }

  res = CloseNetica_bn(RN_netica_env,mesg);
  RN_environ_ns = NULL; //Set to null no matter what.
  if (res < 0) {
    error("%s",mesg);
  }
  return;
}


SEXP RN_Netica_Version() {
  SEXP result, vnum, vstring, names;
  const char *vs;
  PROTECT(result = allocVector(VECSXP,2));
  PROTECT(vnum = allocVector(INTSXP,1));
  PROTECT(vstring = allocVector(STRSXP,1));
  PROTECT(names = allocVector(STRSXP,2));

  INTEGER(vnum)[0]= GetNeticaVersion(RN_netica_env,vs);
  SET_STRING_ELT(vstring,0,vs);

  SET_VECTOR_ELT(result,0,vnum);
  SET_STRING_ELT(names, 0,"number");
  SET_VECTOR_ELT(result,1, vstring);
  SET_STRING_ELT(names, 1,"message");
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
  while ((err = GetError_ns(RN_netica_env, XXX_ERR, err)!=NULL)) {
    Rprintf("Fatal Netica error: %s\n",ErrorMessage_ns(err));
    ecount++;
    counts[0]++;
    if (clearit) ClearError_ns(err);
  }
  if (ecount >0) {
    error("Fatal errors encountered, recomment restarting Netica");
  }

  while ((err = GetError_ns(RN_netica_env, ERROR_ERR, err)!=NULL)) {
    Rprintf("Netica error: %s\n",ErrorMessage_ns(err));
    counts[0]++;
    if (ecount++ > maxerr) return;
    if (clearit) ClearError_ns(err);
  }
  
  counts[1]=0;
  while ((err = GetError_ns(RN_netica_env, WARNING_ERR, err)!=NULL)) {
    Rprintf("Netica warning: %s\n",ErrorMessage_ns(err));
    counts[1]++;
    if (ecount++ > maxerr) return;
    if (clearit) ClearError_ns(err);
  }

  counts[2]=0;
  while ((err = GetError_ns(RN_netica_env, NOTICE_ERR, err)!=NULL)) {
    Rprintf("Netica warning: %s\n",ErrorMessage_ns(err));
    counts[2]++;
    if (ecount++ > maxerr) return;
    if (clearit) ClearError_ns(err);
  }

  counts[3] = 0;
  while ((err = GetError_ns(RN_netica_env, NOTICE_ERR, err)!=NULL)) {
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
      warn("Unknown error type %s, no errors cleared",sev[0]);
      etype = NOTHING_ERR;
    }
  }
  ClearErrors_ns(RN_netica_env,etype);

}

/*****************************************************************************
 * Creating and Destroying Bayesian Networks.
 *****************************************************************************/

SEXP RN_New_Net(SEXP namelist) {
  PROTECT(namelist = AS_CHARACTER(namelist));
  R_len_t n, nn = lenght(namelist);
  const char* name;
  net_bn* netica_handle;
  SEXP bnhandlelist, bn, bnhandle;
  SEXP bnclass,bnatt;

  PROTECT(bnclass = allocVector(STRSXP,1));
  SET_STRING_ELT(bnclass,0,mkChar(NeticaClass));
  PROTECT(bnatt = install("Netica_bn")); 

  PROTECT(bnhandlelist = allocVector(VECSXP,nn));
  for (n=0; n < nn; n++) {
    name = CHAR(STRING_ELT(namelist,n));
    netica_handle = NewNet_bn(name,RN_netica_env);
    PROTECT(bn = allocVector(STRSXP,1));
    bnhandle = R_MakeExternalPtr(netica_handle,bnatt,
                                 R_NilValue);
    PROTECT(bnhandle);
    /* Return the network name */
    SET_STRING_ELT(bn,0,mkChar(name));
    /* Set the handle as an attribute. */
    setAttrib(bn,NetPointer,bnhandle);
    classgets(bn,bnclass);
    /* Now stick it in array */
    SET_VECTOR_ELT(bnhandlelist,n,bn);
    UNPROTECT(2); //I think it should be OK to free these up as soon as
                  //they are assigned to a protected object.
  }
  UNPROTECT(4);
  return(bnhandlelist);
}

SEXP RN_Delete_Net(SEXP netlist) {
  PROTECT(netlist);
  R_len_t n, nn = lenght(netlist);
  net_bn* netica_handle;
  SEXP bn, bnhandle;
  SEXP bnclass,bnatt;

  PROTECT(bnclass = allocVector(STRSXP,1));
  PROTECT(bnatt = install("Netica_bn")); 
  SET_STRING_ELT(bnclass,0,mkChar("DeletedNeticaBN"));
  
  for (n=0; n < nn; n++) {
    PROTECT(bn = GET_VECTOR_ELT(netlist,n));
    PROTECT(bnhandle = getAttrib(bn,bnatt));
    netica_handle = (net_bn*) R_ExternalPtrAddr(bnhandle);

    DeleteNet_bn(netica_handle);

    /* Clear the handle and the class. */
    R_SetExternalPtrAddr(bnhandle,NULL);
    setAttrib(bn,install("Netica_net_bn"),bnhandle);
    classgets(bn,bnclass);
    UNPROTECT(2); //I think it should be OK to free these up as soon as
                  //they are assigned to a protected object.
  }
  UNPROTECT(3);
  return(netlist);
}

// Copyied from NETICA API manual because it looks useful.
net_bn* NetNamed (const char* name, environ_ns* env){
    int nth = 0;
    net_bn* net;
    do {
        net = GetNthNet_bn (nth++, env);
    } while (net  strcmp (name, GetNetName_bn (net)) != 0);
    return net;
}

SEXP RN_Named_Nets(SEXP namelist) {
  PROTECT(namelist = AS_CHARACTER(namelist));
  R_len_t n, nn = lenght(namelist);
  const char* name;
  net_bn* netica_handle;
  SEXP bnhandlelist, bn, bnhandle;
  SEXP bnclass,bnatt;

  PROTECT(bnclass = allocVector(STRSXP,1));
  SET_STRING_ELT(bnclass,0,mkChar(NeticaClass));
  PROTECT(bnatt = install("Netica_bn")); 

  PROTECT(bnhandlelist = allocVector(VECSXP,nn));
  for (n=0; n < nn; n++) {
    name = CHAR(STRING_ELT(namelist,n));
    netica_handle = NetNamed(name,RN_netica_env);
    PROTECT(bn = allocVector(STRSXP,1));
    bnhandle = R_MakeExternalPtr(netica_handle,bnatt,
                                 R_NilValue);
    PROTECT(bnhandle);
    /* Return the network name */
    SET_STRING_ELT(bn,0,mkChar(name));
    /* Set the handle as an attribute. */
    setAttrib(bn,NetPointer,bnhandle);
    classgets(bn,bnclass);
    /* Now stick it in array */
    SET_VECTOR_ELT(bnhandlelist,n,bn);
    UNPROTECT(2); //I think it should be OK to free these up as soon as
                  //they are assigned to a protected object.
  }
  UNPROTECT(4);
  return(bnhandlelist);
}

SEXP RN_GetNth_Nets(SEXP nlist) {
  PROTECT(nlist = AS_INTEGER(nlist));
  R_len_t n, nn = lenght(nlist);
  int *netno;
  const char* name;
  net_bn* netica_handle;
  SEXP bnhandlelist, bn, bnhandle;
  SEXP bnclass,bnatt;

  PROTECT(bnclass = allocVector(STRSXP,1));
  SET_STRING_ELT(bnclass,0,mkChar(NeticaClass));
  PROTECT(bnatt = install("Netica_bn")); 

  PROTECT(bnhandlelist = allocVector(VECSXP,nn));
  netno = INTEGER(nlist);
  for (n=0; n < nn; n++) {
    netica_handle = GetNthNet_bn(netno[n],RN_netica_env);
    name = GetNetName_bn(netica_handle);
    PROTECT(bn = allocVector(STRSXP,1));
    bnhandle = R_MakeExternalPtr(netica_handle,bnatt,
                                 R_NilValue);
    PROTECT(bnhandle);
    /* Return the network name */
    SET_STRING_ELT(bn,0,mkChar(name));
    /* Set the handle as an attribute. */
    setAttrib(bn,NetPointer,bnhandle);
    classgets(bn,bnclass);
    /* Now stick it in array */
    SET_VECTOR_ELT(bnhandlelist,n,bn);
    UNPROTECT(2); //I think it should be OK to free these up as soon as
                  //they are assigned to a protected object.
  }
  UNPROTECT(4);
  return(bnhandlelist);
}

SEXP RN_Copy_Nets(SEXP nets, SEXP namelist, SEXP options) {
  PROTECT(namelist = AS_CHARACTER(namelist));
  PROTECT(nets = AS_CHARACTER(namelist));
  R_len_t n, nn = lenght(namelist);
  const char* name;
  net_bn* old_net, new_net;
  SEXP bnhandlelist, old_bn, new_bn, old_handle, new_handle;
  SEXP bnclass,bnatt;

  PROTECT(bnclass = allocVector(STRSXP,1));
  SET_STRING_ELT(bnclass,0,mkChar(NeticaClass));
  PROTECT(bnatt = install("Netica_bn")); 

  PROTECT(bnhandlelist = allocVector(VECSXP,nn));
  for (n=0; n < nn; n++) {
    name = CHAR(STRING_ELT(namelist,n));
    PROTECT(old_bn = GET_VECTOR_ELT(netlist,n));
    PROTECT(old_handle = getAttrib(old_bn,bnatt));
    old_net = (net_bn*) R_ExternalPtrAddr(old_handle);

    new_net = NewNet_bn(name,RN_netica_env);
    PROTECT(new_bn = allocVector(STRSXP,1));
    new_handle = R_MakeExternalPtr(new_net,bnatt,
                                 R_NilValue);
    PROTECT(new_handle);
    /* Return the network name */
    SET_STRING_ELT(new_bn,0,mkChar(name));
    /* Set the handle as an attribute. */
    setAttrib(new_bn,NetPointer,new_handle);
    classgets(new_bn,bnclass);
    /* Now stick it in array */
    SET_VECTOR_ELT(bnhandlelist,n,new_bn);
    UNPROTECT(4); //I think it should be OK to free these up as soon as
                  //they are assigned to a protected object.
  }
  UNPROTECT(5);
  return(bnhandlelist);
}
