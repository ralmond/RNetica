/** 
 * Session.c 
 *  New Session level C funcitons. */


/**
 * Networks.c --- These files describe functions for creating,
 * destroying, saving and other high level functions on networks.
 */

#include <string.h>
#include <R.h>
#include <Rdefines.h>
#include <RNetica.h>

/* RX_... functions for accessing fields in RC classes.
  This is currently implemented by using an .xData slot which
  contains an environment.  This might break if the R implementation
  changes */

SEXP RX_do_RC_field(SEXP obj, SEXP name) {
  SEXP rho,result;                     
  if (!isS4(obj)) {
    error("Can only get fields for RC (S4) objects (field %s).",
          PRINTNAME(name));
  }
  //Rprintf("Extracting environment\n");
  PROTECT(rho=GET_SLOT(obj,install(".xData")));
  if (isNull(rho)) {
    error("The .xData slot is null, is this an RC class?\n");
  }
  //Rprintf("Extracting variable %s\n",PRINTNAME(name));
  PROTECT(result=findVar(name,rho));
  UNPROTECT(2);
  return result;
}

SEXP RX_do_RC_field_assign(SEXP obj, SEXP name, SEXP value) {
  SEXP rho;
  if (!isS4(obj)) {
    error("Can only set fields for RC (S4) objects (field %s).",
          PRINTNAME(name));
  }
  PROTECT(rho=GET_SLOT(obj,install(".xData")));
  if (isNull(rho)) {
    error("The .xData slot is null, is this an RC class?\n");
  }
  defineVar(name,value,rho);
  UNPROTECT(1);
  return obj;
}

int RX_has_RC_field(SEXP obj, SEXP name) {
  SEXP rho, fvalue;
  int result;
  if (!isS4(obj)) {
    error("Can only get fields for RC (S4) objects.\n");
  }
  PROTECT(rho=GET_SLOT(obj,install(".xData")));
  if (isNull(rho)) {
    error("The .xData slot is null, is this an RC class?\n");
  }
  PROTECT(fvalue=findVar(name,rho));
  result = !isNull(fvalue);
  UNPROTECT(2);
  return result;
}

/* findVar apparently returns R_UnboundValue if the symbol is not bound. */

Rboolean RX_isUnbound(SEXP x) {
  return x==R_UnboundValue;
}

/* Some badly needed utility functions for poitners */
SEXP RX_make_exptr(SEXP foo) {
  return R_MakeExternalPtr(NULL,R_NilValue,R_NilValue);
}

SEXP RX_is_null_ptr(SEXP ptr) {
  if (R_ExternalPtrAddr(ptr) == NULL)
    return TRUEV;
  else
    return FALSEV;
}

/******************* Session Constructor **********  */

SEXP RN_SessionMaker() {
  /* Should worry about license key, but I'm not going to bother. */
  SEXP callme, sess;
  PROTECT(callme=lang1(sessionconstructor));
  PROTECT(sess=eval(callme,R_GlobalEnv));
  UNPROTECT(2);
  return sess;
}


/* ********************  Session Objects *********************** */
environ_ns* GetSessionPtr (SEXP sessobj) {
  environ_ns* netica_env = NULL;
  SEXP exPTR;
  PROTECT(exPTR=GET_FIELD(sessobj,sessionatt));
  if (exPTR) {
    netica_env = (environ_ns*) R_ExternalPtrAddr(exPTR);
  }
  UNPROTECT(1);
  return netica_env;
}

void SetSessionPtr (SEXP sessobj, environ_ns* netica_env) {
  SEXP exPTR;
  PROTECT(exPTR=GET_FIELD(sessobj,sessionatt));
  if (exPTR) {
    R_SetExternalPtrAddr(exPTR,netica_env);
  } else {
    UNPROTECT(1);
    PROTECT(exPTR = R_MakeExternalPtr(netica_env,sessionatt,R_NilValue));
  }
  SET_FIELD(sessobj,sessionatt,exPTR);
  UNPROTECT(1);
  return;
}


SEXP RN_isSessionActive(SEXP sess) {
  SEXP sessPtr, result;
  PROTECT(result=allocVector(LGLSXP,1));
  LOGICAL(result)[0]=FALSE;
  PROTECT(sessPtr=GET_FIELD(sess,sessionatt));
  if (sessPtr && R_ExternalPtrAddr(sessPtr)) {
    LOGICAL(result)[0] = TRUE;
  }
  UNPROTECT(2);
  return result;
}


/**
 * Launchs the Netica Environment.
 *  -- License:  Licence key from Norsys, or NULL for demo mode.
 *  -- Checking:  One of ("NO_CHECK", "QUICK_CHECK", "REGULAR_CHECK",
 *       "COMPLETE_CHECK")
 *  -- maxmem:  Mamximum size for Netica memory.  0 uses Netica
 * defaults.
 */
SEXP RN_start_Session(SEXP sessobj) {
  char mesg[MESG_LEN_ns];
  int res;
  //Now called on library init.
  //RN_Define_Symbols();
  environ_ns* netica_env = GetSessionPtr(sessobj);

  //Rprintf("Got pointer: %x.\n",(long) netica_env);


  if (netica_env != NULL) {
    warning("Netica already running, use stopNetica before restarting Netica with new parameters.");
    return sessobj;
  }
  const char* lic = NULL;
  SEXP license;
  PROTECT(license=GET_FIELD(sessobj,install("LicenseKey")));
  if (length(license) > 0) {
    lic = CHAR(STRING_ELT(license,0));
  }
  UNPROTECT(1);
  netica_env = NewNeticaEnviron_ns(lic,NULL,NULL);

  if(!netica_env) {
    error("Netica License Key not accepted. \n Make sure key starts with a + and ends with five digit security code.");
  }

  res = InitNetica2_bn(netica_env,mesg);
  if (res < 0) {
    error("%s",mesg);
    return sessobj;
  }
  Rprintf("%s\n",mesg);

  //Save pointer in session object
  SetSessionPtr(sessobj,netica_env);
  // Now set up checking according to field.
  const char* check = NULL;
  SEXP checking;
  PROTECT(checking=GET_FIELD(sessobj,install("Checking")));
  if (length(checking) > 0) {
    check = CHAR(STRING_ELT(checking,0));
  }
  UNPROTECT(1);
  
  if (check != NULL) {
    checking_ns do_check = REGULAR_CHECK;
    if (strcmp(check,"NO_CHECK")==0) {
      do_check = NO_CHECK;
    } else  if (strcmp(check,"QUICK_CHECK")==0) {
      do_check = QUICK_CHECK;
    } else  if (strcmp(check,"REGULAR_CHECK")==0) {
      do_check = REGULAR_CHECK;
    } else  if (strcmp(check,"COMPLETE_CHECK")==0) {
      do_check = COMPLETE_CHECK;
    } else {
      warning("Unknown argument checking type %s",check);
    }
    ArgumentChecking_ns(do_check,netica_env);
  }

  double memsize=0.0;
  SEXP maxmem;

  PROTECT(maxmem=GET_FIELD(sessobj,install("maxmem")));
  if (length(maxmem) > 0) {
    memsize = REAL(maxmem)[0];
  }
  UNPROTECT(1);

  if (memsize>200000) {
    LimitMemoryUsage_ns(memsize,netica_env);
  }

  return sessobj;
}

/**
 * This function closes Netica cleanly.
 */
SEXP RN_stop_Session(SEXP sessobj) {

  char mesg[MESG_LEN_ns];
  int res;

  environ_ns* netica_env = GetSessionPtr(sessobj);

  if (netica_env == NULL) {
    warning("Netica not running, nothing to do.");
    return sessobj;
  }


  /* This is now done in the R code. */
  /* Rprintf("Shut down any remaining nets.\n"); */
  /* int nth = 0; */
  /* net_bn* net; */
  /* SEXP bn, bnPointer; */
  /* while (TRUE) { */
  /*   net = GetNthNet_bn (nth++, netica_env); */
  /*   Rprintf("Closing network %s.\n",GetNetName_bn(net)); */
  /*   if (!net) break; */
  /*   //Should probably do this on R side, not C side. */
  /*   //RN_Free_Nodes(GetNetNodes_bn(net),net); */
  /*   RN_UnregisterNetwork(sessobj,GetNetName_bn(net)); */
  /* }  */

  res = CloseNetica_bn(netica_env,mesg);
  netica_env = NULL; //Set to null no matter what.
  //Save pointer in session object
  SEXP sesshandle;
  PROTECT(sesshandle = GET_FIELD(sessobj,sessionatt));
  R_ClearExternalPtr(sesshandle);
  SET_FIELD(sessobj,sessionatt,sesshandle);
  UNPROTECT(1);

  if (res < 0) {
    error("%s",mesg);
  }
  //RN_Free_Symbols();
  return sessobj;
}

/////////////////////////////////////////////////////////////////
// Network Registration Utilities
/////////////////////////////////////////////////////////////////
//
// This is the core of the rework.  Instead of relying on the
// backpointer from Netica, NeticaBN objects are now installed in an
// environment (symbol table) which is the nets feild of the session
// object. 

// These utilities register (install the network in the environment),
// Unregister (set the network to nil) and find networks by name. 

void RN_RegisterNetwork(SEXP sessobj, const char* netname, SEXP netobj) {
  SEXP sess_env = NULL;
  PROTECT(sess_env=GET_FIELD(sessobj,netsfield));
  defineVar(install(netname),netobj,sess_env);
  UNPROTECT(1);
}

void RN_UnregisterNetwork(SEXP sessobj, const char* netname) {
  SEXP sess_env = NULL;
  PROTECT(sess_env=GET_FIELD(sessobj,netsfield));
  defineVar(install(netname),R_NilValue,sess_env);
  UNPROTECT(1);
}

SEXP RN_FindNetworkStr(SEXP sessobj, const char* netname) {
  SEXP sess_env;
  PROTECT(sess_env=GET_FIELD(sessobj,netsfield));
  SEXP result =findVar(install(netname),sess_env);
  UNPROTECT(1);
  return result;
}

////////////////////////////////////////////////////////////////////
// Netica Utilities
////////////////////////////////////////////////////////////////////


SEXP RN_Session_Version(SEXP sessobj) {
  SEXP result, vnum, vstring, names;
  const char *vs;
  environ_ns* netica_env = GetSessionPtr(sessobj);

  PROTECT(result = allocVector(VECSXP,2));
  PROTECT(vnum = allocVector(INTSXP,1));
  PROTECT(names = allocVector(STRSXP,2));

  INTEGER(vnum)[0]= GetNeticaVersion_bn(netica_env,&vs);
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
SEXP RN_Session_errors(SEXP sessobj, SEXP maxerrobj, SEXP clearflag) {
  int maxerr = INTEGER(maxerrobj)[0];
  int clearit = LOGICAL(clearflag)[0];
  environ_ns* netica_env = GetSessionPtr(sessobj);

  SEXP errorCounts;

  PROTECT(errorCounts = allocVector(INTSXP,4));
  int* counts = INTEGER(errorCounts);
  counts[0] = counts[1] = counts[2] = counts[3] = NA_INTEGER;
  report_ns* err = NULL;
  int ecount = 0;

  counts[0] = 0;
  while ((err = GetError_ns(netica_env, XXX_ERR, err))!=NULL) {
    Rprintf("Fatal Netica error: %s\n",ErrorMessage_ns(err));
    ecount++;
    counts[0]++;
    if (clearit) ClearError_ns(err);
  }
  if (ecount >0) {
    error("Fatal errors encountered, recommend restarting Netica");
  }

  while ((err = GetError_ns(netica_env, ERROR_ERR, err))!=NULL) {
    Rprintf("Netica error: %s\n",ErrorMessage_ns(err));
    counts[0]++;
    if (ecount++ > maxerr) return errorCounts;
    if (clearit) ClearError_ns(err);
  }
  
  counts[1]=0;
  while ((err = GetError_ns(netica_env, WARNING_ERR, err))!=NULL) {
    Rprintf("Netica warning: %s\n",ErrorMessage_ns(err));
    counts[1]++;
    if (ecount++ > maxerr) return errorCounts;
    if (clearit) ClearError_ns(err);
  }

  counts[2]=0;
  while ((err = GetError_ns(netica_env, NOTICE_ERR, err))!=NULL) {
    Rprintf("Netica warning: %s\n",ErrorMessage_ns(err));
    counts[2]++;
    if (ecount++ > maxerr) return errorCounts;
    if (clearit) ClearError_ns(err);
  }

  counts[3] = 0;
  while ((err = GetError_ns(netica_env, NOTICE_ERR, err))!=NULL) {
    Rprintf("Netica warning: %s\n",ErrorMessage_ns(err));
    counts[3]++;
    if (ecount++ > maxerr) return errorCounts;
    if (clearit) ClearError_ns(err);
  }

  UNPROTECT(1);
  return errorCounts;
}


/**
 * Clears all errors at a given severity (and lower?)
 * sev -- should be either NULL (all arguments) or a single character
 * string, one of "NOTHING_ERR", "REPORT_ERR", "NOTICE_ERR", 
 * "WARNING_ERR", "ERROR_ERR", or "XXX_ERR"
 */
SEXP RN_ClearSessionErrors(SEXP sessobj, SEXP severity) {
  errseverity_ns etype = XXX_ERR;
  environ_ns* netica_env = GetSessionPtr(sessobj);
  const char* sev = CHAR(STRING_ELT(severity,0));

  if (sev != NULL) {
    if (strcmp(sev,"NOTHING_ERR")==0) {
      etype = NOTHING_ERR;
    } else  if (strcmp(sev,"REPORT_ERR")==0) {
      etype = REPORT_ERR;
    } else  if (strcmp(sev,"NOTICE_ERR")==0) {
      etype = NOTICE_ERR;
    } else  if (strcmp(sev,"WARNING_ERR")==0) {
      etype = WARNING_ERR;
    } else  if (strcmp(sev,"ERROR_ERR")==0) {
      etype = ERROR_ERR;
    } else  if (strcmp(sev,"XXX_ERR")==0) {
      etype = XXX_ERR;
    } else {
      warning("Unknown error type %s, no errors cleared",sev);
      etype = NOTHING_ERR;
    }
  }
  Rprintf("Clearing errors for network @%x, of type %d.\n",
          (long) netica_env, etype);
  ClearErrors_ns(netica_env,etype);
  return sessobj;
}

