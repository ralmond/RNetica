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
  
  // Shut down any remaining nets
  int nth = 0;
  net_bn* net;
  SEXP bn, bnPointer;
  while (TRUE) {
    net = GetNthNet_bn (nth++, RN_netica_env);
    if (!net) break;
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

/*****************************************************************************
 * Creating and Destroying Bayesian Networks.
 *****************************************************************************/
// Original design here was to capture and save the returned handle to
// the network.  But this is difficult to make safe as R will hang
// onto the stale pointers.  Instead, we will use the names, and then
// just try to find the network by name when we want to modify it.
// That should be only slightly slower but infinitely safer.
//
// Design 3.  Now I am using the Netica User data field to store
// NeticaBN object.  This means that I should be able to make the
// objects unique.

/**
 * This is a small utility function meant to be used from within
 * toString to determine if the pointer is live or not.
 */
SEXP RN_isBNActive(SEXP bn) {
  SEXP bnPtr, result;
  PROTECT(result=allocVector(LGLSXP,1));
  LOGICAL(result)[0]=FALSE;
  PROTECT(bnPtr = getAttrib(bn,bnatt));
  if (bnPtr && R_ExternalPtrAddr(bnPtr)) {
    LOGICAL(result)[0] = TRUE;
  }
  UNPROTECT(2);
  return result;
}

// Copyied from NETICA API manual because it looks useful.
// This is now key to the new strategy.  
net_bn* RN_AS_NET (const char* name){
    int nth = 0;
    net_bn* net;
    do {
        net = GetNthNet_bn (nth++, RN_netica_env);
    } while (net && strcmp (name, GetNetName_bn (net)) != 0);
    return net;
}



SEXP RN_New_Net(SEXP namelist) {
  //RN_Define_Symbols();

  PROTECT(namelist = AS_CHARACTER(namelist));
  R_len_t n, nn = length(namelist);
  const char* name;
  net_bn* netica_handle;
  SEXP bnhandlelist, bn, bnhandle;
    
  PROTECT(bnhandlelist = allocVector(VECSXP,nn));
  for (n=0; n < nn; n++) {
    name = CHAR(STRING_ELT(namelist,n));
    netica_handle = RN_AS_NET(name);
    if (netica_handle) {
      warning("Network named %s already exists.",name);
      SET_VECTOR_ELT(bnhandlelist,n,R_NilValue);
    } else {
      netica_handle = NewNet_bn(name,RN_netica_env);
      bn = allocVector(STRSXP,1);
      R_PreserveObject(bn);
      /* Return the network name */
      SET_STRING_ELT(bn,0,mkChar(name));
      /* Set the handle as an attribute. */
      PROTECT(bnhandle = R_MakeExternalPtr(netica_handle,bnatt,
                                           R_NilValue));
      setAttrib(bn,bnatt,bnhandle);

      SET_CLASS(bn,bnclass);

      /* Set a back pointer to the R object in the Netica Object */
      SetNet_RRef(netica_handle,bn);
      /* Finally, stick it in array */
      SET_VECTOR_ELT(bnhandlelist,n,bn);
      UNPROTECT(1); //bn is created within loop, so free it here.
    }
  }

  UNPROTECT(2);
  //RN_Free_Symbols();
  return(bnhandlelist);
}

SEXP RN_Delete_Net(SEXP netlist) {
  //RN_Define_Symbols();

  R_len_t n, nn = length(netlist);
  const char* name;
  net_bn* netica_handle;
  SEXP bn, bnhandle, result;

  PROTECT(result = allocVector(VECSXP,nn));

  for (n=0; n < nn; n++) {
    PROTECT(bn = VECTOR_ELT(netlist,n));
    PROTECT(bnhandle = getAttrib(bn,bnatt));
    netica_handle = (net_bn*) R_ExternalPtrAddr(bnhandle);
    if (netica_handle) {
      DeleteNet_bn(netica_handle);
      /* Clear the handle */
      R_ClearExternalPtr(bnhandle);
      setAttrib(bn,bnatt,bnhandle); //Probably not needed.

      SET_VECTOR_ELT(result,n,bn);
      R_ReleaseObject(bn); //Let R garbage collect it when all
                           //references are gone.
    } else {
      SET_VECTOR_ELT(result,n,R_NilValue);
      warning("Did not find a network named %s.", BN_NAME(bn));
    }
    UNPROTECT(2);
  }
  UNPROTECT(1);
  //RN_Free_Symbols();
  return(result);
}


SEXP RN_Named_Nets(SEXP namelist) {
  //RN_Define_Symbols();
  R_len_t n, nn = length(namelist);
  const char* name;
  net_bn* netica_handle;
  SEXP bnhandlelist, bn;

  PROTECT(bnhandlelist = allocVector(VECSXP,nn));
  for (n=0; n < nn; n++) {
    name = CHAR(STRING_ELT(namelist,n));
    netica_handle = RN_AS_NET(name);
    if (netica_handle) {
      /* Fetch the bn object. */
      bn = GetNet_RRef(netica_handle);
      /* Now stick it in array */
      SET_VECTOR_ELT(bnhandlelist,n,bn);
      //UNPROTECT(1); //It is preserved, do don't need to free it.
    } else {
      SET_VECTOR_ELT(bnhandlelist,n,R_NilValue);
      warning("Did not find a network named %s.",name);
    }
  }
  UNPROTECT(1);
  //RN_Free_Symbols();
  return(bnhandlelist);
}

SEXP RN_GetNth_Nets(SEXP nlist) {
  //RN_Define_Symbols();
  R_len_t n, nn = length(nlist);
  int *netno;
  const char* name;
  net_bn* netica_handle;
  SEXP bnhandlelist, bn, bnhandle;


  PROTECT(bnhandlelist = allocVector(VECSXP,nn));
  netno = INTEGER(nlist);
  for (n=0; n < nn; n++) {
    netica_handle = GetNthNet_bn(netno[n],RN_netica_env);
    if (netica_handle) {
      /* Get corresponding R object */
      bn = GetNet_RRef(netica_handle);
      /* Now stick it in array */
      SET_VECTOR_ELT(bnhandlelist,n,bn);
      //UNPROTECT(1); //bn is preserved, so don't need protection.
    } else{
      SET_VECTOR_ELT(bnhandlelist,n,R_NilValue);
      //warning("Did not find a network named %s.",name);
    }
  }
  UNPROTECT(1);
  //RN_Free_Symbols();
  return(bnhandlelist);
}

SEXP RN_Copy_Nets(SEXP nets, SEXP namelist, SEXP options) {
  //RN_Define_Symbols();
  R_len_t n, nn = length(namelist);
  const char *newname, *opt;
  net_bn *old_net, *new_net;
  SEXP bnhandlelist, old_bn, new_bn, newHandle;

  opt = CHAR(STRING_ELT(options,0));

  PROTECT(bnhandlelist = allocVector(VECSXP,nn));
  for (n=0; n < nn; n++) {
    newname = CHAR(STRING_ELT(namelist,n));
    PROTECT(old_bn = VECTOR_ELT(nets,n));
    old_net = GetNeticaHandle(old_bn);
    if (old_net) {
      new_net = CopyNet_bn(old_net,newname,RN_netica_env,opt);

      R_PreserveObject(new_bn = allocVector(STRSXP,1));
      /* Return the network name */
      SET_STRING_ELT(new_bn,0,mkChar(newname));
      setAttrib(new_bn,bnatt,
                R_MakeExternalPtr(new_net,bnatt,
                                  R_NilValue));

      SET_CLASS(new_bn,bnclass);
      /* Set a back pointer to the R object in the Netica Object */
      SetNet_RRef(new_net,new_bn);

      /* Now stick it in array */
      SET_VECTOR_ELT(bnhandlelist,n,new_bn);
    } else {
      SET_VECTOR_ELT(bnhandlelist,n,R_NilValue);
      warning("Did not find a network named %s.",BN_NAME(old_bn));
    }
      
  }
  UNPROTECT(1); 
  //RN_Free_Symbols();
  return(bnhandlelist);
}

///////////////////////////////////////////////////////////////////////
// Net level File I/O
///////////////////////////////////////////////////////////////////////

SEXP RN_Read_Nets(SEXP filelist) {
  //RN_Define_Symbols();
  R_len_t n, nn = length(filelist);
  const char *name, *filename;
  stream_ns *file;
  net_bn* netica_handle;
  SEXP result, bn;

  PROTECT(result = allocVector(VECSXP,nn));
  
  for (n=0; n < nn; n++) {
      filename = CHAR(STRING_ELT(filelist,n));
      file = NewFileStream_ns(filename,RN_netica_env,NULL);
      netica_handle = ReadNet_bn(file,NO_WINDOW); //NO_WINDOW looks
                                                  //like best opition choice.
      DeleteStream_ns(file);

    if (netica_handle) {
      name = GetNetName_bn(netica_handle);
      R_PreserveObject(bn = allocVector(STRSXP,1));
      /* Return the network name */
      SET_STRING_ELT(bn,0,mkChar(name));
      setAttrib(bn,bnatt,
                R_MakeExternalPtr(netica_handle,bnatt,
                                  R_NilValue));

      SET_CLASS(bn,bnclass);
      /* Set a back pointer to the R object in the Netica Object */
      SetNet_RRef(netica_handle,bn);

      /* Now stick it in array */
      SET_VECTOR_ELT(result,n,bn);

    } else {
      SET_VECTOR_ELT(result,n,R_NilValue);
      warning("Could not find network for file %s.",filename);
    }
  }
  UNPROTECT(1);
  return(result);
}

SEXP RN_Write_Nets(SEXP nets, SEXP filelist) {
  R_len_t n, nn = length(filelist);
  const char *name, *filename;
  stream_ns *file;
  SEXP bn;
  net_bn* netica_handle;

  for (n=0; n < nn; n++) {
    PROTECT(bn = VECTOR_ELT(nets,n));
    netica_handle = GetNeticaHandle(bn);

    if (netica_handle) {
      filename = CHAR(STRING_ELT(filelist,n));
      file = NewFileStream_ns(filename,RN_netica_env,NULL);
      SetNet_RRef(netica_handle,NULL); //Set this temporarily to NULL
      // To keep it from being saved.
      WriteNet_bn(netica_handle,file);
      SetNet_RRef(netica_handle,bn); //Now set it back.
      DeleteStream_ns(file);
    } else {
      SET_VECTOR_ELT(nets,n,R_NilValue);
      warning("Could not find network %s.",BN_NAME(bn));
    }
    UNPROTECT(1);
  }
  return(nets);
}


/**
 * No setter for this method, implicitly defined by
 * Reading or Writing file.
 */
SEXP RN_GetNetFilename(SEXP bn) {
  const char *name, *filename;
  net_bn* netica_handle;
  SEXP result;

  PROTECT(result = allocVector(STRSXP,1));

  netica_handle = GetNeticaHandle(bn);
  if (netica_handle) {
    filename = GetNetFileName_bn(netica_handle);
    SET_STRING_ELT(result,0,mkChar(filename));
  } else {
    SET_STRING_ELT(result,0,NA_STRING);
    warning("Could not find network %s.",name);
  }
  UNPROTECT(1);
  return(result);
}

//////////////////////////////////////////////////////////////////////////
// Getters and Setters for Global Net properties.
//////////////////////////////////////////////////////////////////////////

SEXP RN_GetNetName(SEXP bn) {
  const char *netname;
  stream_ns *file;
  net_bn* netica_handle;
  SEXP result;

  PROTECT(result = allocVector(STRSXP,1));

  netica_handle = GetNeticaHandle(bn);
  if (netica_handle) {
    netname = GetNetName_bn(netica_handle);
    SET_STRING_ELT(result,0,mkChar(netname));
  } else {
    SET_STRING_ELT(result,0,NA_STRING);
    warning("Could not find network %s.",BN_NAME(bn));
  }
  UNPROTECT(1);
  return(result);
}

SEXP RN_SetNetName(SEXP bn, SEXP newnames) {
  const char *newname;
  stream_ns *file;
  net_bn *netica_handle, *other_net;
  SEXP result;

  netica_handle = GetNeticaHandle(bn);

  if (netica_handle) {
    newname = CHAR(STRING_ELT(newnames,0)); 
    other_net = RN_AS_NET(newname);
    if ( other_net && other_net != netica_handle) {
      warning("There is already a network named %s.",newname);
    } else {
      SetNetName_bn(netica_handle,newname);
      // We need to change the bn object to reflect the new name.
      SET_STRING_ELT(bn,0,mkChar(newname));
      SetNet_RRef(netica_handle,bn);
    }
  } else {
    warning("Could not find network %s.",BN_NAME(bn));
  }
  return(bn);
}


SEXP RN_GetNetTitle(SEXP bn) {
  const char *title;
  stream_ns *file;
  net_bn* netica_handle;
  SEXP result;

  PROTECT(result = allocVector(STRSXP,1));

  netica_handle = GetNeticaHandle(bn);

  if (netica_handle) {
    title = GetNetTitle_bn(netica_handle);
    SET_STRING_ELT(result,0,mkChar(title));
  } else {
    SET_STRING_ELT(result,0,NA_STRING);
    warning("Could not find network %s.",BN_NAME(bn));
  }
  UNPROTECT(1);
  return(result);
}

SEXP RN_SetNetTitle(SEXP bn, SEXP newtitle) {
  const char *title;
  stream_ns *file;
  net_bn* netica_handle;

  netica_handle = GetNeticaHandle(bn);

  if (netica_handle) {
    title = CHAR(STRING_ELT(newtitle,0));
    SetNetTitle_bn(netica_handle,title);
  } else {
    warning("Could not find network %s.",BN_NAME(bn));
  }
  return(bn);
}

SEXP RN_GetNetComment(SEXP bn) {
  const char *comment;
  stream_ns *file;
  net_bn* netica_handle;
  SEXP result;

  PROTECT(result = allocVector(STRSXP,1));

  netica_handle = GetNeticaHandle(bn);
  if (netica_handle) {
    comment = GetNetComment_bn(netica_handle);
    SET_STRING_ELT(result,0,mkChar(comment));
  } else {
    SET_STRING_ELT(result,0,NA_STRING);
    warning("Could not find network %s.",BN_NAME(bn));
  }
  UNPROTECT(1);
  return(result);
}

SEXP RN_SetNetComment(SEXP bn, SEXP newcomment) {
  const char *comment;
  stream_ns *file;
  net_bn* netica_handle;

  netica_handle = GetNeticaHandle(bn);

  if (netica_handle) {
    comment = CHAR(STRING_ELT(newcomment,0));
    SetNetComment_bn(netica_handle,comment);
  } else {
    warning("Could not find network %s.",BN_NAME(bn));
  }
  return(bn);
}

SEXP RN_GetNetAutoUpdate(SEXP bn) {
  int update;
  stream_ns *file;
  net_bn* netica_handle;
  SEXP result;

  PROTECT(result = allocVector(LGLSXP,1));

  netica_handle = GetNeticaHandle(bn);

  if (netica_handle) {
    update = GetNetAutoUpdate_bn(netica_handle);
    //Netica docs appear to be wrong here.  They seem to indicate we
    //should test against, BELIEF_UPDATE(=256) but actual value is 1.
    LOGICAL(result)[0] = update > 0;
  } else {
    LOGICAL(result)[0] = NA_LOGICAL;
    warning("Could not find network %s.",BN_NAME(bn));
  }
  UNPROTECT(1);
  return(result);
}

SEXP RN_SetNetAutoUpdate(SEXP bn, SEXP newflags) {
  int update, old_update;
  stream_ns *file;
  net_bn* netica_handle;
  SEXP result;

  PROTECT(result = allocVector(LGLSXP,1));

  netica_handle = GetNeticaHandle(bn);

  if (netica_handle) {
    update = LOGICAL(newflags)[0];
    if (update) update = BELIEF_UPDATE;
    old_update = SetNetAutoUpdate_bn(netica_handle,update);
    //Netica docs appear to be wrong here.  They seem to indicate we
    //should test against, BELIEF_UPDATE(=256) but actual value is 1.
    LOGICAL(result)[0] = old_update > 0;
  } else {
    LOGICAL(result)[0] = NA_LOGICAL;
    warning("Could not find network %s.",BN_NAME(bn));
  }
  UNPROTECT(1);
  return(result);
}

SEXP RN_GetNetUserField(SEXP bn, SEXP fieldnames) {
  const char *value, *fieldname;
  int valuelen;
  stream_ns *file;
  net_bn* netica_handle;
  SEXP result;

  PROTECT(result = allocVector(STRSXP,1));

  netica_handle = GetNeticaHandle(bn);

  if (netica_handle) {
    fieldname = CHAR(STRING_ELT(fieldnames,0));
    value = GetNetUserField_bn(netica_handle,fieldname,&valuelen,0);
    if (valuelen<0) { // No object returned.
      SET_STRING_ELT(result,0,NA_STRING);
    } else {
      SET_STRING_ELT(result,0,mkChar(value));
    }
  } else {
    SET_STRING_ELT(result,0,NA_STRING);
    warning("Could not find network %s.",BN_NAME(bn));
  }
  UNPROTECT(1);
  return(result);
}

SEXP RN_GetAllNetUserFields(SEXP bn) {
  R_len_t n, nn;
  const char *value, *fieldname;
  int valuelen;
  stream_ns *file;
  net_bn* netica_handle;
  SEXP result, fieldnames;

  netica_handle = GetNeticaHandle(bn);
  if (!netica_handle) {
    error("Could not find network %s.",BN_NAME(bn));
    PROTECT(result=allocVector(STRSXP,1));
    SET_STRING_ELT(result,0,NA_STRING);
  } else {
    //Count number of fields.
    nn = 0;
    while (TRUE) {
      GetNetNthUserField_bn(netica_handle, nn, &fieldname, &value,
                         &valuelen, 0);
      if (strlen(fieldname) <1 && valuelen <0) break;
      nn++;
    }
    PROTECT(result = allocVector(STRSXP,nn));
    PROTECT(fieldnames = allocVector(STRSXP,nn));

    for (n=0; n < nn; n++) {
      GetNetNthUserField_bn(netica_handle, n, &fieldname, &value,
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


SEXP RN_SetNetUserField(SEXP bn, SEXP fieldnames, SEXP newvals) {
  const char *value, *fieldname;
  int valuelen;
  stream_ns *file;
  net_bn* netica_handle;

  netica_handle = GetNeticaHandle(bn);

  if (netica_handle) {
    fieldname = CHAR(STRING_ELT(fieldnames,0));
    value = CHAR(STRING_ELT(newvals,0));
    SetNetUserField_bn(netica_handle,fieldname,value, strlen(value),0);
  } else {
    warning("Could not find network %s.",BN_NAME(bn));
  }
  return(bn);
}

SEXP RN_Undo(SEXP bn) {
  net_bn* netica_handle;
  SEXP result;
  
  PROTECT(result = allocVector(INTSXP,1));

  netica_handle = GetNeticaHandle(bn);
  if (!netica_handle) {
    INTEGER(result)[0] = NA_INTEGER;
    UNPROTECT(1);
    error("Could not find network %s.",BN_NAME(bn));
    return(result);
  }
  INTEGER(result)[0] = UndoNetLastOper_bn(netica_handle,-1.0);
  UNPROTECT(1);
  return(result);
}

SEXP RN_Redo(SEXP bn) {
  net_bn* netica_handle;
  SEXP result;
  
  PROTECT(result = allocVector(INTSXP,1));

  netica_handle = GetNeticaHandle(bn);
  if (!netica_handle) {
    INTEGER(result)[0] = NA_INTEGER;
    UNPROTECT(1);
    error("Could not find network %s.",BN_NAME(bn));
    return(result);
  }
  INTEGER(result)[0] = RedoNetOper_bn(netica_handle,-1.0);
  UNPROTECT(1);
  return(result);
}

