/**
 * Networks.c --- These files describe functions for creating,
 * destroying, saving and other high level functions on networks.
 */

#include <string.h>
#include <R.h>
#include <Rdefines.h>
#include <RNetica.h>


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

/**
 * Tests whether or not an object is a Netica Node.
 */
int isNeticaBN(SEXP obj) {
  SEXP klass;
  int result = FALSE;
  PROTECT(klass = getAttrib(obj,R_ClassSymbol));
  R_len_t k, kk=length(klass);
  for (k=0; k<kk; k++) {
    if(strcmp(NeticaClass,CHAR(STRING_ELT(klass,k))) == 0) {
      result = TRUE;
      break;
    }
  }
  UNPROTECT(1);
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

/**
 * This function allocates a back pointer R object
 * for a newly created net.
 */
SEXP MakeNet_RRef(net_bn* net, const char* name) {
  SEXP bn, bnhandle;

  bn = allocVector(STRSXP,1);
  R_PreserveObject(bn);
  /* Return the network name */
  SET_STRING_ELT(bn,0,mkChar(name));
  /* Set the handle as an attribute. */
  PROTECT(bnhandle = R_MakeExternalPtr(net,bnatt,R_NilValue));
  setAttrib(bn,bnatt,bnhandle);
  SET_CLASS(bn,bnclass);
  
  /* Set a back pointer to the R object in the Netica Object */
  SetNet_RRef(net,bn);
  UNPROTECT(1);
  return bn;
}


SEXP RN_New_Nets(SEXP namelist) {
  R_len_t n, nn = length(namelist);
  const char* name;
  net_bn* netica_handle;
  SEXP bnhandlelist, bn;
    
  PROTECT(bnhandlelist = allocVector(VECSXP,nn));
  for (n=0; n < nn; n++) {
    name = CHAR(STRING_ELT(namelist,n));
    netica_handle = RN_AS_NET(name);
    if (netica_handle) {
      warning("Network named %s already exists.",name);
      SET_VECTOR_ELT(bnhandlelist,n,R_NilValue);
    } else {
      netica_handle = NewNet_bn(name,RN_netica_env);
      bn = MakeNet_RRef(netica_handle,name);
      /* Finally, stick it in array */
      SET_VECTOR_ELT(bnhandlelist,n,bn);
    }
  }

  UNPROTECT(1);
  return(bnhandlelist);
}

SEXP RN_Delete_Nets(SEXP netlist) {

  R_len_t n, nn = length(netlist);
  net_bn* netica_handle;
  SEXP bn, bnhandle, result;

  PROTECT(result = allocVector(VECSXP,nn));

  for (n=0; n < nn; n++) {
    PROTECT(bn = VECTOR_ELT(netlist,n));
    PROTECT(bnhandle = getAttrib(bn,bnatt));
    netica_handle = (net_bn*) R_ExternalPtrAddr(bnhandle);
    if (netica_handle) {
      //Find and delete all node.
      RN_Free_Nodes(GetNetNodes_bn(netica_handle));
      //Nodes will be autodeleted when net is deleted.
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
  return(result);
}


SEXP RN_Named_Nets(SEXP namelist) {
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
      if (!bn || !isNeticaBN(bn)) {
        //Corrupted pointer, rebuild it.
        bn = MakeNet_RRef(netica_handle,GetNetName_bn(netica_handle));
      }
      /* Now stick it in array */
      SET_VECTOR_ELT(bnhandlelist,n,bn);
    } else {
      SET_VECTOR_ELT(bnhandlelist,n,R_NilValue);
      //warning("Did not find a network named %s.",name);
    }
  }
  UNPROTECT(1);
  return(bnhandlelist);
}

SEXP RN_GetNth_Nets(SEXP nlist) {
  R_len_t n, nn = length(nlist);
  int *netno;
  net_bn* netica_handle;
  SEXP bnhandlelist, bn;


  PROTECT(bnhandlelist = allocVector(VECSXP,nn));
  netno = INTEGER(nlist);
  for (n=0; n < nn; n++) {
    netica_handle = GetNthNet_bn(netno[n],RN_netica_env);
    if (netica_handle) {
      /* Get corresponding R object */
      bn = GetNet_RRef(netica_handle);
      if (!bn || !isNeticaBN(bn)) {
        //Corrupted pointer, rebuild it.
        bn = MakeNet_RRef(netica_handle,GetNetName_bn(netica_handle));
      }
      /* Now stick it in array */
      SET_VECTOR_ELT(bnhandlelist,n,bn);
      //UNPROTECT(1); //bn is preserved, so don't need protection.
    } else{
      SET_VECTOR_ELT(bnhandlelist,n,R_NilValue);
      //warning("Did not find a network named %s.",name);
    }
  }
  UNPROTECT(1);
  return(bnhandlelist);
}

SEXP RN_Copy_Nets(SEXP nets, SEXP namelist, SEXP options) {
  //RN_Define_Symbols();
  R_len_t n, nn = length(namelist);
  const char *newname, *opt;
  net_bn *old_net, *new_net;
  SEXP bnhandlelist, old_bn, new_bn;

  opt = CHAR(STRING_ELT(options,0));

  PROTECT(bnhandlelist = allocVector(VECSXP,nn));
  for (n=0; n < nn; n++) {
    newname = CHAR(STRING_ELT(namelist,n));
    old_bn = VECTOR_ELT(nets,n); //Preserved, don't need to protect
    old_net = GetNeticaHandle(old_bn);
    if (old_net) {
      new_net = CopyNet_bn(old_net,newname,RN_netica_env,opt);
      new_bn = MakeNet_RRef(new_net,newname);
      SET_VECTOR_ELT(bnhandlelist,n,new_bn);
    } else {
      SET_VECTOR_ELT(bnhandlelist,n,R_NilValue);
      warning("Did not find a network named %s.",BN_NAME(old_bn));
    }
      
  }
  UNPROTECT(1); 
  return(bnhandlelist);
}

///////////////////////////////////////////////////////////////////////
// Net level File I/O
///////////////////////////////////////////////////////////////////////

SEXP RN_Read_Nets(SEXP filelist) {

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
      bn = MakeNet_RRef(netica_handle,name);
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
  const char *filename;
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
  const char *filename;
  net_bn* netica_handle;
  SEXP result;

  PROTECT(result = allocVector(STRSXP,1));

  netica_handle = GetNeticaHandle(bn);
  if (netica_handle) {
    filename = GetNetFileName_bn(netica_handle);
    SET_STRING_ELT(result,0,mkChar(filename));
  } else {
    SET_STRING_ELT(result,0,NA_STRING);
    warning("Could not find network %s.",BN_NAME(bn));
  }
  UNPROTECT(1);
  return(result);
}

//////////////////////////////////////////////////////////////////////////
// Getters and Setters for Global Net properties.
//////////////////////////////////////////////////////////////////////////

SEXP RN_GetNetName(SEXP bn) {
  const char *netname;
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
  net_bn *netica_handle, *other_net;

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

