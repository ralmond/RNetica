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
//
// Design 4.  Networks are now RC classes registered in a special
// environment in the Session object.  We should now be able to fetch
// networks by name from the session.  We can make sure the stale
// pointers are deleted, as the RC objects can be destructively
// modified. 

net_bn* GetNetworkPtr (SEXP netobj) {
  net_bn* net_ptr = NULL;
  SEXP exPTR;
  PROTECT(exPTR=GET_FIELD(netobj,bnatt));
  if (exPTR) {
    net_ptr = (net_bn*) R_ExternalPtrAddr(exPTR);
  }
  UNPROTECT(1);
  return net_ptr;
}


void SetNetworkPtr (SEXP netobj, net_bn* net_ptr) {
  SEXP exPTR;
  PROTECT(exPTR=GET_FIELD(netobj,bnatt));
  if (exPTR) {
    R_SetExternalPtrAddr(exPTR,net_ptr);
  } else {
    UNPROTECT(1);
    PROTECT(exPTR = R_MakeExternalPtr(net_ptr,bnatt,R_NilValue));
  }
  SET_FIELD(netobj,bnatt,exPTR);
  UNPROTECT(1);
  return;
}

extern SEXP RN_DeactivateBN(SEXP net) {
  SEXP exPTR;
  PROTECT(exPTR=GET_FIELD(net,bnatt));
  if (exPTR) {
    R_ClearExternalPtr(exPTR);
  }
  SET_FIELD(net,bnatt,exPTR);
  UNPROTECT(1);
  return net;
}


/**
 * This is a small utility function meant to be used from within
 * toString to determine if the pointer is live or not.
 */
SEXP RN_isBNActive(SEXP bn) {
  SEXP bnPtr, result;
  PROTECT(result=allocVector(LGLSXP,1));
  LOGICAL(result)[0]=FALSE;
  PROTECT(bnPtr = GET_FIELD(bn,bnatt));
  if (bnPtr && R_ExternalPtrAddr(bnPtr)) {
    LOGICAL(result)[0] = TRUE;
  }
  UNPROTECT(2);
  return result;
}

/**
 * Tests whether or not an object is a Netica Network.
 * Should be able to replace this with a simple call to is() as BNs
 * are now official RC classes.
 */
Rboolean isNeticaBN(SEXP obj) {
  return inherits(obj,NetworkClass);
}


// Copyied from NETICA API manual because it looks useful.
// This is now key to the new strategy.  
// May yet be a use for this, but it now needs a session argument
net_bn* RN_AS_NET (const char* name, environ_ns* netica_env){
    int nth = 0;
    net_bn* net;
    do {
        net = GetNthNet_bn (nth++, netica_env);
    } while (net && strcmp (name, GetNetName_bn (net)) != 0);
    return net;
}

/* I just can't seem to figure out how to get this to work.  The
   NEW_OBJECT function doesn't seem to work properly for R6 classes
   and I can't figure out how to call a non-exported method from C
   code. */

/* SEXP CreateBNObject(const char* name, SEXP sessobj) { */
/*   SEXP sname, callme, nodeenv, bn; */
/*   PROTECT(sname= allocVector(STRSXP,1)); */
/*   SET_STRING_ELT(sname,0,mkChar(name)); */

/*   /\* PROTECT(bn = NEW_OBJECT(bnclass)); *\/ */
/*   /\* /\\* Not very well documented, but I think this makes a blank object *\/ */
/*   /\*    and does not call the $initialize() function, so we need to do *\/ */
/*   /\*    those operations manually. *\\/ *\/ */
/*   /\* SET_FIELD(bn,namefield,sname); *\/ */
/*   /\* SET_FIELD(bn,sessionfield,sessobj); *\/ */

/*   PROTECT(callme=lang3(install("BNmaker"),sname,sessobj)); */
/*   SET_TAG(CDR(callme),install("Name"));  */
/*   SET_TAG(CDDR(callme),install("Session"));  */
/*   PROTECT(bn=eval(callme,R_FindPackageEnv(RNeticaPackage))); */
/*   /\* SET_FIELD(bn,nodesfield,env); *\/ */
/*   /\* UNPROTECT(1); *\/ */

/*   RN_RegisterNetwork(sessobj,name,bn); */
/*   UNPROTECT(3); */
/*   return bn; */
/* } */



/**
 * This function allocates a back pointer R object
 * for a newly created net.
 *
 * This checks the session for an existing bn object with the same
 * name.
 * If none exists:  A new one is created.
 * If one exists, but is null, this this is an old inactive object,
 * reuse.
 * If one exitss, but has a different pointer, then something is wrong
 * and we need to generate an error.
 */
/* As we can't create R6 objects from inside C, we need to pass in a
   blank object which we will populate. */

SEXP MakeNet_RRef(net_bn* net, const char* name, SEXP sessobj, SEXP blank) {
  net_bn* bn_ptr;
  SEXP bn, sname;

  PROTECT(bn=RN_FindNetworkStr(sessobj,name));
  Rprintf("Searching for existing node named %s.\n",name);
  if (isNull(bn) || RX_isUnbound(bn)) {
    Rprintf("Didn't find net named %s.\n",name);
    /* Didn't find one, need to make a new one (or use the blank). */
    UNPROTECT(1);
    PROTECT(bn=blank);
  } else {
    Rprintf("Found a net named %s, object of type %d.\n",name,TYPEOF(bn));
    if (TYPEOF(bn)==SYMSXP) {
      Rprintf("It is a symbol:  %s.\n",PRINTNAME(bn));
    }
  }
  bn_ptr = GetNetworkPtr(bn);
  if (bn_ptr && bn_ptr != net) {
    /* Pointer is not null and not equal to the current net:
       something is wrong. */
    error("RNetica Internal error:  pointer mismatch for net %s\n",name);
  }
  Rprintf("Setting netica handle.\n");
  SetNetworkPtr(bn,net);
  PROTECT(sname= allocVector(STRSXP,1));
  SET_STRING_ELT(sname,0,mkChar(name));
  Rprintf("Setting node to name %s.\n",name);
  SET_FIELD(bn,namefield,sname);
  Rprintf("Setting backpointer to session.\n");
  SET_FIELD(bn,sessionfield,sessobj);
  Rprintf("Registering network with session.\n");
  RN_RegisterNetwork(sessobj,name,bn);
  UNPROTECT(2);
  return bn;
}

SEXP RN_New_Nets(SEXP namelist, SEXP session, SEXP blanks) {
  R_len_t n, nn = length(namelist);
  const char* name;
  net_bn* netica_handle;
  SEXP bn,blank;
  environ_ns* netica_env = GetSessionPtr(session);
  SEXP handles;

  PROTECT(handles=allocVector(VECSXP,nn));
  for (n=0; n < nn; n++) {
    name = CHAR(STRING_ELT(namelist,n));
    PROTECT(blank = VECTOR_ELT(blanks,n));
    netica_handle = NewNet_bn(name,netica_env);
    PROTECT(bn = MakeNet_RRef(netica_handle,name,session,blank));
    Rprintf("Bn object created.\n");
    SET_VECTOR_ELT(handles,n,bn);
    UNPROTECT(2);
  }
  UNPROTECT(1);
  return(handles);
}

SEXP RN_Delete_Nets(SEXP netlist, SEXP session) {

  R_len_t n, nn = length(netlist);
  net_bn* netica_handle;
  SEXP bn, bnhandle;
  environ_ns* netica_env = GetSessionPtr(session);

  for (n=0; n < nn; n++) {
    PROTECT(bn = VECTOR_ELT(netlist,n));
    PROTECT(bnhandle = GET_FIELD(bn,bnatt));
    netica_handle = (net_bn*) R_ExternalPtrAddr(bnhandle);
    if (netica_handle) {
      //Find and delete all node.
      RN_Free_Nodes(GetNetNodes_bn(netica_handle),bn);
      //Nodes will be autodeleted when net is deleted.
      DeleteNet_bn(netica_handle);
      /* Clear the handle */
      R_ClearExternalPtr(bnhandle);
      SET_FIELD(bn,bnatt,bnhandle); //Probably not needed.

      /* R_ReleaseObject(bn);       */
      /* Net objects are no longer protected/released in C code,
         rather they exist in the session environment, so rm()ing
         them from there makes the garbage */
    } else {
      warning("Did not find a network named %s.", BN_NAME(bn));
    }
    UNPROTECT(2);
  }
  return(netlist);
}

/* This is now unnecessary, because the reference are cached in the
   session object.  Instead this checks to make sure that the named
   networks actually exists and it returns a vector of true/false
   values. */
SEXP RN_Named_Nets(SEXP namelist, SEXP session, SEXP blanks) {
  R_len_t n, nn = length(namelist);
  const char* name;
  net_bn* netica_handle;
  SEXP bnhandlelist, bn, blank;
  environ_ns* netica_env = GetSessionPtr(session);

  PROTECT(bnhandlelist = allocVector(VECSXP,nn));
  for (n=0; n < nn; n++) {
    name = CHAR(STRING_ELT(namelist,n));
    netica_handle = RN_AS_NET(name, netica_env);
    if (netica_handle) {
      /* Fetch the bn object. */
      /* This will generate an error an an object exists and has a
         different pointer */
      PROTECT(blank = VECTOR_ELT(blanks,n));
      PROTECT(bn = MakeNet_RRef(netica_handle,GetNetName_bn(netica_handle),
                                session,blank));
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

SEXP RN_GetNth_Nets(SEXP nlist, SEXP session, SEXP blanks) {
  R_len_t n, nn = length(nlist);
  int *netno;
  net_bn* netica_handle;
  SEXP bnhandlelist, bn, blank;
  environ_ns* netica_env = GetSessionPtr(session);

  PROTECT(bnhandlelist = allocVector(VECSXP,nn));
  netno = INTEGER(nlist);
  for (n=0; n < nn; n++) {
    netica_handle = GetNthNet_bn(netno[n],netica_env);
    if (netica_handle) {
      /* Get corresponding R object */
      PROTECT(blank = VECTOR_ELT(blanks,n));
      PROTECT(bn = MakeNet_RRef(netica_handle,GetNetName_bn(netica_handle),
                                session,blank));
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

SEXP RN_Copy_Nets(SEXP nets, SEXP namelist, SEXP options, 
                  SEXP session, SEXP blanks) {
  //RN_Define_Symbols();
  R_len_t n, nn = length(namelist);
  const char *newname, *opt;
  net_bn *old_net, *new_net;
  SEXP bnhandlelist, old_bn, new_bn, blank;
  environ_ns* netica_env = GetSessionPtr(session);

  opt = CHAR(STRING_ELT(options,0));

  PROTECT(bnhandlelist = allocVector(VECSXP,nn));
  for (n=0; n < nn; n++) {
    newname = CHAR(STRING_ELT(namelist,n));
    PROTECT(old_bn = VECTOR_ELT(nets,n)); 
    old_net = GetNetworkPtr(old_bn);
    if (old_net) {
      new_net = CopyNet_bn(old_net,newname,netica_env,opt);
      PROTECT(blank = VECTOR_ELT(blanks,n));
      new_bn = MakeNet_RRef(new_net,newname,session,blank);
      SET_VECTOR_ELT(bnhandlelist,n,new_bn);
    } else {
      SET_VECTOR_ELT(bnhandlelist,n,R_NilValue);
      warning("Did not find a network named %s.",BN_NAME(old_bn));
    }
    UNPROTECT(1);
  }
  UNPROTECT(1); 
  return(bnhandlelist);
}

///////////////////////////////////////////////////////////////////////
// Net level File I/O
///////////////////////////////////////////////////////////////////////

SEXP RN_Read_Nets(SEXP filelist, SEXP session, SEXP blanks) {

  R_len_t n, nn = length(filelist);
  SEXP filename;
  const char *name;
  stream_ns *file;
  net_bn* netica_handle;
  SEXP result, bn, blank;
  environ_ns* netica_env = GetSessionPtr(session);

  PROTECT(result = allocVector(VECSXP,nn));
  
  for (n=0; n < nn; n++) {
      filename = STRING_ELT(filelist,n);
      file = NewFileStream_ns(CHAR(filename),netica_env,NULL);
      netica_handle = ReadNet_bn(file,NO_WINDOW); //NO_WINDOW looks
                                                  //like best opition choice.
      DeleteStream_ns(file);

    if (netica_handle) {
      name = GetNetName_bn(netica_handle);
      PROTECT(blank = VECTOR_ELT(blanks,n));
      bn = MakeNet_RRef(netica_handle,name,session,blank);
      SET_FIELD(bn,pathfield,filename);
      SET_VECTOR_ELT(result,n,bn);

    } else {
      SET_VECTOR_ELT(result,n,R_NilValue);
      warning("Could not find network for file %s.",CHAR(filename));
    }
  }
  UNPROTECT(1);
  return(result);
}

SEXP RN_Write_Nets(SEXP nets, SEXP filelist, SEXP session) {
  R_len_t n, nn = length(filelist);
  SEXP filename;
  stream_ns *file;
  SEXP bn;
  net_bn* netica_handle;
  environ_ns* netica_env = GetSessionPtr(session);

  for (n=0; n < nn; n++) {
    PROTECT(bn = VECTOR_ELT(nets,n));
    netica_handle = GetNetworkPtr(bn);

    if (netica_handle) {
      filename = STRING_ELT(filelist,n);
      file = NewFileStream_ns(CHAR(filename),netica_env,NULL);
      // To keep it from being saved.
      WriteNet_bn(netica_handle,file);
      DeleteStream_ns(file);
      SET_FIELD(bn,pathfield,filename);
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

  netica_handle = GetNetworkPtr(bn);
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

  netica_handle = GetNetworkPtr(bn);
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

//FIXME
SEXP RN_SetNetName(SEXP bn, SEXP newnames, SEXP session) {
  const char *newname;
  net_bn *netica_handle, *other_net;
  environ_ns* netica_env = GetSessionPtr(session);

  netica_handle = GetNetworkPtr(bn);

  if (netica_handle) {
    newname = CHAR(STRING_ELT(newnames,0)); 
    other_net = RN_AS_NET(newname,netica_env);
    if ( other_net && other_net != netica_handle) {
      warning("There is already a network named %s.",newname);
    } else {
      SetNetName_bn(netica_handle,newname);
      // We need to change the bn object to reflect the new name.
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

  netica_handle = GetNetworkPtr(bn);

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

  netica_handle = GetNetworkPtr(bn);

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

  netica_handle = GetNetworkPtr(bn);
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

  netica_handle = GetNetworkPtr(bn);

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

  netica_handle = GetNetworkPtr(bn);

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

  netica_handle = GetNetworkPtr(bn);

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

  netica_handle = GetNetworkPtr(bn);

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

  netica_handle = GetNetworkPtr(bn);
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

  netica_handle = GetNetworkPtr(bn);

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

  netica_handle = GetNetworkPtr(bn);
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

  netica_handle = GetNetworkPtr(bn);
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


