/**
 * Cases.c --- This file contains functions for working with case set files
 */

#define _GNU_SOURCE         /* Needed for mempcpy */
#include <string.h>
#ifdef __APPLE__   /* Fails for Macs, need to define mempcpy
                      explicitly */
/* From Gnulib */
void *
mempcpy (void *dest, const void *src, size_t n)
{
  return (char *) memcpy (dest, src, n) + n;
}
#endif
#include <R.h>
#include <Rdefines.h>
#include <RNetica.h>

/**
 * Case sets are generally tab separated files of variable values,
 * with each column representing a variable.  Generally, we will want
 * to pass one of two things when Netica wants a caseset:  (1) A file
 * name of a file containing a case set or (2) a data frame, which we
 * can write to a memory buffer and create a case set from that.
 */

SEXP RN_CaseFileDelimiter(SEXP newchar) {
  int result;
  if (isNull(newchar)) {
    result = SetCaseFileDelimChar_ns(QUERY_ns,RN_netica_env);
  } else {
    result = SetCaseFileDelimChar_ns(INTEGER(newchar)[0],RN_netica_env);
  }
  return ScalarInteger(result);
}

SEXP RN_MissingCode(SEXP newchar) {
  int result;
  if (isNull(newchar)) {
    result = SetMissingDataChar_ns(QUERY_ns,RN_netica_env);
  } else {
    result = SetMissingDataChar_ns(INTEGER(newchar)[0],RN_netica_env);
  }
  return ScalarInteger(result);
}


/**
 * Much of the next bit follows 
 * http://www.stat.uiowa.edu/~luke/R/references/weakfinex.html
 *
 * This is not well documented in Luke's web page, but I've put the
 * stream pointer in as the "key" of the weak pointer list, and the
 * NeticaCaseStream object as the "value".  This allows me to set the
 * pointer to NULL when I'm closing all open streams, say on detatch.
 */
/**
 * The Netica API uses file streams in four places:
 * 1) Reading networks
 * 2) Writing networks
 * 3) Reading Case files
 * 4) Writing Case files
 * Only in case 3 do we need to keep the stream open after returning
 * to R.  In particular, that means we can stash information about the
 * current case position in the NeticaStream object.  However,
 * allowing for case 4 as well allows us to support memory streams.
 */


/**
 * Finalizer:  calling this multiple times should be harmless.
 */
SEXP CaseStreamClose (SEXP streamptr) {
  stream_ns *stream_handle;
  if (streamptr == NULL || isNull(streamptr)) {
    //Already closed, nothing to do
  } else {
    if (TYPEOF(streamptr) != EXTPTRSXP || 
        R_ExternalPtrTag(streamptr) != casestreamatt) {
      warning("Trying to close a non-stream pointer");
    } else {
      stream_handle = (stream_ns*) R_ExternalPtrAddr(streamptr);
      if (stream_handle != NULL) {
        DeleteStream_ns(stream_handle);
        R_ClearExternalPtr(streamptr);
      }
    }
  }
  return R_NilValue;
}

void AddStreamRef(SEXP ref) {
  SEXP s, streams, next=NULL, last=NULL;
  streams = CDR(CaseStreamList);
  for (s = streams; s != R_NilValue; s = next) {
    SEXP r = CAR(s);
    SEXP key = R_WeakRefKey(r);
    next = CDR(s);
    if (key == R_NilValue || R_ExternalPtrAddr(key)==NULL) {
      if (last == NULL) streams = next;
      else SETCDR(last,next);
    } else {
      last = s;
    }
  }
  SETCDR(CaseStreamList, CONS(ref,streams));
}

void CloseOpenCaseStreams () {
  SEXP s, streams, next=NULL, last=NULL;
  streams = CDR(CaseStreamList);
  for (s = streams; s != R_NilValue; s = next) {
    SEXP r = CAR(s);
    SEXP key = R_WeakRefKey(r);
    SEXP stream = R_WeakRefValue(r);
    next = CDR(s);
    if (key != R_NilValue) {
      CaseStreamClose(key);
      if (stream && stream != R_NilValue) {
        setAttrib(stream,casestreamatt,R_NilValue);
      }
    }
  }
}


SEXP RN_isCaseStreamActive(SEXP stream) {
  SEXP stPtr, result;
  PROTECT(result=allocVector(LGLSXP,1));
  LOGICAL(result)[0]=FALSE;
  PROTECT(stPtr = getAttrib(stream,casestreamatt));
  if (!isNull(stPtr) && R_ExternalPtrAddr(stPtr)) {
    LOGICAL(result)[0] = TRUE;
  }
  UNPROTECT(2);
  return result;
}

SEXP RN_OpenCaseFileStream (SEXP path, SEXP stream) {
  const char* pathname=CHAR(STRING_ELT(path,0));
  stream_ns* str = 
    NewFileStream_ns (pathname,RN_netica_env, NULL);
  if (str == NULL ) 
    return R_NilValue;
  else {
    SEXP stPtr, ref;
    if (isNull(stream)) {
      //Allocate new stream object
      PROTECT(stream = allocVector(STRSXP,1));
      SET_STRING_ELT(stream,0,mkChar(pathname));
      SET_CLASS(stream,casefilestreamclass);
    } else {
      PROTECT(stream); //To keep protect count constant
    }
    PROTECT(stPtr = R_MakeExternalPtr(str,casestreamatt, R_NilValue));
    setAttrib(stream,casestreamatt,stPtr);
    PROTECT(ref = R_MakeWeakRefC(stPtr,stream,
                                 (R_CFinalizer_t) &CaseStreamClose, 
                                 TRUE));
    AddStreamRef(ref);
    setAttrib(stream,casestreampathatt,path);
    // Use pos of NULL to indicate start from the beginning.
    setAttrib(stream,casestreamposatt,R_NilValue);
    setAttrib(stream,casestreamlastidatt,R_NilValue);
    setAttrib(stream,casestreamlastfreqatt,R_NilValue);
    setAttrib(stream,casestreamdfatt,R_NilValue);
    setAttrib(stream,casestreamdfnameatt,R_NilValue);
    UNPROTECT(3);
    return stream;
  }

}


SEXP RN_OpenCaseMemoryStream (SEXP label, SEXP stream) {
  const char* lab=CHAR(STRING_ELT(label,0));
  //Rprintf("Opening Stream for R object %s\n",lab);
  stream_ns* str = 
    NewMemoryStream_ns (lab,RN_netica_env, NULL);
  if (str == NULL ) 
    return R_NilValue;
  else {
    SEXP stPtr, ref;
    if (isNull(stream)) {
      //Allocate new stream object
      PROTECT(stream = allocVector(STRSXP,1));
      SET_STRING_ELT(stream,0,mkChar(lab));
      SET_CLASS(stream,memorystreamclass);
    } else {
      PROTECT(stream); //To keep protect count constant
    }
    PROTECT(stPtr = R_MakeExternalPtr(str,casestreamatt, R_NilValue));
    setAttrib(stream,casestreamatt,stPtr);
    PROTECT(ref = R_MakeWeakRefC(stPtr,stream,
                                 (R_CFinalizer_t) &CaseStreamClose, 
                                 TRUE));
    AddStreamRef(ref);
    setAttrib(stream,casestreamdfnameatt,label);
    // Use pos of NULL to indicate start from the beginning.
    setAttrib(stream,casestreamposatt,R_NilValue);
    setAttrib(stream,casestreamlastidatt,R_NilValue);
    setAttrib(stream,casestreamlastfreqatt,R_NilValue);
    setAttrib(stream,casestreamdfatt,R_NilValue);
    setAttrib(stream,casestreampathatt,R_NilValue);
    UNPROTECT(3);
    return stream;
  }

}


SEXP RN_CloseCaseStream (SEXP stream) {

  if (!isNeticaStream(stream)) {
    warning("Trying to close a non-Stream object.");
  }
  CaseStreamClose(getAttrib(stream,casestreamatt));
  setAttrib(stream,casestreamatt,R_NilValue);
  return(stream);
}


/**
 * Tests whether or not an object is a Netica stream
 */
int isNeticaStream(SEXP obj) {
  SEXP klass;
  int result = FALSE;
  PROTECT(klass = getAttrib(obj,R_ClassSymbol));
  R_len_t k, kk=length(klass);
  for (k=0; k<kk; k++) {
    if(strcmp(CaseStreamClass,CHAR(STRING_ELT(klass,k))) == 0) {
      result = TRUE;
      break;
    }
  }
  UNPROTECT(1);
  return result;
}

SEXP RN_SetMemoryStreamContents(SEXP stream, SEXP contents) {

  char *buf =NULL;
  void *pos;
  long totlen=0;
  size_t irow, nrow = length(contents);
  if (!isNull(contents)) {
    for (irow = 0; irow < nrow; irow++) {
      totlen += strlen(CHAR(STRING_ELT(contents,irow)));
      totlen ++; //For eol.
    }
    buf = (char *) R_alloc(totlen+1,sizeof(char));
    if (buf == NULL) {
      error("Could not allocate memory for string buffer.");
      return R_NilValue;
    }
    pos = (void *) buf;
    for (irow = 0; irow < nrow; irow++) {
      pos = mempcpy(pos, (const void*) CHAR(STRING_ELT(contents,irow)),
                    strlen(CHAR(STRING_ELT(contents,irow))));
      pos = mempcpy(pos,"\n",1);
    }
    mempcpy(pos,"\0",1);
  }
  //Rprintf("Length at creation time %ld\n",totlen);
  SetStreamContents_ns(GetCaseStream_Handle(stream),buf,totlen,TRUE);
  const char *obuf = GetStreamContents_ns(GetCaseStream_Handle(stream),&totlen);
  //Rprintf("Buffer contents now:\n%s\n",obuf);

  setAttrib(stream,casestreamposatt,R_NilValue);
  setAttrib(stream,casestreamlastidatt,R_NilValue);
  setAttrib(stream,casestreamlastfreqatt,R_NilValue);
  return (stream);  
}


SEXP RN_GetMemoryStreamContents(SEXP stream) {
  SEXP contents;
  char *line;
  char *buf;
  long ipos, totlen=0;
  size_t irow, nrow;

  const char *nbuf = GetStreamContents_ns(GetCaseStream_Handle(stream),&totlen);
  //Copy so we can tokenize it.
  Rprintf("Buffer length %ld\n",(size_t) totlen);
  if (totlen == 0) return R_NilValue;
  buf = (char *) R_alloc((size_t) totlen+1,sizeof(char));
  if (buf == NULL) {
    error("Could not allocate memory for string buffer.");
    return R_NilValue;
  }
  nrow =0;
  for (ipos=0; ipos < totlen; ipos++) {
    buf[ipos]=nbuf[ipos];
    if (buf[ipos]=='\n') nrow++;
  }
  buf[totlen] = '\0';
  Rprintf("ipos = %ld, nrow=%ld\n",ipos,nrow);
  PROTECT(contents = allocVector(STRSXP,nrow));
  line = strtok(buf,"\n");
  irow=0;
  while (line) {
    Rprintf("Line %ld: %s\n",irow,line);
    SET_STRING_ELT(contents,irow++,mkChar(line));
    line = strtok(NULL,"\n");
  }

  UNPROTECT(1);
  return (contents);  
}


SEXP RN_WriteFindings(SEXP nodes, SEXP pathOrStream, SEXP id, SEXP freq) {
  nodelist_bn* nodelist = RN_AS_NODELIST(nodes,NULL);
  long idnum = -1;
  double freqnum = -1.0;
  stream_ns *stream;
  caseposn_bn pos;
  if (!isNull(id)) idnum = INTEGER(id)[0];
  if (!isNull(freq)) freqnum = REAL(freq)[0];
  if (isNeticaStream(pathOrStream)) {
    stream = GetCaseStream_Handle(pathOrStream);
    if (stream == NULL) {
      DeleteNodeList_bn(nodelist);
      error("RN_WriteFindings:  Stream is not open.");
    }
  } else {
    const char* filename = CHAR(STRING_ELT(pathOrStream,0));
    stream = NewFileStream_ns(filename,RN_netica_env,NULL);
  }
  pos = WriteNetFindings_bn(nodelist,stream,idnum,freqnum);
  if (isNeticaStream(pathOrStream)) {
    setAttrib(pathOrStream,casestreamposatt,ScalarInteger(pos));
    setAttrib(pathOrStream,casestreamlastidatt,ScalarInteger(idnum));
    setAttrib(pathOrStream,casestreamlastfreqatt,ScalarReal(freqnum));
  } else {
    DeleteStream_ns(stream);
  }
  DeleteNodeList_bn(nodelist);
  return pathOrStream;
}

SEXP RN_ReadFindings(SEXP nodes, SEXP stream, SEXP pos, SEXP add) {
  nodelist_bn* nodelist = RN_AS_NODELIST(nodes,NULL);
  long idnum = -1;
  double freqnum = -1.0;
  bool_ns addflag=FALSE;
  caseposn_bn case_posn=0;
  //Translate string positions to integer ones.
  if (isInteger(pos)) {
    case_posn = INTEGER(pos)[0];
  } else if (isString(pos)) {
    if(strcmp(CHAR(STRING_ELT(pos,0)),"FIRST") == 0) {
      case_posn = FIRST_CASE;
    } else if(strcmp(CHAR(STRING_ELT(pos,0)),"NEXT") == 0) {
      case_posn = NEXT_CASE;
    } else {
      error("RN_ReadFindings: Pos should be 'FIRST', 'NEXT' or integer.");
    }
  } else {
    error("RN_ReadFindings: Pos should be an integer or string scalar.");
  }
  if (!isNull(add)) addflag = INTEGER(add)[0];
  if (!isNeticaStream(stream)) {
    error("RN_ReadFindings:  stream is not a valid Netica stream.");
  } 
  if (!RN_isCaseStreamActive(stream)) {
    error("RN_ReadFindings:  stream is not a open.");
  }
  stream_ns *stream_handle = GetCaseStream_Handle(stream);
  //Rprintf("RN_ReadFindings: Stream_handle %ld.\n",stream_handle);
  ReadNetFindings2_bn(&case_posn,stream_handle,addflag,nodelist,
                      &idnum,&freqnum);
  if (case_posn == NO_MORE_CASES) {
    setAttrib(stream,casestreamposatt,ScalarInteger(NA_INTEGER));
  } else {
    setAttrib(stream,casestreamposatt,ScalarInteger(case_posn));
  }
  setAttrib(stream,casestreamlastidatt,ScalarInteger(idnum));
  setAttrib(stream,casestreamlastfreqatt,ScalarReal(freqnum));

  DeleteNodeList_bn(nodelist);
  return stream;
}
