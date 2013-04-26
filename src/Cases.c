/**
 * Cases.c --- This file contains functions for working with case set files
 */

#include <string.h>
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
    result = SetCaseFileDelimChar_ns(INT(newchar)[0],RN_netica_env);
  }
  return ScalerInteger(result);
}

SEXP RN_MissingCode(SEXP newchar) {
  int result;
  if (isNull(newchar)) {
    result = SetMissingDataChar_ns(QUERY_ns,RN_netica_env);
  } else {
    result = SetMissingDataChar_ns(INT(newchar)[0],RN_netica_env);
  }
  return ScalerInteger(result);
}

SEXP RN_WriteFindingsToFile(SEXP nodes, SEXP casefile, SEXP id, SEXP freq) {
  nodelist_bn* nodelist = RN_AS_NODELIST(nodes,NULL);
  long id = freq =-1;
  if (!isNull(id)) id = INT(id)[0];
  if (!isNull(freq)) id = INT(id)[0];
  const char *filename = CHAR(STRING_ELT(casefile,0));
  stream_ns *file = NewFileStream_ns(filename,RN_netica_env,NULL);
  WriteNetFindings_bn(nodelist,file,id,freq);
  DeleteStream_ns(file);
  DeleteNodeList_bn(nodelist);
  return R_NilValue;
}

/**
 * Much of the next bit follows 
 * http://www.stat.uiowa.edu/~luke/R/references/weakfinex.html
 */


