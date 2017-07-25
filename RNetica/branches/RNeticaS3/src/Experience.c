/**
 * Experience.c --- This file contains functions for creating,
 * destroying, and modifying CPTs via learning.
 */

#include <string.h>
#include <R.h>
#include <Rdefines.h>
#include <RNetica.h>

SEXP RN_GetNodeExperience(SEXP node, SEXP states) {
  node_bn* node_handle;

  node_handle = GetNodeHandle(node);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(node));
    return(R_NilValue);
  } else {
    return ScalarReal(GetNodeExperience_bn(node_handle, 
                                           RN_AS_STATE_BN(states)));
  }
}

SEXP RN_SetNodeExperience(SEXP node, SEXP states, SEXP weight) {
  node_bn* node_handle;

  node_handle = GetNodeHandle(node);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(node));
  } else {
    SetNodeExperience_bn(node_handle, RN_AS_STATE_BN(states),
                         REAL(weight)[0]);
  }
  return node;
}

SEXP RN_FadeCPT(SEXP node, SEXP degree) {
  node_bn* node_handle;

  node_handle = GetNodeHandle(node);
  if (!node_handle) {
    error("Could not find node %s.",NODE_NAME(node));
  } else {
    FadeCPTable_bn(node_handle, REAL(degree)[0]);
  }
  return node;
}

SEXP RN_LearnFindings(SEXP nodelist, SEXP weight) {
  nodelist_bn* targets = RN_AS_NODELIST(nodelist,NULL);
  ReviseCPTsByFindings_bn(targets,0,REAL(weight)[0]);
  DeleteNodeList_bn(targets);
  return R_NilValue;
}


SEXP RN_LearnCaseStream(SEXP stream, SEXP nodelist, SEXP weight) {
  stream_ns *stream_handle = GetCaseStream_Handle(stream);
  if (stream_handle == NULL) {
    error("RN_LearnFindingCaseStream:  Stream is not open.");
  }
  nodelist_bn* targets = RN_AS_NODELIST(nodelist,NULL);
  ReviseCPTsByCaseFile_bn(stream_handle,targets,0,REAL(weight)[0]);
  DeleteNodeList_bn(targets);
  return R_NilValue;
}

/**
 * This is meant to be an internal function called by other methods.
 * At the momment, RNetica is not passing the learner object back across
 * the R boundary.  There may be some use cases in which it needs to
 * preserve the learner object across returns to R for speed.  If so,
 * this decsion should be revisited later.
 */
learner_bn* NewLearner_rn(SEXP method, SEXP maxIters, SEXP maxTol) {
  const char* meth = CHAR(STRING_ELT(method,0));
  learn_method_bn algorithm;
  if(strcmp(meth,"COUNTING") == 0) {
      algorithm = COUNTING_LEARNING;
    } else if(strcmp(meth,"EM") == 0) {
      algorithm = EM_LEARNING;
    } else if(strcmp(meth,"GRADIENT") == 0) {
      algorithm = GRADIENT_DESCENT_LEARNING;
    } else {
      error("RN_NewLearner: Pos should be 'COUNTING', 'EM' or 'GRADIENT'.");
    }
  learner_bn *result = NewLearner_bn(algorithm, (const char*) NULL,
                                     RN_netica_env);
  if (result == NULL) {
    warning("RN_NewLearner:  error creating learner.");
    return NULL;
  }
  if (!isNull(maxIters)) {
    SetLearnerMaxIters_bn(result,INTEGER(maxIters)[0]);
  }
  if (!isNull(maxTol)) {
    SetLearnerMaxTol_bn(result,REAL(maxTol)[0]);
  }
  return result;
}

/**
 * Once again, the caseset is an object that probably does not need to
 * be passed back across the R boundary.   Case sets appear to be a
 * wrapper for either CaseStreams or Database links.  As RNetica is
 * unlikely to support the database functionality, we can use
 * CaseStreams instead. 
 */
caseset_cs* NewCaseset_rn(SEXP caseStream) {
  caseset_cs* result = NewCaseset_cs((const char*)NULL,RN_netica_env);
  if (result == NULL) {
    warning("RN_NewCaseset:  error creating caseset.");
    return NULL;
  }
  stream_ns *stream = GetCaseStream_Handle(caseStream);
  if (stream == NULL) {
    warning("RN_NewCaseset:  Stream is not open.");
    DeleteCaseset_cs(result);
    return NULL;
  }
  /* Netica allows us to add a degree, but it seems redundant with the
     one in the call to Learner, so won't bother. */
  AddFileToCaseset_cs(result,stream,1.0,(const char*) NULL);
  /* Debugging:  Somehow the the Caseset doesn't work properly if the
  stream is a memory stream instead of a file stream.  The next
  couple of lines write it out. */
  /*  stream_ns *temp = NewFileStream_ns("NeticaTest.cas",RN_netica_env,NULL);
      WriteCaseset_cs(result, temp, NULL);
      DeleteStream_ns(temp); */
  /* Verified that this file is exactly what was expected.  */
  return result;
}

/* Need a return result that discusses convergence.  Waiting for a
   Netica upgrade to cover this. */
SEXP RN_LearnCPTs (SEXP caseStream, SEXP nodes, SEXP method, 
                   SEXP maxIters, SEXP maxTol, SEXP weight) {
  nodelist_bn* nodelist = RN_AS_NODELIST(nodes,NULL);
  learner_bn* learner=NewLearner_rn(method,maxIters,maxTol);
  if (learner==NULL) {
    DeleteNodeList_bn(nodelist);
    error("RN_LearnCPTs:  Error creating learner.");
  }
  caseset_cs* cases = NewCaseset_rn(caseStream);
  if (cases==NULL) {
    DeleteNodeList_bn(nodelist);
    DeleteLearner_bn(learner);
    error("RN_LearnCPTs:  Error creating case set.");
  }
  LearnCPTs_bn(learner, nodelist, cases, REAL(weight)[0]);
  DeleteNodeList_bn(nodelist);
  DeleteLearner_bn(learner);
  DeleteCaseset_cs(cases);
  return R_NilValue;
}
