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
char* NeticaClass = "NeticaBN";
char* DeletedClass = "DeletedNeticaBN";
//char* NetPointer = "Netica_net_bn";

SEXP bnclass=NULL;
//SEXP delbnclass=NULL;
SEXP bnatt=NULL;

static int symbolRegCount=0;

void RN_Define_Symbols() {
  //printf("RN_Defining_Symbols: %d.\n",symbolRegCount);
  if (bnclass==NULL) {
    R_PreserveObject(bnclass = allocVector(STRSXP,1));
    SET_STRING_ELT(bnclass,0,mkChar(NeticaClass));
  }
  //  if (delbnclass==NULL) {
  //    R_PreserveObject(delbnclass = allocVector(STRSXP,1));
  //    SET_STRING_ELT(delbnclass,0,mkChar(DeletedClass));
  //  }
  if (bnatt==NULL) { 
    R_PreserveObject(bnatt = install("Netica_bn"));  
  } 

  symbolRegCount++;
}

void RN_Free_Symbols() {
  //printf("RN_Free_Symbols: %d.\n",symbolRegCount);
  if (--symbolRegCount == 0) {
    //UNPROTECT(3);
    if (bnclass != NULL) {
      R_ReleaseObject(bnclass);
      bnclass = NULL;
    }
    //    if (delbnclass != NULL) {
    //      R_ReleaseObject(delbnclass);
    //      delbnclass = NULL;
    //    }
    if (bnatt != NULL) { 
      R_ReleaseObject(bnatt); 
      bnatt = NULL; 
    } 
  }
}


/////////////////////////////////////////////////////////////////////
// .Call Methods
////////////////////////////////////////////////////////////////
// File = Networks.c
extern SEXP RN_Netica_Version();
extern SEXP RN_New_Net(SEXP namelist);
extern SEXP RN_Delete_Net(SEXP netlist);
extern SEXP RN_Named_Nets(SEXP namelist);
extern SEXP RN_GetNth_Nets(SEXP nlist);
extern SEXP RN_Copy_Nets(SEXP nets, SEXP namelist, SEXP options);

R_CallMethodDef callMethods[] = {
  {"RN_Netica_Version", (DL_FUNC) &RN_Netica_Version, 0},
  {"RN_isBNActive", (DL_FUNC) &RN_Netica_Version, 1},
  {"RN_New_Net", (DL_FUNC) &RN_New_Net, 1},
  {"RN_Delete_Net", (DL_FUNC) &RN_Delete_Net, 1},
  {"RN_Named_Nets", (DL_FUNC) &RN_Named_Nets, 1},
  {"RN_GetNth_Nets", (DL_FUNC) &RN_GetNth_Nets, 1},
  {"RN_Copy_Nets", (DL_FUNC) &RN_Copy_Nets, 3},
  {"RN_Read_Nets", (DL_FUNC) &RN_Copy_Nets, 1},
  {"RN_WriteNets", (DL_FUNC) &RN_Copy_Nets, 2},
  {"RN_GetNetFilenames", (DL_FUNC) &RN_Copy_Nets, 1},
  {"RN_GetNetNames", (DL_FUNC) &RN_Copy_Nets, 1},
  {"RN_GetNetTitles", (DL_FUNC) &RN_Copy_Nets, 1},
  {"RN_GetNetComments", (DL_FUNC) &RN_Copy_Nets, 1},
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