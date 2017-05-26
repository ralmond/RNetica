/**
 * Header Files for R-Netica User Interface.
 */
#include <Netica.h>
/**
 * This is a global pointer to the Netica environment.
 * It is created once during a session.
 */
extern environ_ns* RN_netica_env;

/**
 * We use the NetUserData field to set up a back pointer to an
 * R object which represents the net.  These macros facilitate the process.
 */
#define SetNet_RRef(n,r)	SetNetUserData_bn(n,0,(void *)r)
#define GetNet_RRef(n)		(SEXP) GetNetUserData_bn(n,0)
#define GetNeticaHandle(b)      (net_bn*) R_ExternalPtrAddr(getAttrib(b,bnatt))
#define BN_NAME(b)              CHAR(STRING_ELT(AS_CHARACTER(b),0))
extern int isNeticaBN(SEXP obj);
//Now for node.
#define SetNode_RRef(n,r)	SetNodeUserData_bn(n,0,(void *)r)
/* This function automatically allocations an object for the node if
   it does not exit */
extern SEXP GetNode_RRef(node_bn* node);
#define FastGetNode_RRef(n)		(SEXP) GetNodeUserData_bn(n,0)
#define GetNodeHandle(n)        (node_bn*) R_ExternalPtrAddr(getAttrib(n,nodeatt))
#define NODE_NAME(n)            CHAR(STRING_ELT(AS_CHARACTER(n),0))
extern int isNeticaNode(SEXP obj);
#define GetCaseStream_Handle(cs) (stream_ns*) R_ExternalPtrAddr(getAttrib(cs,casestreamatt))
extern int isNeticaStream(SEXP obj);
#define GetRNG_Handle(rng) (randgen_ns*) R_ExternalPtrAddr(getAttrib(rng,rngatt))
extern int isNeticaRng(SEXP obj);

/**
 * Converts from name to network.  
 */
extern net_bn* RN_AS_NET(const char* name);

/**
 * Removes the back pointers to a node, so it can be cleanly deleted.
 */
extern void RN_Free_Node(node_bn* node_handle);
extern void RN_Free_Nodes(const nodelist_bn* nodelist);
extern void RN_Free_Nodelist(const SEXP nodes);

/**
 * Converts between Netica nodelists and R lists.  Calling programs
 * are responsible for freeing/unprotecting the results.
 */
extern SEXP RN_AS_RLIST(const nodelist_bn* nodelist);
extern nodelist_bn* RN_AS_NODELIST(SEXP nodes, net_bn* net);


/**
 * Common Symbols so we don't need to keep redefining them.
 * Defined in Registration.c
 */
extern SEXP bnclass;
const char* NeticaClass;
extern SEXP bnatt;
extern SEXP nodeclass;
const char* NodeClass;
extern SEXP nodeatt;
extern SEXP nodediscatt;
extern SEXP cliquenodeclass;
extern SEXP cliqueatt;
extern SEXP sdatt;
extern SEXP TRUEV;  //Length 1 logical vector containing TRUE
extern SEXP FALSEV;
extern SEXP NAV;
extern SEXP XYnames;
extern const char* EmptyString;
extern SEXP casestreamclass;
extern const char* CaseStreamClass;
extern SEXP casefilestreamclass;
extern const char* CaseFileStreamClass;
extern SEXP memorystreamclass;
extern const char* MemoryStreamClass;
extern SEXP casestreamatt;
extern SEXP casestreamposatt;
extern SEXP casestreampathatt;
extern SEXP casestreamlastidatt;
extern SEXP casestreamlastfreqatt;
extern SEXP casestreamdfatt;
extern SEXP casestreamdfnameatt;
extern SEXP CaseStreamList;
extern SEXP rngclass;
extern const char* RNGClass;
extern SEXP rngatt;
extern SEXP RngList;

extern void RN_Define_Symbols();  //Reloads symbol definitions.
extern void RN_Free_Symbols();  //Seems we can only allocate them on 
                                //a call basis.

extern SEXP RN_KindToChar (nodekind_bn kind);
extern nodekind_bn RN_CharToKind (SEXP csxp);

//Takes care of conversion of infinities
extern level_bn RN_RnumToNnum (double x);
extern double RN_NnumToRnum (level_bn x);

//Takes care of conversion between state indexes and prob vectors and
//R SEXPs.
extern state_bn *RN_AS_STATE_BN(SEXP states);
extern prob_bn *RN_AS_PROB_BN(SEXP vals);
extern SEXP RN_AS_PROBSXP(const prob_bn *vals, int nn);

//Closes the case streams
extern void CloseOpenCaseStreams();
//Frees unneeded RNGs
extern void FreeRNGs();

