/**
 * Header Files for R-Netica User Interface.
 */
#include <Netica.h>
/**
 * This is a global pointer to the Netica environment.
 * It is created once during a session.
 */
//extern environ_ns* RN_netica_env;
/* Now replaced with the session objects. */

/**
 * Need to extend standard R to be able to access slots in
 * reference classes.
 */

extern SEXP RX_do_RC_field(SEXP obj, SEXP name);
extern SEXP RX_do_RC_field_assign(SEXP obj, SEXP name, SEXP value);
extern int RX_has_RC_field(SEXP obj, SEXP name);
#define GET_FIELD(x, what)       RX_do_RC_field(x, what)
#define SET_FIELD(x, what, value)  RX_do_RC_field_assign(x, what, value)
#define HAS_FIELD(x, what)       RX_has_RC_field(x, what)



/**
 * We use the NetUserData field to set up a back pointer to an
 * R object which represents the net.  These macros facilitate the process.
 */
extern environ_ns* GetSessionPtr(SEXP sessobj);
extern net_bn* GetNetworkPtr(SEXP netobj);
/* #define SetNet_RRef(n,r)	SetNetUserData_bn(n,0,(void *)r) */
/* #define GetNet_RRef(n)		(SEXP) GetNetUserData_bn(n,0) */
#define GetNeticaHandle(b)      GetNetworkPtr(b)
#define BN_NAME(b)              CHAR(STRING_ELT(GET_FIELD(b,namefield),0))
extern Rboolean isNeticaBN(SEXP obj);
//Now for node.
/* #define SetNode_RRef(n,r)	SetNodeUserData_bn(n,0,(void *)r) */
/* This function automatically allocations an object for the node if
   it does not exit */
extern SEXP GetNode_RRef(node_bn* node, SEXP netobj);
/* #define FastGetNode_RRef(n)		(SEXP) GetNodeUserData_bn(n,0) */
extern node_bn* GetNodePtr(SEXP nodeobj);
#define GetNodeHandle(n)        GetNodePtr(n)
#define NODE_NAME(n)            CHAR(STRING_ELT(GET_FIELD(n,namefield),0))
#define NODE_NET(n)             GET_FIELD(n,netfield)
extern int isNeticaNode(SEXP obj);
#define GetCaseStream_Handle(cs) (stream_ns*) R_ExternalPtrAddr(GET_FIELD(cs,casestreamatt))
extern Rboolean isNeticaStream(SEXP obj);
#define GetRNG_Handle(rng) (randgen_ns*) R_ExternalPtrAddr(GET_FIELD(rng,rngatt))
extern Rboolean isNeticaRng(SEXP obj);


extern Rboolean RX_isUnbound(SEXP x);
#define isUnbound(x)    RX_isUnbound(x)





/**
 * Converts from name to network.  
 */
//extern net_bn* RN_AS_NET(const char* name);

/**
 * Removes the back pointers to a node, so it can be cleanly deleted.
 * FIXME:: Not sure this needs to be done here, probably better to do
 * this on R side.
 */
//extern void RN_Free_Node(node_bn* node_handle, SEXP bn);
extern void RN_Free_Nodes(const nodelist_bn* nodelist, SEXP bn);
//extern void RN_Free_Nodelist(const SEXP nodes, SEXP bn);

/**
 * Converts between Netica nodelists and R lists.  Calling programs
 * are responsible for freeing/unprotecting the results.
 */
extern SEXP RN_AS_RLIST(const nodelist_bn* nodelist, SEXP bn);
extern nodelist_bn* RN_AS_NODELIST(SEXP nodes, net_bn* net);


/**
 * Common Symbols so we don't need to keep redefining them.
 * Defined in Registration.c
 */
extern SEXP RNeticaPackageName;
extern const char* NeticaClass;
extern SEXP sessionatt;
extern SEXP namefield;
extern SEXP netsfield;
extern SEXP sessionclass;
extern SEXP sessionconstructor;
extern SEXP bnclass;
extern SEXP bnconstructor;
extern SEXP pathfield;
extern SEXP sessionfield;
extern SEXP nodesfield;
extern const char* NetworkClass;
extern SEXP bnatt;
extern SEXP nodeclass;
extern SEXP nodeconstructor;
extern const char* NodeClass;
extern SEXP nodeatt;
extern SEXP netfield;
extern SEXP nodediscatt;
extern SEXP cliquenodeclass;
extern SEXP cliquenodeconstructor;
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

// Utilities for installing networks in the session environment
extern SEXP RN_FindNetworkStr(SEXP sessobj, const char* netname);
extern void RN_RegisterNetwork(SEXP sessobj, const char* netname, SEXP netobj);
extern void RN_UnregisterNetwork(SEXP sessobj, const char* netname);

extern SEXP RN_FindNodeStr(SEXP netobj, const char* nodename);
extern void RN_RegisterNode(SEXP netobj, const char* nodename, SEXP nodeobj);
extern void RN_UnregisterNode(SEXP netobj, const char* nodename);


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

