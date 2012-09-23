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
//Now for node.
#define SetNode_RRef(n,r)	SetNodeUserData_bn(n,0,(void *)r)
/* This function automatically allocations an object for the node if
   it does not exit */
extern SEXP GetNode_RRef(node_bn* node);
#define FastGetNode_RRef(n)		(SEXP) GetNodeUserData_bn(n,0)
#define GetNodeHandle(n)        (node_bn*) R_ExternalPtrAddr(getAttrib(n,nodeatt))
#define NODE_NAME(n)            CHAR(STRING_ELT(AS_CHARACTER(n),0))

/**
 * Converts from name to network.  
 */
extern net_bn* RN_AS_NET(const char* name);

/**
 * Removes the back pointers to a node, so it can be cleanly deleted.
 */
extern void RN_Free_Node(node_bn* node_handle);
extern void RN_Free_Nodes(const nodelist_bn* nodelist);

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
extern SEXP bnatt;
extern SEXP nodeclass;
extern SEXP nodeatt;
extern SEXP nodediscatt;
extern SEXP TRUEV;  //Length 1 logical vector containing TRUE
extern SEXP FALSEV;
extern SEXP NAV;
extern const char* EmptyString;

extern void RN_Define_Symbols();  //Reloads symbol definitions.
extern void RN_Free_Symbols();  //Seems we can only allocate them on 
                                //a call basis.

