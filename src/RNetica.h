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

/**
 * Converts from name to network.  This is the safe way to access
 * networks.
 */
extern net_bn* RN_AS_NET(const char* name);

/**
 * Common Symbols so we don't need to keep redefining them.
 * Defined in Registration.c
 */
extern SEXP bnclass;
//extern SEXP delbnclass;
extern SEXP bnatt;
extern char* EmptyString;

extern void RN_Define_Symbols();  //Reloads symbol definitions.
extern void RN_Free_Symbols();  //Seems we can only allocate them on 
                                //a call basis.

