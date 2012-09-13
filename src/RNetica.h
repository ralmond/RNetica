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
 * Common Symbols so we don't need to keep redefining them.
 * Defined in Registration.c
 */
extern SEXP bnclass;
extern SEXP delbnclass;
extern SEXP bnatt;

