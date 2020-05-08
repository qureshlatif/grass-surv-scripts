#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _cjshmm_CJSRL4fwdalg(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _cjshmm_CJSRL5fwdalg(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _cjshmm_rcpparma_bothproducts(SEXP);
extern SEXP _cjshmm_rcpparma_hello_world();
extern SEXP _cjshmm_rcpparma_innerproduct(SEXP);
extern SEXP _cjshmm_rcpparma_outerproduct(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_cjshmm_CJSRL4fwdalg",          (DL_FUNC) &_cjshmm_CJSRL4fwdalg,          6},
    {"_cjshmm_CJSRL5fwdalg",          (DL_FUNC) &_cjshmm_CJSRL5fwdalg,          6},
    {"_cjshmm_rcpparma_bothproducts", (DL_FUNC) &_cjshmm_rcpparma_bothproducts, 1},
    {"_cjshmm_rcpparma_hello_world",  (DL_FUNC) &_cjshmm_rcpparma_hello_world,  0},
    {"_cjshmm_rcpparma_innerproduct", (DL_FUNC) &_cjshmm_rcpparma_innerproduct, 1},
    {"_cjshmm_rcpparma_outerproduct", (DL_FUNC) &_cjshmm_rcpparma_outerproduct, 1},
    {NULL, NULL, 0}
};

void R_init_cjshmm(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

