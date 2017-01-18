
#include "demlife.h"
#include <R_ext/Rdynload.h>



/* wrap makeLxInner */
SEXP makeLxInner_R(SEXP mx_R, SEXP ax_R, SEXP nx_R, SEXP nAge_R, SEXP nOther_R)
{
    /* mx matrix of doubles, nAge x nOther*/

    /* ax a matrix of doubles, nAge x nOther*/

    /* nx a vector of doubles */

    /* nAge an integer */

    /* nOther an integer */
    int nAge = *INTEGER(nAge_R);
    int nOther = *INTEGER(nOther_R);
    double *mx = REAL(mx_R);
    double *ax = REAL(ax_R);
    double *nx = REAL(nx_R);

    SEXP ans_R;
    PROTECT(ans_R = allocVector(REALSXP, nAge * nOther));
    double *ans = REAL(ans_R);
    makeLxInner(ans, mx, ax, nx, nAge, nOther);
    UNPROTECT(1);
    return ans_R;
}

/* ******************************************************************************* */
/* Create table describing R-visible versions of C functions ********************* */
/* ******************************************************************************* */

#define CALLDEF(name, n) {#name, (DL_FUNC) &name, n}

static const
R_CallMethodDef callMethods[] = {
  CALLDEF(makeLxInner_R, 5),
  
  {NULL}
};

#define ADD_SYM(name) name##_sym = install(#name)


/* Macro to make routines are callable from other packages' C code: */

#define RREGDEF(name)  R_RegisterCCallable("demlife", #name, (DL_FUNC) name)


/* ******************************************************************************* */
/* Register R-visible versions of C functions, plus symbols ********************** */
/* ******************************************************************************* */

void
R_init_demlife(DllInfo *info)
{
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);

  /* register functions as callable from other packages using RREGDEF
   * make sure each of these is in the header file in inst/include. 
   *  
   * eg 
   * RREGDEF(collapse_R);
   *  */
  
  /* install symbols 
   * 
   * eg
   * ADD_SYM(indices);
   *  */
}
