
#ifndef _DEMLIFE_H_
#define _DEMLIFE_H_

#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>



/* everything in "x_sym" form here must be macro defined in init.c 
 * 
 * eg
 * SEXP indices_sym;
 */ 



/* C function declarations */
void makeLxInner(double * ans, double * mx, double * ax, 
                    double * nx, int nAge, int nOther);

#endif
