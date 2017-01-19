
#include "demlife.h"

/* File "conversion-inner.c" contains C versions of functions 
 * from "conversion-inner.R". */


/* ans g'teed to have length nAge*nOther */
void
makeLxInner(double * ans, double * mx, double * ax, double * nx, int nAge, int nOther)
{
    for (int j = 0; j < nOther; ++j) {
        
        double this_lx = 1;
        
        for (int i = 0; i < nAge-1; ++i) {
            
            int i_ans = i + j * nAge;
            double this_nx = nx[i];
            double this_mx = mx[i_ans];
            double this_ax = ax[i_ans];
            double this_qx = this_nx * this_mx / (1 + (this_nx - this_ax)*this_mx);
            double next_lx = this_lx * (1 - this_qx);
            ans[i_ans] = next_lx * this_nx + (this_lx - next_lx)*this_ax;
            
            this_lx = next_lx;
        }
        /* last row */
        int last_i_ans = (j+1)*nAge - 1;
        double last_mx = mx[last_i_ans];
        ans[last_i_ans] = this_lx / last_mx;
    }
} 


