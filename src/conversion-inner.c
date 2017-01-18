
#include "demlife.h"

/* File "conversion-inner.c" contains C versions of functions 
 * from "conversion-inner.R". */

/*## READY_TO_TRANSLATE
## HAS_TESTS
makeLxInner <- function(mx, ax, nx, nAge, nOther, useC = FALSE) {
    ## mx
    stopifnot(is.matrix(mx))
    stopifnot(is.double(mx))
    stopifnot(!any(is.na(mx)))
    stopifnot(all(mx >= 0))
    ## ax
    stopifnot(is.matrix(ax))
    stopifnot(is.double(ax))
    stopifnot(!any(is.na(ax)))
    stopifnot(all(ax >= 0))
    ## nx
    stopifnot(is.double(nx))
    stopifnot(!any(is.na(nx)))
    stopifnot(all(nx >= 0))
    ## nAge
    stopifnot(is.integer(nAge))
    stopifnot(identical(length(nAge), 1L))
    stopifnot(!is.na(nAge))
    stopifnot(nAge >= 2L)
    ## nOther
    stopifnot(is.integer(nOther))
    stopifnot(identical(length(nOther), 1L))
    stopifnot(!is.na(nOther))
    stopifnot(nOther >= 1L)
    ## mx, nAge, nOther
    stopifnot(identical(dim(mx), c(nAge, nOther)))
    ## ax, nAge, nOther
    stopifnot(identical(dim(ax), c(nAge, nOther)))
    if (useC) {
        .Call(makeLxInner_R, mx, ax, nx, nAge, nOther)
    }
    else {
        ans <- double(length = nAge * nOther)
        for (j in seq_len(nOther)) {
            lx.i <- 1
            for (i in seq_len(nAge - 1L)) {
                i.ans <- i + (j - 1L) * nAge
                nx.i <- nx[i]
                mx.i <- mx[i.ans]
                ax.i <- ax[i.ans]
                qx.i <- nx.i * mx.i / (1 + (nx.i - ax.i) * mx.i)
                lx.iplus1 <- lx.i * (1 - qx.i)
                ans[i.ans] <- lx.iplus1 * nx.i + (lx.i - lx.iplus1) * ax.i
                lx.i <- lx.iplus1
            }
            i.ans <- j * nAge
            mx.i <- mx[i.ans]
            ans[i.ans] <- lx.i / mx.i
        }
        ans
    }
}
*/


/* ans g'teed to have length nAge*nOther */
void
makeLxInner(double * ans, double * mx, double * ax, double * nx, int nAge, int nOther)
{
    /* mx matrix of doubles, nAge x nOther*/
    /* ax a matrix of doubles, nAge x nOther*/
    /* nx a vector of doubles */
    
    /*for (j in seq_len(nOther)) {
            lx.i <- 1
            for (i in seq_len(nAge - 1L)) {
                i.ans <- i + (j - 1L) * nAge
                nx.i <- nx[i]
                mx.i <- mx[i.ans]
                ax.i <- ax[i.ans]
                qx.i <- nx.i * mx.i / (1 + (nx.i - ax.i) * mx.i)
                lx.iplus1 <- lx.i * (1 - qx.i)
                ans[i.ans] <- lx.iplus1 * nx.i + (lx.i - lx.iplus1) * ax.i
                lx.i <- lx.iplus1
            }
            i.ans <- j * nAge
            mx.i <- mx[i.ans]
            ans[i.ans] <- lx.i / mx.i
        }*/
    
    for (int j = 0; j < nOther; ++j) {
        
        double this_lx = 0;
        
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


