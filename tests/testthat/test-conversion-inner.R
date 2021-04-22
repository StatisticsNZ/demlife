
context("conversion-inner")
n.test <- 5
test.identity <- FALSE

## Conversion between life table functions.  Assume that input data OK.
## Just check that calculations are correct.


test_that("mx2qx works", {
    mx2qx <- demlife:::mx2qx
    ## open = TRUE
    mx <- array(c(0.00912160,
                  0.00048752,
                  0.00020030,
                  0.00020454,
                  0.00042186,
                  0.00048792,
                  0.00049932,
                  0.00059055,
                  0.00090236,
                  0.00152871,
                  0.00253801,
                  0.00401730,
                  0.00625507,
                  0.00981909,
                  0.01582430,
                  0.02616533,
                  0.04514432,
                  0.07986271,
                  0.17421076),
                dim = c(19, 1))
    nx <- c(1, 4, rep(5, 16), Inf)
    ax <- c(0.5, 2, rep(2.5, 16), Inf)
    ans.obtained <- mx2qx(mx, nx = nx, ax = ax, open = TRUE)
    ans.expected <- array(c(0.00908, 0.00195, 0.00100, 0.00102,
                            0.00211, 0.00244, 0.00249, 0.00295,
                            0.00450, 0.00761, 0.01261, 0.01989,
                            0.03079, 0.04792, 0.07611, 0.12279,
                            0.20283, 0.33286, 1.00000),
                          dim = c(19, 1))
    expect_equal(ans.obtained, ans.expected, tol = 0.001)
    ## open = FALSE
    mx <- array(c(0.00912160,
                  0.00048752,
                  0.00020030,
                  0.00020454,
                  0.00042186,
                  0.00048792,
                  0.00049932,
                  0.00059055,
                  0.00090236,
                  0.00152871,
                  0.00253801,
                  0.00401730,
                  0.00625507,
                  0.00981909,
                  0.01582430,
                  0.02616533,
                  0.04514432,
                  0.07986271),
                dim = c(18, 1))
    nx <- c(1, 4, rep(5, 16))
    ax <- c(0.5, 2, rep(2.5, 16))
    ans.obtained <- mx2qx(mx, nx = nx, ax = ax, open = FALSE)
    ans.expected <- array(c(0.00908, 0.00195, 0.00100, 0.00102,
                            0.00211, 0.00244, 0.00249, 0.00295,
                            0.00450, 0.00761, 0.01261, 0.01989,
                            0.03079, 0.04792, 0.07611, 0.12279,
                            0.20283, 0.33286),
                          dim = c(18, 1))
    expect_equal(ans.obtained, ans.expected, tol = 0.001)
})

test_that("qx2mx works", {
    qx2mx <- demlife:::qx2mx
    mx2qx <- demlife:::mx2qx
    ## open = TRUE
    mx <- array(c(0.00912160,
                  0.00048752,
                  0.00020030,
                  0.00020454,
                  0.00042186,
                  0.00048792,
                  0.00049932,
                  0.00059055,
                  0.00090236,
                  0.00152871,
                  0.00253801,
                  0.00401730,
                  0.00625507,
                  0.00981909,
                  0.01582430,
                  0.02616533,
                  0.04514432,
                  0.07986271,
                  0.17421076),
                dim = c(19, 1))
    nx <- c(1, 4, rep(5, 16), Inf)
    ax <- c(0.5, 2, rep(2.5, 16), Inf)
    ans.obtained <- qx2mx(mx2qx(mx, nx = nx, ax = ax, open = TRUE),
                          nx = nx, ax = ax, open = TRUE, mA = mx[19])
    expect_equal(ans.obtained, mx)
    ## open = FALSE
    mx <- array(c(0.00912160,
                  0.00048752,
                  0.00020030,
                  0.00020454,
                  0.00042186,
                  0.00048792,
                  0.00049932,
                  0.00059055,
                  0.00090236,
                  0.00152871,
                  0.00253801,
                  0.00401730,
                  0.00625507,
                  0.00981909,
                  0.01582430,
                  0.02616533,
                  0.04514432,
                  0.07986271),
                dim = c(18, 1))
    nx <- c(1, 4, rep(5, 16))
    ax <- c(0.5, 2, rep(2.5, 16))
    ans.obtained <- qx2mx(mx2qx(mx, nx = nx, ax = ax, open = FALSE),
                          nx = nx, ax = ax, open = FALSE, mA = NULL)
    expect_equal(ans.obtained, mx)
    expect_error(qx2mx(mx2qx(mx, nx = nx, ax = ax, open = FALSE),
                       nx = nx, ax = ax, open = TRUE, mA = NULL),
                 "insufficient information to calculate values for open age group")
})    

test_that("qx2px works", {
    qx2px <- demlife:::qx2px
    qx <- array(runif(20), dim = c(5, 4))
    ans.obtained <- qx2px(qx)
    ans.expected <- 1 - qx
    expect_identical(ans.obtained, ans.expected)
})

test_that("px2qx works", {
    px2qx <- demlife:::px2qx
    px <- array(runif(20), dim = c(5, 4))
    ans.obtained <- px2qx(px)
    ans.expected <- 1 - px
    expect_identical(ans.obtained, ans.expected)
})




## test_that("mx2px works", {
##     mx2px <- demlife:::mx2px
##     ## open = TRUE
##     mx <- array(c(0.00912160,
##                   0.00048752,
##                   0.00020030,
##                   0.00020454,
##                   0.00042186,
##                   0.00048792,
##                   0.00049932,
##                   0.00059055,
##                   0.00090236,
##                   0.00152871,
##                   0.00253801,
##                   0.00401730,
##                   0.00625507,
##                   0.00981909,
##                   0.01582430,
##                   0.02616533,
##                   0.04514432,
##                   0.07986271,
##                   0.17421076),
##                 dim = c(19, 1))
##     nx <- c(1, 4, rep(5, 16), Inf)
##     ax <- c(0.5, 2, rep(2.5, 16), Inf)
##     ans.obtained <- mx2px(mx, nx = nx, ax = ax, open = TRUE)
##     ans.expected <- array(c(0.00908, 0.00195, 0.00100, 0.00102,
##                             0.00211, 0.00244, 0.00249, 0.00295,
##                             0.00450, 0.00761, 0.01261, 0.01989,
##                             0.03079, 0.04792, 0.07611, 0.12279,
##                             0.20283, 0.33286, 1.00000),
##                           dim = c(19, 1))
##     expect_equal(ans.obtained, ans.expected, tol = 0.001)
##     ## open = FALSE
##     mx <- array(c(0.00912160,
##                   0.00048752,
##                   0.00020030,
##                   0.00020454,
##                   0.00042186,
##                   0.00048792,
##                   0.00049932,
##                   0.00059055,
##                   0.00090236,
##                   0.00152871,
##                   0.00253801,
##                   0.00401730,
##                   0.00625507,
##                   0.00981909,
##                   0.01582430,
##                   0.02616533,
##                   0.04514432,
##                   0.07986271),
##                 dim = c(18, 1))
##     nx <- c(1, 4, rep(5, 16))
##     ax <- c(0.5, 2, rep(2.5, 16))
##     ans.obtained <- mx2qx(mx, nx = nx, ax = ax, open = FALSE)
##     ans.expected <- array(c(0.00908, 0.00195, 0.00100, 0.00102,
##                             0.00211, 0.00244, 0.00249, 0.00295,
##                             0.00450, 0.00761, 0.01261, 0.01989,
##                             0.03079, 0.04792, 0.07611, 0.12279,
##                             0.20283, 0.33286),
##                           dim = c(18, 1))
##     expect_equal(ans.obtained, ans.expected, tol = 0.001)
## })

## mx <- dembase::Values(array(c(0.00912160,
##                      0.00048752,
##                      0.00020030,
##                      0.00020454,
##                      0.00042186,
##                      0.00048792,
##                      0.00049932,
##                      0.00059055,
##                      0.00090236,
##                      0.00152871,
##                      0.00253801,
##                      0.00401730,
##                      0.00625507,
##                      0.00981909,
##                      0.01582430,
##                      0.02616533,
##                      0.04514432,
##                      0.07986271,
##                      0.17421076),
##                    dim = c(19, 1),
##                    dimnames = list(age = c(0, "1-4", paste(seq(5,80,5),
##                                        seq(9,84,5), sep="-"), "85+"),
##                        sex = "Female")))

test_that("makeLx works", {
    ## data from Preston et al, 2000, Demography, Box 3.1
    makeLx <- demlife:::makeLx
    mx <- c(0.008743,
            0.000370,
            0.000153,
            0.000193,
            0.000976,
            0.001285,
            0.001135,
            0.001360,
            0.001882,
            0.002935,
            0.004849,
            0.007133,
            0.011263,
            0.018600,
            0.028382,
            0.041238,
            0.071634,
            0.112324,
            0.190585)
    ax <- c(0.068,
            1.626,
            2.500,
            3.143,
            2.724,
            2.520,
            2.481,
            2.601,
            2.701,
            2.663,
            2.698,
            2.676,
            2.645,
            2.624,
            2.619,
            2.593,
            2.518,
            2.423,
            5.247)
    Lx <- c(99192,
            396183,
            494741,
            494375,
            492980,
            490106,
            487127,
            484175,
            480384,
            474686,
            465777,
            452188,
            432096,
            401480,
            357713,
            301224,
            228404,
            145182,
            110889)
    nx <- c(1, 4, rep(5, 16), Inf)
    nAge <- 19L
    age.levels <- c("0", "1-4", paste(seq(5, 80, 5), seq(9, 84, 5), sep = "-"), "85+")
    ## vector
    mx1 <- array(mx,
                 dim = 19,
                 dimnames = list(age = age.levels))
    ax1 <- array(ax, 
                 dim = 19,
                 dimnames = list(age = age.levels))
    Lx1 <- array(Lx / 100000,
                 dim = 19,
                 dimnames = list(age = age.levels))
    mx1 <- dembase::Values(mx1)
    ax1 <- dembase::Values(ax1)
    Lx1 <- dembase::Counts(Lx1)
    ans.obtained <- makeLx(mx = mx1,
                           ax = ax1)
    ans.expected <- Lx1
    expect_equal(ans.obtained, ans.expected, tol = 0.001)
    ## matrix
    mx2 <- array(mx,
                 dim = c(19, 2),
                 dimnames = list(age = age.levels,
                                 sex = c("f", "m")))
    ax2 <- array(ax, 
                 dim = c(19, 2),
                 dimnames = list(age = age.levels,
                                 sex = c("f", "m")))
    Lx2 <- array(Lx / 100000,
                 dim = c(19, 2),
                 dimnames = list(age = age.levels,
                                 sex = c("f", "m")))
    mx2 <- dembase::Values(mx2)
    ax2 <- dembase::Values(ax2)
    Lx2 <- dembase::Counts(Lx2)
    ans.obtained <- makeLx(mx = mx2,
                           ax = ax2)
    ans.expected <- Lx2
    expect_equal(ans.obtained, ans.expected, tol = 0.001)
})

test_that("R version of makeLxInner works", {
    makeLxInner <- demlife:::makeLxInner
    ## data from Preston et al, 2000, Demography, Box 3.1
    mx <- c(0.008743,
            0.000370,
            0.000153,
            0.000193,
            0.000976,
            0.001285,
            0.001135,
            0.001360,
            0.001882,
            0.002935,
            0.004849,
            0.007133,
            0.011263,
            0.018600,
            0.028382,
            0.041238,
            0.071634,
            0.112324,
            0.190585)
    ax <- c(0.068,
            1.626,
            2.500,
            3.143,
            2.724,
            2.520,
            2.481,
            2.601,
            2.701,
            2.663,
            2.698,
            2.676,
            2.645,
            2.624,
            2.619,
            2.593,
            2.518,
            2.423,
            5.247)
    Lx <- c(99192,
            396183,
            494741,
            494375,
            492980,
            490106,
            487127,
            484175,
            480384,
            474686,
            465777,
            452188,
            432096,
            401480,
            357713,
            301224,
            228404,
            145182,
            110889)
    nx <- c(1, 4, rep(5, 16), Inf)
    nAge <- 19L
    ## 19x1 matrix
    mx1 <- matrix(mx, nrow = 19)
    ax1 <- matrix(ax, nrow = 19)
    Lx1 <- Lx / 100000
    nOther1 <- 1L
    ans.obtained <- makeLxInner(mx = mx1,
                                ax = ax1,
                                nx = nx,
                                nAge = nAge,
                                nOther = nOther1)
    ans.expected <- Lx1
    expect_equal(ans.obtained, ans.expected, tol = 0.001)
    ## 19x2 matrix
    mx2 <- matrix(mx, nrow = 19, ncol = 2)
    ax2 <- matrix(ax, nrow = 19, ncol = 2)
    Lx2 <- rep(Lx / 100000, 2)
    nOther2 <- 2L
    ans.obtained <- makeLxInner(mx = mx2,
                                ax = ax2,
                                nx = nx,
                                nAge = nAge,
                                nOther = nOther2)
    ans.expected <- Lx2
    expect_equal(ans.obtained, ans.expected, tol = 0.001)
})

test_that("R and C versions of makeLxInner give same answer", {
    makeLxInner <- demlife:::makeLxInner
    ## data from Preston et al, 2000, Demography, Box 3.1
    mx <- c(0.008743,
            0.000370,
            0.000153,
            0.000193,
            0.000976,
            0.001285,
            0.001135,
            0.001360,
            0.001882,
            0.002935,
            0.004849,
            0.007133,
            0.011263,
            0.018600,
            0.028382,
            0.041238,
            0.071634,
            0.112324,
            0.190585)
    ax <- c(0.068,
            1.626,
            2.500,
            3.143,
            2.724,
            2.520,
            2.481,
            2.601,
            2.701,
            2.663,
            2.698,
            2.676,
            2.645,
            2.624,
            2.619,
            2.593,
            2.518,
            2.423,
            5.247)
    Lx <- c(99192,
            396183,
            494741,
            494375,
            492980,
            490106,
            487127,
            484175,
            480384,
            474686,
            465777,
            452188,
            432096,
            401480,
            357713,
            301224,
            228404,
            145182,
            110889)
    nx <- c(1, 4, rep(5, 16), Inf)
    nAge <- 19L
    ## 19x1 matrix
    mx1 <- matrix(mx, nrow = 19)
    ax1 <- matrix(ax, nrow = 19)
    Lx1 <- Lx / 100000
    nOther1 <- 1L
    ans.R <- makeLxInner(mx = mx1,
                                ax = ax1,
                                nx = nx,
                                nAge = nAge,
                         nOther = nOther1,
                         useC = FALSE)
    ans.C <- makeLxInner(mx = mx1,
                                ax = ax1,
                                nx = nx,
                                nAge = nAge,
                         nOther = nOther1,
                         useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
    ## 19x2 matrix
    mx2 <- matrix(mx, nrow = 19, ncol = 2)
    ax2 <- matrix(ax, nrow = 19, ncol = 2)
    Lx2 <- rep(Lx / 100000, 2)
    nOther2 <- 2L
    ans.obtained <- makeLxInner(mx = mx2,
                                ax = ax2,
                                nx = nx,
                                nAge = nAge,
                                nOther = nOther2)
    ans.expected <- Lx2
    expect_equal(ans.obtained, ans.expected, tol = 0.001)
})
                            
