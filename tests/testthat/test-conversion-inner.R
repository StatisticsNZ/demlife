
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

## mx <- Values(array(c(0.00912160,
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
