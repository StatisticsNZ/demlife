
context("AllClasses")

test_that("can create object of class LifeTable", {
    ## one dimension
    mx <- ValuesOne(c(0.5, 0.3, 0.1),
                    labels = c("0-4", "5-9", "10+"),
                    name = "age")
    ax <- ValuesOne(c(1, 2.5, 2.5),
                    labels = c("0-4", "5-9", "10+"),
                    name = "age")
    x <- new("LifeTable",
             mx = mx,
             ax = ax,
             showFun = c("mx", "lx", "ex"),
             radix = 100000,
             showQuantiles = TRUE,
             prob = c(0.025, 0.5, 0.975))
    expect_true(validObject(x))
    ## three dimensions
    mx <- Values(array(0.1,
                       dim = c(4, 2, 3),
                       dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                       sex = c("f", "m"),
                                       time = c("2001-2005", "2006-2010", "2011-2015"))))
    ax <- Values(array(1,
                       dim = c(4, 2, 3),
                       dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                       sex = c("f", "m"),
                                       time = c("2001-2005", "2006-2010", "2011-2015"))))
    x <- new("LifeTable",
             mx = mx,
             ax = ax,
             showFun = c("mx", "dx", "lx", "Lx"),
             radix = 1,
             showQuantiles = TRUE,
             prob = c(0.025, 0.5, 0.975))
    expect_true(validObject(x))
})

test_that("validity tests for LifeTable work", {
    mx <- Values(array(0.1,
                       dim = c(4, 2, 3),
                       dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                       sex = c("f", "m"),
                                       time = c("2001-2005", "2006-2010", "2011-2015"))))
    ax <- Values(array(1,
                       dim = c(4, 2, 3),
                       dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                       sex = c("f", "m"),
                                       time = c("2001-2005", "2006-2010", "2011-2015"))))
    x <- new("LifeTable",
             mx = mx,
             ax = ax,
             showFun = c("mx", "dx", "lx", "Lx"),
             radix = 1,
             showQuantiles = TRUE,
             prob = c(0.025, 0.5, 0.975))
    ## 'mx' and 'ax' have identical metadata
    x.wrong <- x
    x.wrong@ax <- Values(array(1,
                               dim = c(4, 2, 3),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15-19"),
                                               sex = c("f", "m"),
                                               time = c("2001-2005", "2006-2010", "2011-2015"))))
    expect_error(validObject(x.wrong),
                 "'mx' and 'ax' have different metadata")
    ## first dimension of 'mx' has dimtype "age"
    x.wrong <- x
    x.wrong@mx <- aperm(x.wrong@mx, perm = 3:1)
    x.wrong@ax <- aperm(x.wrong@ax, perm = 3:1)
    expect_error(validObject(x.wrong),
                 "first dimension of 'mx' does not have dimtype \"age\"")    
    ## if 'mx' has time dimension, dimscale must be "Intervals"
    mx <- Values(array(0.1,
                       dim = c(4, 2, 3),
                       dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                       sex = c("f", "m"),
                                       time = c("2000", "2005", "2010"))))
    ax <- Values(array(1,
                       dim = c(4, 2, 3),
                       dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                       sex = c("f", "m"),
                                       time = c("2000", "2005", "2010"))))
    expect_error(new("LifeTable",
                     mx = mx,
                     ax = ax,
                     showFun = c("mx", "dx", "lx", "Lx"),
                     radix = 1),
                 "dimension with dimtype \"time\" has dimscale \"Points\"")
    ## 'mx' does not have dimension with dimtype "quantile"
    mx <- Values(array(0.1,
                       dim = c(4, 2, 3),
                       dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                       sex = c("f", "m"),
                                       quantile = c(0.025, 0.5, 0.975))))
    ax <- Values(array(1,
                       dim = c(4, 2, 3),
                       dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                       sex = c("f", "m"),
                                       quantile = c(0.025, 0.5, 0.975))))
    expect_error(new("LifeTable",
                     mx = mx,
                     ax = ax,
                     showFun = c("mx", "dx", "lx", "Lx"),
                     radix = 1),
                 "dimension with dimtype \"quantile\"")
    ## 'mx' has no zero-length dimensions
    mx <- Values(array(0.1,
                       dim = c(4, 0, 3),
                       dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                       sex = character(),
                                       time = c("2001-2005", "2006-2010", "2011-2015"))))
    ax <- Values(array(1,
                       dim = c(4, 0, 3),
                       dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                       sex = character(),
                                       time = c("2001-2005", "2006-2010", "2011-2015"))))
    expect_error(new("LifeTable",
                     mx = mx,
                     ax = ax,
                     showFun = c("mx", "dx", "lx", "Lx"),
                     radix = 1),
                 "'mx' has dimension with length 0")
    ## 'mx', 'ax' do not have missing values
    x.wrong <- x
    x.wrong@mx[1] <- NA
    expect_error(validObject(x.wrong),
                 "'mx' has missing values")
    ## 'mx', 'ax' do not have negative values
    x.wrong <- x
    x.wrong@ax[1] <- -0.000000001
    expect_error(validObject(x.wrong),
                 "'ax' has negative values")
    ## all values of 'ax' are within range implied by metadata for age
    x.wrong <- x
    x.wrong@ax[1] <- 100
    expect_error(validObject(x.wrong),
                 "some values of 'ax' are larger than the width of the age group")
})

