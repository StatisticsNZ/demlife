
context("helper-functions")
n.test <- 5
test.identity <- FALSE

test_that("calculateLifeTableFuns works - ltFunSecond is TRUE", {
    calculateLifeTableFuns <- demlife:::calculateLifeTableFuns
    convertLifeTableFun <- demlife:::convertLifeTableFun
    ## 2 dimensions
    al <- demdata::afghan.life
    al <- Values(al)
    mx <- subarray(al,
                   subarray = (fun == "mx") & (time == "2001-2005"))
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005"))
    ans.obtained <- calculateLifeTableFuns(mx = mx,
                                           ax = ax,
                                           radix = 100000,
                                           funs = c("mx", "ax", "lx"),
                                           ltFunSecond = TRUE)
    nx <- matrix(c(1, 4, rep(5, 16), Inf), nr = 19, nc  = 2)
    ans.expected <- c(mx@.Data,
                      ax@.Data,
                      100000*convertLifeTableFun(.Data = mx@.Data,
                                                 from = "mx",
                                                 to = "lx",
                                                 ax = ax@.Data,
                                                 nx = nx,
                                                 open = TRUE,
                                                 mA = NULL))
    ans.expected <- array(ans.expected,
                          dim = c(dim(mx), 3))
    dimnames(ans.expected) <- c(dimnames(mx),
                                list(lt.fun = c("mx", "ax", "lx")))
    ans.expected <- Values(ans.expected)
    ans.expected <- aperm(ans.expected, perm = c(1, 3, 2))
    expect_identical(ans.obtained, ans.expected)
    ## 1 dimension
    al <- demdata::afghan.life
    al <- Values(al)
    mx <- subarray(al,
                   subarray = ((fun == "mx")
                       & (time == "2001-2005")
                       & (sex == "Female")))
    ax <- subarray(al,
                   subarray = ((fun == "ax")
                       & (time == "2001-2005")
                       & (sex == "Female")))
    mx <- ValuesOne(mx,
                    labels = names(mx),
                    name = "age")
    ax <- ValuesOne(ax,
                    labels = names(ax),
                    name = "age")
    ans.obtained <- calculateLifeTableFuns(mx = mx,
                                           ax = ax,
                                           radix = 1000,
                                           funs = c("mx", "ax", "lx"),
                                           ltFunSecond = TRUE)
    nx <- matrix(c(1, 4, rep(5, 16), Inf), nr = 19, nc  = 1)
    ans.expected <- c(mx@.Data,
                      ax@.Data,
                      1000*convertLifeTableFun(.Data = matrix(mx@.Data,nc=1), 
                                               from = "mx",
                                               to = "lx",
                                               ax = matrix(ax@.Data, nc=1),
                                               nx = nx,
                                               open = TRUE,
                                               mA = NULL))
    ans.expected <- array(ans.expected,
                          dim = c(19, 3))
    dimnames(ans.expected) <- c(dimnames(mx),
                                list(lt.fun = c("mx", "ax", "lx")))
    ans.expected <- Values(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})

test_that("calculateLifeTableFuns works - ltFunSecond is FALSE", {
    calculateLifeTableFuns <- demlife:::calculateLifeTableFuns
    convertLifeTableFun <- demlife:::convertLifeTableFun
    ## 2 dimensions
    al <- demdata::afghan.life
    al <- Values(al)
    mx <- subarray(al,
                   subarray = (fun == "mx") & (time == "2001-2005"))
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005"))
    ans.obtained <- calculateLifeTableFuns(mx = mx,
                                           ax = ax,
                                           radix = 100,
                                           funs = c("mx", "ax", "dx"),
                                           ltFunSecond = FALSE)
    nx <- matrix(c(1, 4, rep(5, 16), Inf), nr = 19, nc  = 2)
    ans.expected <- c(mx@.Data,
                      ax@.Data,
                      100*convertLifeTableFun(.Data = mx@.Data,
                                          from = "mx",
                                          to = "dx",
                                          ax = ax@.Data,
                                          nx = nx,
                                          open = TRUE,
                                          mA = NULL))
    ans.expected <- array(ans.expected,
                          dim = c(dim(mx), 3))
    dimnames(ans.expected) <- c(dimnames(mx),
                                list(lt.fun = c("mx", "ax", "dx")))
    ans.expected <- Values(ans.expected)
    expect_identical(ans.obtained, ans.expected)
    ## 1 dimension
    al <- demdata::afghan.life
    al <- Values(al)
    mx <- subarray(al,
                   subarray = ((fun == "mx")
                       & (time == "2001-2005")
                       & (sex == "Female")))
    ax <- subarray(al,
                   subarray = ((fun == "ax")
                       & (time == "2001-2005")
                       & (sex == "Female")))
    mx <- ValuesOne(mx,
                    labels = names(mx),
                    name = "age")
    ax <- ValuesOne(ax,
                    labels = names(ax),
                    name = "age")
    ans.obtained <- calculateLifeTableFuns(mx = mx,
                                           ax = ax,
                                           radix = 1000,
                                           funs = c("mx", "ax", "Lx"),
                                           ltFunSecond = FALSE)
    nx <- matrix(c(1, 4, rep(5, 16), Inf), nr = 19, nc  = 1)
    ans.expected <- c(mx@.Data,
                      ax@.Data,
                      1000*convertLifeTableFun(.Data = matrix(mx@.Data,nc=1), 
                                          from = "mx",
                                          to = "Lx",
                                          ax = matrix(ax@.Data, nc=1),
                                          nx = nx,
                                          open = TRUE,
                                          mA = NULL))
    ans.expected <- array(ans.expected,
                          dim = c(19, 3))
    dimnames(ans.expected) <- c(dimnames(mx),
                                list(lt.fun = c("mx", "ax", "Lx")))
    ans.expected <- Values(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})

test_that("checkAndTidyProb works", {
    checkAndTidyProb <- demlife:::checkAndTidyProb
    expect_identical(checkAndTidyProb(c(0.025, 0.5, 0.975)),
                     c(0.025, 0.5, 0.975))
    expect_identical(checkAndTidyProb(c(0L, 1L)),
                     c(0.0, 1.0))
    ## 'prob' does not have length 0
    expect_error(checkAndTidyProb(numeric()),
                 "'prob' has length 0")
    ## 'prob' is numeric
    expect_error(checkAndTidyProb("wrong"),
                 "'prob' is non-numeric")
    ## 'prob' has no missing values
    expect_error(checkAndTidyProb(c(0.025, NA)),
                 "'prob' has missing values")
    ## 'prob' all non-negative
    expect_error(checkAndTidyProb(c(-0.025, 0.5)),
                 "'prob' has negative values")
    ## 'prob' all less than or equal to 1
    expect_error(checkAndTidyProb(c(0.025, 1.1)),
                 "'prob' has values greater than 1")
    ## 'prob' monotonically increasing
    expect_error(checkAndTidyProb(c(0.025, 0.975, 0.5)),
                 "values of 'prob' not increasing")
})

test_that("checkAndTidyRadix works", {
    checkAndTidyRadix <- demlife:::checkAndTidyRadix
    expect_identical(checkAndTidyRadix(100000), 100000)
    expect_identical(checkAndTidyRadix(1L), 1)
    ## 'radix' has length 1
    expect_error(checkAndTidyRadix(c(1, 1)),
                 "'radix' does not have length 1")
    ## 'radix' is numeric
    expect_error(checkAndTidyRadix("1"),
                 "'radix' is non-numeric")
    ## 'radix' is not missing
    expect_error(checkAndTidyRadix(as.numeric(NA)),
                 "'radix' is missing")
    ## 'radix' is positive
    expect_error(checkAndTidyRadix(0),
                 "'radix' is non-positive")
})

test_that("checkLifeTableInputValues works", {
    checkLifeTableInputValues <- demlife:::checkLifeTableInputValues
    x <- Values(array(c(0.3, 0.1, 0.2, 0.4, 0.2, 0.5),
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          sex = c("m", "f"))))
    expect_identical(checkLifeTableInputValues(x, from = "mx", radix = 100000),
                     NULL)
    ## length > 0
    x.wrong <- Values(array(0,
                            dim = c(3, 0),
                            dimnames = list(age = c("0-4", "5-9", "10+"),
                                sex = character())))
    expect_error(checkLifeTableInputValues(x.wrong, from = "mx", radix = 100000),
                 "'object' has length 0")
    ## no missing values
    x.wrong <- x
    x.wrong[1] <- NA
    expect_error(checkLifeTableInputValues(x.wrong, from = "mx", radix = 100000),
                 "'object' has missing values")
    ## no negative values
    x.wrong <- x
    x.wrong[1] <- -0.01
    expect_error(checkLifeTableInputValues(x.wrong, from = "mx", radix = 100000),
                 "'object' has negative values")
    ## if probability or dx, no values > 1
    x.wrong <- x
    x.wrong[1] <- 100001
    expect_error(checkLifeTableInputValues(x.wrong, from = "dx", radix = 100000),
                 "'from' is \"dx\" but 'object' has values greater than 'radix'")
    ## if lx, all inital values equal 'radix'
    x.wrong <- x
    expect_error(checkLifeTableInputValues(x.wrong, from = "lx", radix = 100000),
                 "'from' is \"lx\" but some values for first age group do not equal 'radix'")
    ## if Lx, all values less than nx
    x.wrong <- Values(array(c(4, 3, 10, 4, 6, 1),
                            dim = c(3, 2),
                            dimnames = list(age = c("0-4", "5-9", "10+"),
                                sex = c("m", "f"))))
    expect_error(checkLifeTableInputValues(x.wrong, from = "Lx", radix = 1),
                 "'from' is \"Lx\" but some values are larger than 'radix' times the width of the age group")
    ## if lx, Lx, Tx, values are non-increasing
    x.wrong <- Values(array(c(4, 3, 10, 4, 3, 1),
                            dim = c(3, 2),
                            dimnames = list(age = c("0-4", "5-9", "10+"),
                                sex = c("m", "f"))))
    expect_error(checkLifeTableInputValues(x.wrong, from = "Tx", radix = 100000),
                 "'from' is \"Tx\" but some values increase with age")
})

test_that("checkShowFun works", {
    checkShowFun <- demlife:::checkShowFun
    ## 'showFun' does not have length 0
    expect_error(checkShowFun(character()),
                 "'showFun' has length 0")
    ## 'showFun' has no missing values
    expect_error(checkShowFun(c("mx", NA)),
                 "'showFun' has missing values")
    ## 'showFun' has no blanks
    expect_error(checkShowFun(c("mx", "")),
                 "'showFun' has blanks")
    ## 'showFun' has no duplicates
    expect_error(checkShowFun(c("mx", "mx")),
                 "'showFun' has duplicates")
    ## all elements of 'showFun' are valid life table functions
    expect_error(checkShowFun(c("mx", "wrong")),
                 "\"wrong\" is not a valid life table function")
})

test_that("checkShowQuantiles works", {
    checkShowQuantiles <- demlife:::checkShowQuantiles
    ## 'showQuantiles' is logical
    expect_error(checkShowQuantiles("wrong"),
                 "'showQuantiles' does not have type \"logical\"")
    ## 'showQuantiles' has length 1
    expect_error(checkShowQuantiles(logical()),
                 "'showQuantiles' does not have length 1")
    ## 'showQuantiles' has no missing values
    expect_error(checkShowQuantiles(NA),
                 "'showQuantiles' is missing")
})

test_that("getLifeTableClass works", {
    getLifeTableClass <- demlife:::getLifeTableClass
    for (name in c("mx", "qx", "px", "ex", "ax"))
        expect_identical(getLifeTableClass(name),
                         "Values")
    for (name in c("dx", "lx", "Lx", "Tx"))
        expect_identical(getLifeTableClass(name),
                         "Counts")
    expect_error(getLifeTableClass("wrong"),
                 "\"wrong\" is not a valid life table function")
})

test_that("getLifeTableDimScale works", {
    getLifeTableDimScale <- demlife:::getLifeTableDimScale
    for (name in c("mx", "qx", "px", "dx", "Lx"))
        expect_identical(getLifeTableDimScale(name),
                         "Intervals")
    for (name in c("lx", "Tx", "ex"))
        expect_identical(getLifeTableDimScale(name),
                         "Points")
    expect_error(getLifeTableDimScale("wrong"),
                 "\"wrong\" is not a valid life table function")
})

test_that("makeLifeTableMetaData works", {
    makeLifeTableMetaData <- demlife:::makeLifeTableMetaData
    ## mx to qx, open
    object <- new("MetaData",
                  nms = "age",
                  dimtypes = "age",
                  DimScales = list(new("Intervals",
                      dimvalues = c(seq(0, 85, 5), Inf))))
    ans.obtained <- makeLifeTableMetaData(object, from = "mx", to = "qx")
    ans.expected <- new("MetaData",
                        nms = "age",
                        dimtypes = "age",
                        DimScales = list(new("Intervals",
                            dimvalues = c(seq(0, 85, 5), Inf))))
    expect_identical(ans.obtained, ans.expected)
    ## mx to qx, closed
    object <- new("MetaData",
                  nms = "age",
                  dimtypes = "age",
                  DimScales = list(new("Intervals",
                      dimvalues = seq(0, 85, 5))))
    ans.obtained <- makeLifeTableMetaData(object, from = "mx", to = "qx")
    ans.expected <- new("MetaData",
                        nms = "age",
                        dimtypes = "age",
                        DimScales = list(new("Intervals",
                            dimvalues = seq(0, 85, 5))))
    expect_identical(ans.obtained, ans.expected)
    ## mx to px, open
    object <- new("MetaData",
                  nms = "age",
                  dimtypes = "age",
                  DimScales = list(new("Intervals",
                      dimvalues = c(seq(0, 85, 5), Inf))))
    ans.obtained <- makeLifeTableMetaData(object, from = "mx", to = "px")
    ans.expected <- new("MetaData",
                        nms = "age",
                        dimtypes = "age",
                        DimScales = list(new("Intervals",
                            dimvalues = c(seq(0, 85, 5), Inf))))
    expect_identical(ans.obtained, ans.expected)
    ## mx to px, closed
    object <- new("MetaData",
                  nms = "age",
                  dimtypes = "age",
                  DimScales = list(new("Intervals",
                      dimvalues = seq(0, 85, 5))))
    ans.obtained <- makeLifeTableMetaData(object, from = "mx", to = "px")
    ans.expected <- new("MetaData",
                        nms = "age",
                        dimtypes = "age",
                        DimScales = list(new("Intervals",
                            dimvalues = seq(0, 85, 5))))
    expect_identical(ans.obtained, ans.expected)
    ## mx to lx, open
    object <- new("MetaData",
                  nms = "age",
                  dimtypes = "age",
                  DimScales = list(new("Intervals",
                      dimvalues = c(seq(0, 85, 5), Inf))))
    ans.obtained <- makeLifeTableMetaData(object, from = "mx", to = "lx")
    ans.expected <- new("MetaData",
                        nms = "age",
                        dimtypes = "age",
                        DimScales = list(new("Points",
                            dimvalues = seq(0, 85, 5))))
    expect_identical(ans.obtained, ans.expected)
    ## mx to lx, closed
    object <- new("MetaData",
                  nms = "age",
                  dimtypes = "age",
                  DimScales = list(new("Intervals",
                      dimvalues = seq(0, 85, 5))))
    ans.obtained <- makeLifeTableMetaData(object, from = "mx", to = "lx")
    ans.expected <- new("MetaData",
                        nms = "age",
                        dimtypes = "age",
                        DimScales = list(new("Points",
                            dimvalues = seq(0, 85, 5))))
    expect_identical(ans.obtained, ans.expected)
    ## mx to dx, open
    object <- new("MetaData",
                  nms = "age",
                  dimtypes = "age",
                  DimScales = list(new("Intervals",
                      dimvalues = c(seq(0, 85, 5), Inf))))
    ans.obtained <- makeLifeTableMetaData(object, from = "mx", to = "dx")
    ans.expected <- new("MetaData",
                        nms = "age",
                        dimtypes = "age",
                        DimScales = list(new("Intervals",
                            dimvalues = c(seq(0, 85, 5), Inf))))
    expect_identical(ans.obtained, ans.expected)
    ## mx to dx, closed
    object <- new("MetaData",
                  nms = "age",
                  dimtypes = "age",
                  DimScales = list(new("Intervals",
                      dimvalues = seq(0, 85, 5))))
    ans.obtained <- makeLifeTableMetaData(object, from = "mx", to = "dx")
    ans.expected <- new("MetaData",
                        nms = "age",
                        dimtypes = "age",
                        DimScales = list(new("Intervals",
                            dimvalues = seq(0, 85, 5))))
    expect_identical(ans.obtained, ans.expected)
    ## mx to Lx, open
    object <- new("MetaData",
                  nms = "age",
                  dimtypes = "age",
                  DimScales = list(new("Intervals",
                      dimvalues = c(seq(0, 85, 5), Inf))))
    ans.obtained <- makeLifeTableMetaData(object, from = "mx", to = "Lx")
    ans.expected <- new("MetaData",
                        nms = "age",
                        dimtypes = "age",
                        DimScales = list(new("Intervals",
                            dimvalues = c(seq(0, 85, 5), Inf))))
    expect_identical(ans.obtained, ans.expected)
    ## mx to Lx, closed
    object <- new("MetaData",
                  nms = "age",
                  dimtypes = "age",
                  DimScales = list(new("Intervals",
                      dimvalues = seq(0, 85, 5))))
    ans.obtained <- makeLifeTableMetaData(object, from = "mx", to = "Lx")
    ans.expected <- new("MetaData",
                        nms = "age",
                        dimtypes = "age",
                        DimScales = list(new("Intervals",
                            dimvalues = seq(0, 85, 5))))
    expect_identical(ans.obtained, ans.expected)
    ## mx to Tx, open
    object <- new("MetaData",
                  nms = "age",
                  dimtypes = "age",
                  DimScales = list(new("Intervals",
                      dimvalues = c(seq(0, 85, 5), Inf))))
    ans.obtained <- makeLifeTableMetaData(object, from = "mx", to = "Tx")
    ans.expected <- new("MetaData",
                        nms = "age",
                        dimtypes = "age",
                        DimScales = list(new("Points",
                            dimvalues = seq(0, 85, 5))))
    expect_identical(ans.obtained, ans.expected)
    ## mx to Tx, closed
    object <- new("MetaData",
                  nms = "age",
                  dimtypes = "age",
                  DimScales = list(new("Intervals",
                      dimvalues = seq(0, 85, 5))))
    expect_error(makeLifeTableMetaData(object, from = "mx", to = "Tx"),
                 "cannot calculate \"Tx\" when last age group is closed")
    ## lx to mx
    object <- new("MetaData",
                  nms = "age",
                  dimtypes = "age",
                  DimScales = list(new("Points",
                      dimvalues = c(seq(0, 85, 5)))))
    ans.obtained <- makeLifeTableMetaData(object, from = "lx", to = "mx")
    ans.expected <- new("MetaData",
                        nms = "age",
                        dimtypes = "age",
                        DimScales = list(new("Intervals",
                            dimvalues = seq(0, 85, 5))))
    expect_identical(ans.obtained, ans.expected)
    ## ex to mx
    object <- new("MetaData",
                  nms = "age",
                  dimtypes = "age",
                  DimScales = list(new("Points",
                      dimvalues = c(seq(0, 85, 5)))))
    ans.obtained <- makeLifeTableMetaData(object, from = "ex", to = "mx")
    ans.expected <- new("MetaData",
                        nms = "age",
                        dimtypes = "age",
                        DimScales = list(new("Intervals",
                            dimvalues = c(seq(0, 85, 5), Inf))))
    expect_identical(ans.obtained, ans.expected)
    ## lx to ex
    object <- new("MetaData",
                  nms = "age",
                  dimtypes = "age",
                  DimScales = list(new("Points",
                      dimvalues = c(seq(0, 85, 5)))))
    expect_error(makeLifeTableMetaData(object, from = "lx", to = "ex"),
                 "cannot calculate \"ex\" from \"lx\"")
    ## ex to Tx
    object <- new("MetaData",
                  nms = "age",
                  dimtypes = "age",
                  DimScales = list(new("Points",
                      dimvalues = c(seq(0, 85, 5)))))
    ans.obtained <- makeLifeTableMetaData(object, from = "ex", to = "Tx")
    ans.expected <- new("MetaData",
                        nms = "age",
                        dimtypes = "age",
                        DimScales = list(new("Points",
                            dimvalues = seq(0, 85, 5))))
    expect_identical(ans.obtained, ans.expected)
    ## invalid to, from is Intervals
    object <- new("MetaData",
                  nms = "age",
                  dimtypes = "age",
                  DimScales = list(new("Intervals",
                      dimvalues = c(seq(0, 85, 5)))))
    expect_error(makeLifeTableMetaData(object, from = "mx", to = "wrong"),
                 "\"wrong\" is not a valid life table function")
    ## invalid from, from is Points
    object <- new("MetaData",
                  nms = "age",
                  dimtypes = "age",
                  DimScales = list(new("Points",
                      dimvalues = c(seq(0, 85, 5)))))
    expect_error(makeLifeTableMetaData(object, from = "wrong", to = "ex"),
                 "\"wrong\" is not a valid life table function")
})



## ## origin is "mx", open
## ax <- c(0.5, 2, rep(2.5, 16), NA)
## nx <- c(1, 4, rep(5, 16), Inf)
## qx <- convertLifeTableFunInner(mx, from = "mx", to = "qx", nx = nx, ax = ax,
##                      open = TRUE, mA = NULL)
## px <- convertLifeTableFunInner(mx, from = "mx", to = "px", nx = nx, ax = ax,
##                      open = TRUE, mA = NULL)
## lx <- convertLifeTableFunInner(mx, from = "mx", to = "lx", nx = nx, ax = ax,
##                      open = TRUE, mA = NULL)
## dx <- convertLifeTableFunInner(mx, from = "mx", to = "dx", nx = nx, ax = ax,
##                      open = TRUE, mA = NULL)
## Lx <- convertLifeTableFunInner(mx, from = "mx", to = "Lx", nx = nx, ax = ax,
##                      open = TRUE, mA = NULL)
## Tx <- convertLifeTableFunInner(mx, from = "mx", to = "Tx", nx = nx, ax = ax,
##                      open = TRUE, mA = NULL)
## ex <- convertLifeTableFunInner(mx, from = "mx", to = "ex", nx = nx, ax = ax,
##                      open = TRUE, mA = NULL)


## ## origin is "mx", closed
## ax <- c(0.5, 2, rep(2.5, 16))
## nx <- c(1, 4, rep(5, 16))
## qx <- convertLifeTableFunInner(mx[-19,,drop=FALSE], from = "mx", to = "qx", nx = nx, ax = ax,
##                      open = FALSE, mA = NULL)
## px <- convertLifeTableFunInner(mx[-19,,drop=FALSE], from = "mx", to = "px", nx = nx, ax = ax,
##                      open = FALSE, mA = NULL)
## lx <- convertLifeTableFunInner(mx[-19,,drop=FALSE], from = "mx", to = "lx", nx = nx, ax = ax,
##                      open = FALSE, mA = NULL)
## dx <- convertLifeTableFunInner(mx[-19,,drop=FALSE], from = "mx", to = "dx", nx = nx, ax = ax,
##                      open = FALSE, mA = NULL)
## Lx <- convertLifeTableFunInner(mx[-19,,drop=FALSE], from = "mx", to = "Lx", nx = nx, ax = ax,
##                      open = FALSE, mA = NULL)
## Tx <- convertLifeTableFunInner(mx[-19,,drop=FALSE], from = "mx", to = "Tx", nx = nx, ax = ax,
##                      open = FALSE, mA = NULL)
## ex <- convertLifeTableFunInner(mx[-19,,drop=FALSE], from = "mx", to = "ex", nx = nx, ax = ax,
##                      open = FALSE, mA = NULL)


