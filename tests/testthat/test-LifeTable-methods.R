
context("LifeTable-methods")
n.test <- 5
test.identity <- FALSE


test_that("collapseIntervals works", {
    makeLx <- demlife:::makeLx
    al <- demdata::afghan.life
    al <- Values(al)
    mx <- subarray(al,
                   subarray = (fun == "mx") & (time == "2001-2005"))
    lt <- LifeTable(mx = mx)
    ax <- lt@ax
    ## do not supply weights
    ans.obtained <- collapseIntervals(lt,
                                      dimension = "age",
                                      breaks = seq(5, 70, 10))
    Lx <- makeLx(mx = mx, ax = ax)
    mx.collapsed <- collapseIntervals(lt@mx,
                                      dimension = "age",
                                      breaks = seq(5, 70, 10),
                                      weights = Lx)
    ax.collapsed <- collapseIntervals(lt@ax,
                                      dimension = "age",
                                      breaks = seq(5, 70, 10),
                                      weights = Lx)
    ans.expected <- LifeTable(mx = mx.collapsed,
                              ax = ax.collapsed)
    expect_identical(ans.obtained, ans.expected)
    ## supply weights
    popn <- 1000 * makeLx(mx = mx, ax = ax) + 5
    ans.obtained <- collapseIntervals(lt,
                                      dimension = "age",
                                      breaks = seq(5, 70, 10),
                                      weights = popn)
    mx.collapsed <- collapseIntervals(lt@mx,
                                      dimension = "age",
                                      breaks = seq(5, 70, 10),
                                      weights = popn)
    ax.collapsed <- collapseIntervals(lt@ax,
                                      dimension = "age",
                                      breaks = seq(5, 70, 10),
                                      weights = popn)
    ans.expected <- LifeTable(mx = mx.collapsed,
                              ax = ax.collapsed)
    expect_identical(ans.obtained, ans.expected)
    ## aggregate time periods, not age
    mx <- subarray(al, subarray = fun == "mx")
    lt <- LifeTable(mx)
    ans.obtained <- collapseIntervals(lt,
                                      dimension = "time",
                                      width = 10)
    Lx <- makeLx(mx = lt@mx, ax = lt@ax)
    mx.collapsed <- collapseIntervals(lt@mx,
                                      dimension = "time",
                                      width = 10,
                                      weights = Lx)
    ax.collapsed <- collapseIntervals(lt@ax,
                                      dimension = "time",
                                      width = 10,
                                      weights = Lx)
    ans.expected <- LifeTable(mx = mx.collapsed,
                              ax = ax.collapsed)
    expect_identical(ans.obtained, ans.expected)
})

test_that("lifeExpectancy works", {
    ## result has one dimension
    al <- demdata::afghan.life
    al <- Values(al)
    mx <- subarray(al,
                   subarray = (fun == "mx") & (time == "2001-2005"))
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005"))
    lt <- LifeTable(mx = mx,
                    ax = ax)
    ans.obtained <- lifeExpectancy(lt)
    ans.expected <- lifeTableFun(lt, fun = "ex")
    ans.expected <- slab(ans.expected,
                         dimension = "age",
                         elements = 1L)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- lifeExpectancy(lt, age = 10)
    ans.expected <- lifeTableFun(lt, fun = "ex")
    ans.expected <- slab(ans.expected,
                         dimension = "age",
                         elements = 4L)
    expect_identical(ans.obtained, ans.expected)
    ## result is number
    al <- demdata::afghan.life
    al <- Values(al)
    mx <- subarray(al,
                   subarray = (fun == "mx") & (time == "2001-2005") &
                       (sex == "Female"))
    mx <- as(mx, "array")
    names(dimnames(mx)) <- "age"
    mx <- Values(mx)
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005") & (sex == "Female"))
    ax <- as(ax, "array")
    names(dimnames(ax)) <- "age"
    ax <- Values(ax)
    lt <- LifeTable(mx = mx, ax = ax)
    ans.obtained <- lifeExpectancy(lt)
    ans.expected <- lifeTableFun(lt, fun = "ex")[[1]]
    expect_identical(ans.obtained, ans.expected)
})

test_that("lifeTableFun works", {
    al <- demdata::afghan.life
    al <- Values(al)
    mx <- subarray(al,
                   subarray = (fun == "mx") & (time == "2001-2005"))
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005"))
    ## showTotal is TRUE
    lt <- LifeTable(mx = mx,
                    ax = ax)
    ans.obtained <- lifeTableFun(lt, fun = "qx")
    ans.expected <- subarray(al,
                             subarray = (fun == "qx") & (time == "2001-2005"))
    expect_equal(as.numeric(subarray(ans.obtained, sex %in% c("Female", "Male"))),
                 as.numeric(ans.expected), tol = 0.001)
    ## showTotal is FALSE
    lt <- LifeTable(mx = mx,
                    ax = ax,
                    showTotal = FALSE)
    ans.obtained <- lifeTableFun(lt, fun = "qx")
    ans.expected <- subarray(al,
                             subarray = (fun == "qx") & (time == "2001-2005"))
    expect_equal(ans.obtained, ans.expected, tol = 0.001)
    ## life expectancy
    lt <- LifeTable(mx = mx,
                    ax = ax,
                    showTotal = FALSE)
    ans.obtained <- lifeTableFun(lt, fun = "ex")
    ans.expected <- subarray(al,
                             subarray = (fun == "ex") & (time == "2001-2005"))
    ans.expected <- as.array(ans.expected)
    dimnames(ans.expected)$age <- c(0, 1, seq(5, 85, 5))
    ans.expected <- Values(ans.expected)
    expect_equal(ans.obtained, ans.expected, tol = 0.001)
})


test_that("prob works", {
    al <- demdata::afghan.life
    al <- Values(al)
    mx <- subarray(al,
                   subarray = (fun == "mx") & (time == "2001-2005"))
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005"))
    lt <- LifeTable(mx = mx,
                    ax = ax,
                    prob = c(0.1, 0.9))
    expect_identical(prob(lt), c(0.1, 0.9))
})

test_that("prob replacement function works", {
    al <- demdata::afghan.life
    al <- Values(al)
    mx <- subarray(al,
                   subarray = (fun == "mx") & (time == "2001-2005"))
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005"))
    lt <- LifeTable(mx = mx,
                    ax = ax,
                    prob = c(0.1, 0.9))
    prob(lt) <- c(0.2, 0.8)
    expect_identical(prob(lt), c(0.2, 0.8))
})

test_that("radix works", {
    al <- demdata::afghan.life
    al <- Values(al)
    mx <- subarray(al,
                   subarray = (fun == "mx") & (time == "2001-2005"))
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005"))
    lt <- LifeTable(mx = mx,
                    ax = ax,
                    radix = 123)
    expect_identical(radix(lt), 123)
})

test_that("radix replacement function works", {
    al <- demdata::afghan.life
    al <- Values(al)
    mx <- subarray(al,
                   subarray = (fun == "mx") & (time == "2001-2005"))
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005"))
    lt <- LifeTable(mx = mx,
                    ax = ax,
                    radix = 123)
    radix(lt) <- 321
    expect_identical(radix(lt), 321)
})

test_that("showFun works", {
    al <- demdata::afghan.life
    al <- Values(al)
    mx <- subarray(al,
                   subarray = (fun == "mx") & (time == "2001-2005"))
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005"))
    lt <- LifeTable(mx = mx,
                    ax = ax,
                    showFun = c("ex", "Tx"))
    expect_identical(showFun(lt), c("ex", "Tx"))
})

test_that("showFun replacement function works", {
    al <- demdata::afghan.life
    al <- Values(al)
    mx <- subarray(al,
                   subarray = (fun == "mx") & (time == "2001-2005"))
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005"))
    lt <- LifeTable(mx = mx,
                    ax = ax,
                    showFun = c("ex", "Tx"))
    showFun(lt) <- c("lx", "dx")
    expect_identical(showFun(lt), c("lx", "dx"))
})

test_that("showQuantiles works", {
    al <- demdata::afghan.life
    al <- Values(al)
    mx <- subarray(al,
                   subarray = (fun == "mx") & (time == "2001-2005"))
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005"))
    lt <- LifeTable(mx = mx,
                    ax = ax,
                    showQuantiles = FALSE)
    expect_false(showQuantiles(lt))
})

test_that("showQuantiles replacement function works", {
    al <- demdata::afghan.life
    al <- Values(al)
    mx <- subarray(al,
                   subarray = (fun == "mx") & (time == "2001-2005"))
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005"))
    lt <- LifeTable(mx = mx,
                    ax = ax,
                    showQuantiles = FALSE)
    showQuantiles(lt) <- TRUE
    expect_true(showQuantiles(lt))
})













## Lx1 <- Counts(array(c(96354,
##                       377877,
##                       467474,
##                       464534,
##                       460915,
##                       455193,
##                       447783,
##                       439466,
##                       429742,
##                       418269,
##                       403859,
##                       384356,
##                       358766,
##                       324494,
##                       279761,
##                       223797,
##                       155169,
##                       89054,
##                       55200) / 100000,
##                     dim = 19,
##                     dimnames = list(age = c("0",
##                                         "1-4",
##                                         paste(seq(5, 80, 5),
##                                               seq(9, 84, 5),
##                                               sep = "-"),
##                                         "85+"))))
## Lx2 <- Counts(array(c(99410,
##                       396947,
##                       495676,
##                       495275,
##                       494459,
##                       493254,
##                       491863,
##                       489996,
##                       487383,
##                       483743,
##                       478583,
##                       470679,
##                       458397,
##                       439689,
##                       411580,
##                       372191,
##                       318738,
##                       248061,
##                       274139) / 100000,
##                     dim = 19,
##                     dimnames = list(age = c("0",
##                                         "1-4",
##                                         paste(seq(5, 80, 5),
##                                               seq(9, 84, 5),
##                                               sep = "-"),
##                                         "85+"))))
## lx1 <- Counts(array(c(100000,
##                       95458,
##                       93887,
##                       93174,
##                       92613,
##                       91681,
##                       90341,
##                       88746,
##                       86997,
##                       84847,
##                       82368,
##                       79012,
##                       74539,
##                       68688,
##                       60779,
##                       50757,
##                       38276,
##                       23930,
##                       12281) / 100000,
##                     dim = 19,
##                     dimnames = list(age = c(0, 1, seq(5, 85, 5)))))
## lx2 <- Counts(array(c(100000,
##                       99321,
##                       99179,
##                       99096,
##                       98999,
##                       98772,
##                       98524,
##                       98206,
##                       97769,
##                       97152,
##                       96298,
##                       95048,
##                       93085,
##                       90071,
##                       85504,
##                       78775,
##                       69655,
##                       57275,
##                       41424) / 100000,
##                     dim = 19,
##                     dimnames = list(age = c(0, 1, seq(5, 85, 5)))))

## decompLifeExpPair(lx1, lx2, Lx1, Lx2)



test_that("Sx works", {
    makeLx <- demlife:::makeLx
    al <- demdata::afghan.life
    al <- Values(al)
    mx <- subarray(al,
                   subarray = (fun == "mx") & (time == "2001-2005"))
    lt <- LifeTable(mx = mx)
    ## life table not regular
    expect_error(Sx(lt),
                 "age groups have unequal lengths : consider using function 'collapseIntervals' to make lengths equal")
    ## life table regular; useLabelStart = TRUE
    lt <- collapseIntervals(lt,
                            dimension = "age",
                            width = 5)
    ans.obtained <- Sx(lt)
    Lx <- lifeTableFun(lt, "Lx")
    head <- 1/subarray(Lx, age < 80) * as.numeric(subarray(Lx, age > 5 & age < 85))
    tail <- 1/collapseIntervals(subarray(Lx, age > 80), dimension = "age", breaks = 80) * as.numeric(subarray(Lx, age > 85))
    ans.expected <- dbind(head, tail, along = "age")
    ans.expected <- as(ans.expected, "Values")
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
    ## life table regular; useLabelStart = FALSE
    lt <- collapseIntervals(lt,
                            dimension = "age",
                            width = 5)
    ans.obtained <- Sx(lt, useLabelStart = FALSE)
    Lx <- lifeTableFun(lt, "Lx")
    head <- subarray(Lx, age > 5 & age < 85) / as.numeric(subarray(Lx, age < 80))
    tail <- subarray(Lx, age > 85, drop = FALSE) / as.numeric(collapseIntervals(subarray(Lx, age > 80), dimension = "age", breaks = 80))
    ans.expected <- dbind(head, tail, along = "age")
    ans.expected <- as(ans.expected, "Values")
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
    ## make sure that function does no require age intervals and time
    ## intervals to have the same length
    mx <- addDimension(mx, name = "time", labels = "2000", dimscale = "Intervals")
    lt <- LifeTable(mx)
    lt <- collapseIntervals(lt,
                            dimension = "age",
                            width = 5)
    ans.obtained <- Sx(lt)
    expect_identical(dimnames(ans.obtained)$time, "2000")
})
