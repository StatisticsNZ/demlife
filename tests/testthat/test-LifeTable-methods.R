
context("LifeTable-methods")
n.test <- 5
test.identity <- FALSE

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
    lt <- LifeTable(mx = mx,
                    ax = ax)
    ans.obtained <- lifeTableFun(lt, fun = "qx")
    ans.expected <- subarray(al,
                             subarray = (fun == "qx") & (time == "2001-2005"))
    expect_equal(ans.obtained, ans.expected, tol = 0.001)
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
