
context("convertToMx")

test_that("convertToMx works with qx", {
    convertToMx <- demlife:::convertToMx
    al <- demdata::afghan.life
    al <- Values(al)
    object <- subarray(al,
                       subarray = (fun == "qx") & (time == "2001-2005"))
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005"))
    ans.obtained <- convertToMx(object = object,
                                from = "qx",
                                ax = ax)
    ans.expected <- subarray(al,
                             subarray = (fun == "mx") & (time == "2001-2005") & (age < 85))
    expect_equal(ans.obtained, ans.expected, tol = 0.000001)
})

test_that("convertToMx works with px", {
    convertToMx <- demlife:::convertToMx
    al <- demdata::afghan.life
    al <- Values(al)
    object <- subarray(al,
                       subarray = (fun == "px") & (time == "2001-2005"))
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005"))
    ans.obtained <- convertToMx(object = object,
                                from = "px",
                                ax = ax)
    ans.expected <- subarray(al,
                             subarray = (fun == "mx") & (time == "2001-2005") & (age < 85))
    expect_equal(ans.obtained, ans.expected, tol = 0.000001)
})

test_that("convertToMx works with dx", {
    convertToMx <- demlife:::convertToMx
    al <- demdata::afghan.life
    al <- Values(al)
    object <- subarray(al,
                       subarray = (fun == "dx") & (time == "2001-2005"))
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005"))
    ans.obtained <- convertToMx(object = object,
                                from = "dx",
                                ax = ax)
    ans.expected <- subarray(al,
                             subarray = (fun == "mx") & (time == "2001-2005") & (age < 85))
    expect_equal(ans.obtained, ans.expected, tol = 0.001)
})

test_that("convertToMx works with lx", {
    convertToMx <- demlife:::convertToMx
    al <- demdata::afghan.life
    al <- Values(al)
    object <- subarray(al,
                       subarray = (fun == "lx") & (time == "2001-2005"))
    object <- as(object, "array")
    dimnames(object)$age <- c(0, 1, seq(5, 85, 5))
    object <- Counts(object)
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005"))
    ans.obtained <- convertToMx(object = object,
                                from = "lx",
                                ax = ax)
    ans.expected <- subarray(al,
                             subarray = (fun == "mx") & (time == "2001-2005") & (age < 85))
    expect_equal(ans.obtained, ans.expected, tol = 0.001)
})

test_that("convertToMx works with lx", {
    convertToMx <- demlife:::convertToMx
    al <- demdata::afghan.life
    al <- Values(al)
    object <- subarray(al,
                       subarray = (fun == "lx") & (time == "2001-2005"))
    object <- as(object, "array")
    dimnames(object)$age <- c(0, 1, seq(5, 85, 5))
    object <- Counts(object)
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005"))
    ans.obtained <- convertToMx(object = object,
                                from = "lx",
                                ax = ax)
    ans.expected <- subarray(al,
                             subarray = (fun == "mx") & (time == "2001-2005") & (age < 85))
    expect_equal(ans.obtained, ans.expected, tol = 0.001)
})

test_that("convertToMx works with Lx", {
    convertToMx <- demlife:::convertToMx
    al <- demdata::afghan.life
    al <- Values(al)
    object <- subarray(al,
                       subarray = (fun == "Lx") & (time == "2001-2005"))
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005"))
    ans.obtained <- convertToMx(object = object,
                                from = "Lx",
                                ax = ax)
    ans.expected <- subarray(al,
                             subarray = (fun == "mx") & (time == "2001-2005"))
    expect_equal(ans.obtained, ans.expected, tol = 0.001)
})

test_that("convertToMx works with Tx", {
    convertToMx <- demlife:::convertToMx
    al <- demdata::afghan.life
    al <- Values(al)
    object <- subarray(al,
                       subarray = (fun == "Tx") & (time == "2001-2005"))
    object <- as(object, "array")
    dimnames(object)$age <- c(0, 1, seq(5, 85, 5))
    object <- Counts(object)
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005"))
    ans.obtained <- convertToMx(object = object,
                                from = "Tx",
                                ax = ax)
    ans.expected <- subarray(al,
                             subarray = (fun == "mx") & (time == "2001-2005"))
    expect_equal(ans.obtained, ans.expected, tol = 0.001)
})

test_that("convertToMx works with ex", {
    convertToMx <- demlife:::convertToMx
    al <- demdata::afghan.life
    al <- Values(al)
    object <- subarray(al,
                       subarray = (fun == "ex") & (time == "2001-2005"))
    object <- as(object, "array")
    dimnames(object)$age <- c(0, 1, seq(5, 85, 5))
    object <- Counts(object)
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005"))
    ans.obtained <- convertToMx(object = object,
                                from = "ex",
                                ax = ax)
    ans.expected <- subarray(al,
                             subarray = (fun == "mx") & (time == "2001-2005"))
    expect_equal(ans.obtained, ans.expected, tol = 0.001)
})

test_that("convertToMx works with age dimension second", {
    convertToMx <- demlife:::convertToMx
    al <- demdata::afghan.life
    al <- Values(al)
    object <- subarray(al,
                       subarray = (fun == "ex") & (time == "2001-2005"))
    object <- aperm(object, perm = c("sex", "age"))
    object <- as(object, "array")
    dimnames(object)$age <- c(0, 1, seq(5, 85, 5))
    object <- Counts(object)
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005"))
    ans.obtained <- convertToMx(object = object,
                                from = "ex",
                                ax = ax)
    ans.expected <- subarray(al,
                             subarray = (fun == "mx") & (time == "2001-2005"))
    ans.expected <- aperm(ans.expected, perm = c("sex", "age"))
    expect_equal(ans.obtained, ans.expected, tol = 0.001)
})

test_that("convertToMx works with one-dimensional object", {
    convertToMx <- demlife:::convertToMx
    al <- demdata::afghan.life
    al <- Values(al)
    object <- subarray(al,
                       subarray = (fun == "ex") & (time == "2001-2005") & (sex == "Female"))
    object <- as(object, "array")
    names(dimnames(object)) <- "age"
    dimnames(object)$age <- c(0, 1, seq(5, 85, 5))
    object <- Counts(object)
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005") & (sex == "Female"))
    ax <- as(ax, "array")
    names(dimnames(ax)) <- "age"
    ax <- Values(ax)
    ans.obtained <- convertToMx(object = object,
                                from = "ex",
                                ax = ax)
    ans.expected <- subarray(al,
                             subarray = (fun == "mx") & (time == "2001-2005") & (sex == "Female"))
    ans.expected <- as(ans.expected, "array")
    names(dimnames(ans.expected)) <- "age"
    ans.expected <- Values(ans.expected)
    expect_equal(ans.obtained, ans.expected, tol = 0.001)
})

test_that("convertToMx throws appropriate errors", {
    convertToMx <- demlife:::convertToMx
    al <- demdata::afghan.life
    al <- Values(al,
                 dimtypes = c(age = "state"))
    object <- subarray(al,
                       subarray = (fun == "qx") & (time == "2001-2005"))
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005"))
    expect_error(convertToMx(object = object,
                                from = "qx",
                             ax = ax),
                 "'object' does not have a dimension with dimtype \"age\"")
})
