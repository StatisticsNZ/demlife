
context("LifeTable-generator")
n.test <- 5
test.identity <- FALSE

test_that("LifeTable creates valid objects - no iterations", {
    al <- demdata::afghan.life
    al <- Values(al)
    mx <- subarray(al,
                   subarray = (fun == "mx") & (time == "2001-2005"))
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005"))
    ans.obtained <- LifeTable(mx = mx,
                              ax = ax)
    expect_true(validObject(ans.obtained))
})

test_that("LifeTable creates valid objects - with iterations", {
    al <- demdata::afghan.life
    al <- Values(al)
    mx <- subarray(al,
                   subarray = (fun == "mx") & (time == "2001-2005"))
    mx <- perturb(mx*1000, n = 20)/1000
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005"))
    ans.obtained <- LifeTable(mx = mx,
                              ax = ax)
    expect_true(validObject(ans.obtained))
})

test_that("LifeTable throws appropriate errors", {
    al <- demdata::afghan.life
    al <- Values(al)
    mx <- subarray(al,
                   subarray = (fun == "mx") & (time == "2001-2005"))
    ax <- subarray(al,
                   subarray = (fun == "ax") & (time == "2001-2005"))
    expect_error(LifeTable(mx = "wrong",
                           ax = ax),
                 "'mx' has class \"character\"")
    expect_error(LifeTable(mx = mx,
                           ax = "wrong"),
                 "'ax' has class \"character\"")
    mx.wrong <- mx
    dimtypes(mx.wrong)["age"] <- "state"
    expect_error(LifeTable(mx = mx.wrong,
                           ax = ax),
                 "'mx' does not have a dimension with dimtype \"age\"")
})
