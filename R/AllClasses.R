
## HAS_TESTS
#' S4 class for describing a life table.
#'
#' An object of class \code{LifeTable} holds values for 'mx' and 'ax',
#' plus the life table 'radix' (ie l0). These provide sufficient information
#' to calculate all life table functions.
#'
#' At present, objects of class \code{LifeTable} can only handle
#' single-decrement life tables.  In future they will be extended to
#' handle multiple-decrement and multi-state life tables.
#'
#' Objects of class \code{LifeTable} also holds names of the life table
#' functions that are displayed when the object is printed, that are
#' included when the object is turned into a data.frame.
#'
#' @slot mx An object of class
#' \code{\link[dembase:DemographicArray-class]{Values}}, holding mortality
#' rates.
#' @slot ax An object of class
#' \code{\link[dembase:DemographicArray-class]{Values}}, with the same
#' metadata as \code{mx}, holding separation factors.
#' @slot radix A positive number.
#' @slot showFun  A character vector with names of life table
#' functions.
#' @slot showQuantiles \code{TRUE} or \code{FALSE}.
#' @slot prob A numeric vector.
#'
#' @param object An object of class \code{"LifeTable"}.
#' 
#' @seealso Objects of class \code{LifeTable} are typically created using
#' function \code{\link{LifeTable}}.
#'
#' @export
setClass("LifeTable",
         slots = c(mx = "Values",
                   ax = "Values",
                   radix = "numeric",
                   showFun = "character",
                   showQuantiles = "logical",
                   prob = "numeric"),
         validity = function(object) {
             mx <- object@mx
             ax <- object@ax
             showFun <- object@showFun
             radix <- object@radix
             showQuantiles <- object@showQuantiles
             prob <- object@prob
             dim <- dim(mx)
             dimtypes <- dimtypes(mx, use.names = FALSE)
             DimScales <- DimScales(mx, use.names = FALSE)
             ## 'ax' and 'mx' have identical metadata
             if (!identical(ax@metadata, mx@metadata))
                 return(gettextf("'%s' and '%s' have different metadata",
                                 "mx", "ax"))
             ## first dimension of 'mx' has dimtype "age"
             if (dimtypes[1L] != "age")
                 return(gettextf("first dimension of '%s' does not have %s \"%s\"",
                                 "mx", "dimtype", "age"))
             ## age dimension of 'mx':
             ## - has age dimension
             ## - has at least 2 age groups
             ## - has dimscale "Intervals"
             ## - first intervals is not open
             ## - last interval can be (but is not necessarily) open
             return.value <- tryCatch(dembase::checkAge(object = mx,
                                                        minAges = 2L,
                                                        regular = FALSE,
                                                        openLeftOK = FALSE,
                                                        openRightOK = TRUE,
                                                        expectedDimscale = "Intervals"),
                                      error = function(e) e)
             if (methods::is(return.value, "error"))
                 return(return.value$message)
             ## if 'mx' has time dimension, dimscale must be "Intervals"
             i.time <- match("time", dimtypes, nomatch = 0L)
             has.time <- i.time > 0L
             if (has.time) {
                 dimscale.time <- DimScales[[i.time]]
                 if (!is(dimscale.time, "Intervals"))
                     stop(gettextf("dimension with %s \"%s\" has %s \"%s\"",
                                   "dimtype", "time", "dimscale", class(dimscale.time)))
             }
             ## 'mx' does not have quantile dimension
             i.quantile <- match("quantile", dimtypes, nomatch = 0L)
             has.quantile <- i.quantile > 0L
             if (has.quantile)
                 stop(gettextf("dimension with dimtype \"%s\"",
                               "quantile"))
             ## 'mx' has no zero-length dimensions
             if (any(dim == 0L))
                 return(gettextf("'%s' has dimension with length %d",
                                 "mx", 0L))
             for (name in c("mx", "ax")) {
                 value <- slot(object, name)
                 ## 'mx', 'ax' do not have missing values
                 if (any(is.na(value)))
                     return(gettextf("'%s' has missing values",
                                     name))
                 ## 'mx', 'ax' do not have negative values
                 if (any(value < 0))
                     return(gettextf("'%s' has negative values",
                                     name))
             }
             ## all values of 'ax' are within range implied by metadata for age
             i.age <- match("age", dimtypes)
             DimScale.age <- DimScales[[i.age]]
             dv.age <- DimScale.age@dimvalues
             nx <- diff(dv.age)
             .Data.ax <- ax@.Data
             index <- slice.index(.Data.ax, MARGIN = i.age)
             for (i in seq_along(nx)) {
                 if (any(.Data.ax[index == i] > nx[i]))
                     stop(gettextf("'some values of '%s' are larger than the width of the age group",
                                   "ax"))
             }
             ## check 'showFun'
             return.value <- tryCatch(checkShowFun(showFun),
                                      error = function(e) e)
             if (methods::is(return.value, "error"))
                 return(return.value$message)
             ## check 'radix'
             return.value <- tryCatch(checkAndTidyRadix(radix),
                                      error = function(e) e)
             if (methods::is(return.value, "error"))
                 return(return.value$message)
             ## check 'showQuantiles'
             return.value <- tryCatch(checkShowQuantiles(showQuantiles),
                                      error = function(e) e)
             if (methods::is(return.value, "error"))
                 return(return.value$message)
             ## check 'prob'
             return.value <- tryCatch(checkAndTidyProb(prob),
                                      error = function(e) e)
             if (methods::is(return.value, "error"))
                 return(return.value$message)
             TRUE
         })

## NO_TESTS
setClass("SummaryLifeTable",
         slots = c(dimensions = "matrix",
                   showFun = "character",
                   radix = "character",
                   showQuantiles = "character",
                   prob = "numeric"),
         validity = function(object) {
             dimensions <- object@dimensions
             showFun <- object@showFun
             radix <- object@radix
             showQuantiles <- object@showQuantiles
             prob <- object@prob
             ## 'dimensions' has type "character"
             if (!is.character(dimensions))
                 return(gettextf("'%s' does not have type \"%s\"",
                                 "dimensions", "character"))
             ## check 'showFun'
             return.value <- tryCatch(checkShowFun(showFun),
                                      error = function(e) e)
             if (methods::is(return.value, "error"))
                 return(return.value$message)
             ## check 'radix'
             return.value <- tryCatch(checkAndTidyRadix(radix),
                                      error = function(e) e)
             if (methods::is(return.value, "error"))
                 return(return.value$message)
             ## check 'showQuantiles'
             return.value <- tryCatch(checkShowQuantiles(showQuantiles),
                                      error = function(e) e)
             if (methods::is(return.value, "error"))
                 return(return.value$message)
             ## check 'prob'
             return.value <- tryCatch(checkAndTidyProb(prob),
                                      error = function(e) e)
             if (methods::is(return.value, "error"))
                 return(return.value$message)
             TRUE
         })

