

## #' Decompose changes in life expectancy.
## #'
## #' NEED TO UPDATE THIS!!!
## #'
## #' Using the method proposed by Arriaga (1984), calculate the contributions of
## #' different age groups to overall changes in life expectancy.
## #'
## #' The calculations are extremely sensitive to small changes in the input
## #' values. This means, for instance, that I was not able to replicate the
## #' calculations in Preston et al 2001, pp64-65, unless I used exactly
## #' the same a0.
## #'
## #' @param obj1,obj2 Objects of class \code{\linkS4class{DemographicArray}} holding
## #' life table quantities (eg death rates, or person-years lived.)
## #' @param from The name of the life table quantities, eg \code{"mx"} or
## #' \code{"Lx"}.
## #' @param a0 The number of person-years lived by people who their first year of
## #' life.
## #' @export
## setGeneric("decompLifeExpPair",
##            function(lx1, lx2, Lx1, Lx2)
##              standardGeneric("decompLifeExpPair"))



#' Calculate life expectancy.
#'
#' Obtain life expectancies from an object of class
#' \code{\linkS4class{LifeTable}}.  By default, \code{lifeExpectancy}
#' returns life expectancy at age 0, but other ages can be used instead.
#'
#' @param object An object of class \code{\linkS4class{LifeTable}}.
#' @param age The age from which life expectancy is calculated. Defaults to 0.
#'
#' @return An object of class \code{\link[dembase]{Values}}, or, if,
#' there is only one dimension, a numeric vector.
#' 
#' @seealso Values for life expectancy can also be extracted from a
#' \code{\linkS4class{LifeTable}} object using \code{\link{lifeTableFun}},
#' though in this case all ages are shown.
#'
#' @examples
#' al <- demdata::afghan.life
#' al <- Values(al)
#' mx <- subarray(al,
#'                subarray = (fun == "mx") & (time == "2001-2005"))
#' ax <- subarray(al,
#'                subarray = (fun == "ax") & (time == "2001-2005"))
#' lt <- LifeTable(mx = mx, ax = ax)
#' lt
#' lifeExpectancy(lt)
#' lifeExpectancy(lt, age = 65)
#' @export
setGeneric("lifeExpectancy",
           function(object, age = 0)
               standardGeneric("lifeExpectancy"))




#' Extract a life table function from a life table.
#'
#' Obtain values for a life table function from an object of class
#' \code{\linkS4class{LifeTable}}.
#'
#' The life table functions are as follows:
#' \tabular{ll}{
#'   Function \tab Definition \cr
#'   \code{mx} \tab Mortality rate for age-group \code{x}. \cr
#'   \code{qx} \tab Given survival to exact age x, the probability dying
#' before the end of the age interval. \cr
#'   \code{px} \tab \code{1 - qx} \cr
#'   \code{lx} \tab Of \code{radix} births, the number of people expected to
#' survive to exact age x \code{radix} \cr
#'   \code{dx} \tab The number of deaths in the age interval, for a cohort of
#' size \code{radix} at birth. \cr
#'   \code{Lx} \tab The expected number of person-years lived in an age interval
#' by a cohort of size \code{radix} at birth. \cr
#'   \code{Tx} \tab Sum of the \code{Lx} from age \code{x} to the highest age. \cr
#'   \code{ex} \tab Life expectancy at age \code{x}. \cr
#'   \code{ax} \tab The 'separation factor' - the average number of years lived
#' during an age interval by someone who does during that interval. \cr
#' }
#'
#' \code{fun} does not need to be one of the functions specified in the
#' \code{showFun} argument to \code{\link{LifeTable}}.
#' 
#' @param object An object of class \code{\linkS4class{LifeTable}}.
#' @param fun The name of a life table function.
#'
#' @return If \code{fun} is \code{"dx"}, \code{"lx"}, \code{"Lx"}, or
#' \code{"Tx"}, an object of class \code{\link[dembase]{Counts}};
#' otherwise an object of class \code{\link[dembase]{Values}}.
#'
#' @seealso \code{\link{lifeExpectancy}} is a convenience function for
#' extracting life expectancy at a particular age.
#'
#' @examples
#' al <- demdata::afghan.life
#' al <- Values(al)
#' mx <- subarray(al,
#'                subarray = (fun == "mx") & (time == "2001-2005"))
#' ax <- subarray(al,
#'                subarray = (fun == "ax") & (time == "2001-2005"))
#' lt <- LifeTable(mx = mx, ax = ax)
#' lt
#' lifeTableFun(lt, fun = "qx")
#' lifeTableFun(lt, fun = "Lx")
#' ## Because it is not included in \code{\link{showFun}}, \code{"Tx"}
#' ## is not displayed.  But 'lifeTableFun' will work anyway.
#' lifeTableFun(lt, fun = "Tx")
#' @export
setGeneric("lifeTableFun",
           function(object, fun = c("mx", "qx", "px", "dx", "lx",
                                    "Lx", "Tx", "ex"))
               standardGeneric("lifeTableFun"))


#' Quantiles used by a life table.
#'
#' Extract or change the \code{prob} slot of an object of class
#' \code{\linkS4class{LifeTable}}.  The \code{prob} slot specifies the
#' quantiles used in displays, in a life table that has a dimension with
#' \code{\link[dembase]{dimtype}} \code{"iterations"}.
#'
#' \code{prob} does not affect the underlying data, which are always
#' stored as iterations, rather than quantiles.
#'
#' If \code{object} does not have a dimension with dimtype \code{"iterations"},
#' then \code{prob} has no effect.
#'
#' The quantiles must be between 0 and 1 (inclusive) and must be ordered
#' from smallest to largest.
#'
#' @param object An object of class \code{\linkS4class{LifeTable}}.
#' @param value A numeric vector.
#'
#' @return The extraction function returns a numeric vector,
#' and the replacement function returns a \code{\linkS4class{LifeTable}} object
#' with a new value for the \code{prob} slot.
#'
#' @seealso Life tables are created using function \code{\link{LifeTable}}.
#'
#' mx <- ValuesOne(c(0.2, 0.05, 0.1, 0.4),
#'                labels = c("0", "1-4", "5-9", "10+"),
#'                name = "age")
#' ax <- ValuesOne(c(0.3, 1, 2.5, 2.5),
#'                 labels = c("0", "1-4", "5-9", "10+"),
#'                name = "age")
#' mx <- perturb(1000 * mx, n = 20) / 1000
#' lt <- LifeTable(mx = mx, ax = ax)
#' lt
#' prob(lt)
#' prob(lt) <- c(0.1, 0.9)
#' lt
#' @export
setGeneric("prob",
           function(object)
               standardGeneric("prob"))

#' @export
#' @rdname prob
setGeneric("prob<-",
           function(object, value)
               standardGeneric("prob<-"))


#' Get or set the radix of a life table.
#'
#' Extract or change the 'radix' of an object of class
#' \code{\linkS4class{LifeTable}}.  The radix of a life table is the value for
#' \code{l0}, the first entry in the \code{lx} column.  As well as affecting
#' \code{lx}, changing the radix affects \code{dx}, \code{Lx}, and \code{Tx}.
#'
#' The radix \code{radix} must be a positive number, and defaults to
#' 100,000.
#'
#' @param object An object of class \code{\linkS4class{LifeTable}}.
#' @param value A positive number.
#'
#' @return The extraction function returns a number and the replacement
#' function returns a \code{\linkS4class{LifeTable}} object with a
#' new radix.
#'
#' @seealso Life tables are created using function \code{\link{LifeTable}}.
#'
#' mx <- ValuesOne(c(0.2, 0.05, 0.1, 0.4),
#'                labels = c("0", "1-4", "5-9", "10+"),
#'                name = "age")
#' ax <- ValuesOne(c(0.3, 1, 2.5, 2.5),
#'                 labels = c("0", "1-4", "5-9", "10+"),
#'                name = "age")
#' lt <- LifeTable(mx = mx, ax = ax)
#' radix(lt)
#' radix(lt) <- 1000
#' lt
#' @export
setGeneric("radix",
           function(object)
               standardGeneric("radix"))

#' @export
#' @rdname radix
setGeneric("radix<-",
           function(object, value)
               standardGeneric("radix<-"))


#' Get or set the life table functions shown by default by a life table.
#'
#' Extract or change the \code{showFun} slot of an object of class
#' \code{\linkS4class{LifeTable}}.  The \code{showFun} slot controls the way
#' that life table functions are displayed, or output via functions
#' such as \code{\link[=as.data.frame-LifeTable]{as.data.frame}}, but does
#' not affect the underlying data contained in the
#' \code{\linkS4class{LifeTable}} object.
#'
#' See the documentation for \code{\link{lifeTableFun}} for a list of the
#' valid life table functions.
#'
#' @param object An object of class \code{\linkS4class{LifeTable}}.
#' @param value A character vector with names of life table functions.
#'
#' @return The extraction function returns a character vector and the
#' replacement function returns a \code{\linkS4class{LifeTable}} object
#' with a new value for the \code{showFun} slot.
#'
#' @seealso Life tables are created using function \code{\link{LifeTable}}.
#'
#' mx <- ValuesOne(c(0.2, 0.05, 0.1, 0.4),
#'                labels = c("0", "1-4", "5-9", "10+"),
#'                name = "age")
#' ax <- ValuesOne(c(0.3, 1, 2.5, 2.5),
#'                 labels = c("0", "1-4", "5-9", "10+"),
#'                name = "age")
#' lt <- LifeTable(mx = mx, ax = ax)
#' lt
#' showFun(lt)
#' showFun(lt) <- c("ex", "lx")
#' lt
#' @export
setGeneric("showFun",
           function(object)
               standardGeneric("showFun"))

#' @export
#' @rdname showFun
setGeneric("showFun<-",
           function(object, value)
               standardGeneric("showFun<-"))


#' Whether a LifeTable objects shows quantiles.
#'
#' Extract or change the \code{showQuantiles} slot of an object of class
#' \code{\linkS4class{LifeTable}}.  The \code{showQuantiles} slot controls
#' whether all iterations are shown, or just quantiles summarising those
#' iterations, in a life table that has a dimension with
#' \code{\link[dembase]{dimtype}} \code{"iterations"}.
#'
#' \code{showQuantiles} does not affect the underlying data, which are always
#' stored as iterations, rather than quantiles.
#'
#' If \code{object} does not have a dimension with dimtype \code{"iterations"},
#' then \code{showQuantiles} has no effect.
#'
#' @param object An object of class \code{\linkS4class{LifeTable}}.
#' @param value Logical.
#'
#' @return The extraction function returns \code{TRUE} or \code{FALSE},
#' and the replacement function returns a \code{\linkS4class{LifeTable}} object
#' with a new value for the \code{showQuantiles} slot.
#'
#' @seealso Life tables are created using function \code{\link{LifeTable}}.
#'
#' mx <- ValuesOne(c(0.2, 0.05, 0.1, 0.4),
#'                labels = c("0", "1-4", "5-9", "10+"),
#'                name = "age")
#' ax <- ValuesOne(c(0.3, 1, 2.5, 2.5),
#'                 labels = c("0", "1-4", "5-9", "10+"),
#'                name = "age")
#' mx <- perturb(1000 * mx, n = 20) / 1000
#' lt <- LifeTable(mx = mx, ax = ax)
#' lt
#' showQuantiles(lt)
#' showQuantiles(lt) <- FALSE
#' lt
#' @export
setGeneric("showQuantiles",
           function(object)
               standardGeneric("showQuantiles"))

#' @export
#' @rdname showQuantiles
setGeneric("showQuantiles<-",
           function(object, value)
               standardGeneric("showQuantiles<-"))










