

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
#' al <- dembase::Values(al)
#' mx <- dembase::subarray(al,
#'                subarray = (fun == "mx") & (time == "2001-2005"))
#' lt <- LifeTable(mx)
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
#' survive to exact age \code{x} \cr
#'   \code{dx} \tab The number of deaths in the age interval, for a cohort of
#' size \code{radix} at birth. \cr
#'   \code{Lx} \tab The expected number of person-years lived in an age interval
#' by a cohort of size \code{radix} at birth. \cr
#'   \code{Tx} \tab Sum of the \code{Lx} from age \code{x} to the highest age. \cr
#'   \code{ex} \tab Life expectancy at age \code{x}. \cr
#'   \code{ax} \tab The 'separation factor' - the average number of years
#'     lived during an age interval by someone who does during that interval. \cr
#' }
#'
#' The \code{fun} argument in calls to \code{lifeTableFun} does not need to be
#' one of the functions specified in the \code{showFun}
#' argument to \code{\link{LifeTable}}.
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
#' al <- dembase::Values(al)
#' mx <- dembase::subarray(al,
#'                subarray = (fun == "mx") & (time == "2001-2005"))
#' lt <- LifeTable(mx)
#' lt
#' lifeTableFun(lt, fun = "qx")
#' lifeTableFun(lt, fun = "Lx")
#' ## Because it is not included in the default value for 'showFun'
#' ## in function 'LifeTable', "Tx" is not displayed in the
#' ## life table above.  However, 'lifeTableFun' will generate
#' ## a value for 'Tx' anyway.
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
#' @examples
#' mx <- dembase::ValuesOne(c(0.2, 0.05, 0.1, 0.4),
#'                labels = c("0", "1-4", "5-9", "10+"),
#'                name = "age")
#' mx <- dembase::perturb(1000 * mx, n = 20) / 1000
#' lt <- LifeTable(mx)
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
#' \code{radix} must be a positive number, and defaults to
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
#' @examples
#' mx <- dembase::ValuesOne(c(0.2, 0.05, 0.1, 0.4),
#'                labels = c("0", "1-4", "5-9", "10+"),
#'                name = "age")
#' lt <- LifeTable(mx)
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
#' that life table functions are displayed, as well as output via functions
#' such as \code{\link[=as.data.frame.LifeTable]{as.data.frame}}. However it
#' does not affect the underlying data contained in the
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
#' @examples
#' mx <- dembase::ValuesOne(c(0.2, 0.05, 0.1, 0.4),
#'                labels = c("0", "1-4", "5-9", "10+"),
#'                name = "age")
#' lt <- LifeTable(mx)
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
#' @examples
#' mx <- dembase::ValuesOne(c(0.2, 0.05, 0.1, 0.4),
#'                labels = c("0", "1-4", "5-9", "10+"),
#'                name = "age")
#' mx <- dembase::perturb(1000 * mx, n = 20) / 1000
#' lt <- LifeTable(mx)
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


#' Whether a LifeTable objects shows a "total" category for sex
#'
#' Extract or change the \code{showTotal} slot of an object of class
#' \code{\linkS4class{LifeTable}}.  The \code{showTotal} slot controls
#' whether a "total" category is shown, in addition to "female"
#' and "male" categories.  It only has an effect if the life table
#' has a dimension with \code{\link[dembase]{dimtype}} \code{"sex"}.
#'
#' \code{showTotal} does not affect the underlying data.  A
#' \code{\linkS4class{LifeTable}} only stores "female" and "male"
#' categories; totals are generated as needed.
#'
#' @param object An object of class \code{\linkS4class{LifeTable}}.
#' @param value Logical.
#'
#' @return The extraction function returns \code{TRUE} or \code{FALSE},
#' and the replacement function returns a \code{\linkS4class{LifeTable}} object
#' with a new value for the \code{showTotal} slot.
#'
#' @seealso Life tables are created using function \code{\link{LifeTable}}.
#'
#' @examples
#' al <- demdata::afghan.life
#' al <- dembase::Values(al)
#' mx <- dembase::subarray(al,
#'                subarray = (fun == "mx") & (time == "2001-2005"))
#' lt <- LifeTable(mx)
#' lt
#' showTotal(lt)
#' showTotal(lt) <- FALSE
#' lt
#' @export
setGeneric("showTotal",
           function(object)
               standardGeneric("showTotal"))

#' @export
#' @rdname showTotal
setGeneric("showTotal<-",
           function(object, value)
               standardGeneric("showTotal<-"))


#' Calculate Sx, the probability of surviving into the next age group.
#'
#' Given a \code{\linkS4class{LifeTable}}, calculate the 'Sx', the probability
#' of surviving into the next age group.  'Sx' is similar to the life table
#' function 'Px', except that 'Sx' describes survival from one age group to
#' the next, while 'Px' describes survival from one exact age to the next.
#'
#' 'Sx' differs from other life table functions in that
#' \itemize{
#'   \item it requires all age groups (except the last) to have the same width
#' (see \code{\link[dembase]{hasRegularAgeTime}}), and
#'   \item given the same set of data, 'Sx' contains one fewer age group.
#' }
#'
#' 'Sx' is mainly used in models of population dynamics, such as population
#' projections, though it occasionally appears in life tables.
#'
#' If \eqn{L_x} is the life table population between exact ages \eqn{x} and
#' \eqn{x+n}, then \eqn{S_x = L_{x+a} / L_x}, except for the second-to-last
#' age group, where \eqn{S_x = (L_{x+a} + L_x) / L_x}.  (\eqn{S_x} is not
#' defined for the final age group.)  See below for an example.
#'
#' @param object a \code{\linkS4class{LifeTable}}.
#' @param useLabelStart Logical. If \code{TRUE}, the default, the
#' Sx values are labelled by age at the start of the period;
#' if \code{FALSE}, Sx values are labelled by age at the
#' end of the period.
#'
#' @return An object of class \code{\link[dembase]{Values}}.
#' 
#' @seealso To make the age groups in a life table 'regular', in the sense
#' described in \code{\link[dembase]{hasRegularAgeTime}}, use function
#' \code{\link[=collapseIntervals-LifeTable]{collapseIntervals}}.
#'
#' @examples
#' mx <- dembase::ValuesOne(c(0.2, 0.05, 0.1, 0.4),
#'                labels = c("0", "1-4", "5-9", "10+"),
#'                name = "age")
#' lt <- LifeTable(mx)
#' lt
#'
#' ## calling 'Sx' on a life table with irregular age intervals
#' ## raises an error
#' \dontrun{
#' Sx(lt)
#' }
#'
#' ## collapse the intervals, and try again
#' lt <- collapseIntervals(lt, dimension = "age", width = 5)
#' lt
#' Sx(lt)
#'
#' ## use age at the end of the interval, rather than the start
#' Sx(lt, useLabelStart = FALSE)
#' @export
setGeneric("Sx",
           function(object, useLabelStart = TRUE)
               standardGeneric("Sx"))








