
#' Create a LifeTable object.
#'
#' Create an object of class \code{\linkS4class{LifeTable}} holding the
#' information necessary to create the full suite of life table functions.
#'
#' For definitions of the life table functions, see the documentation for
#' \code{\link{lifeTableFun}}.
#'
#' \code{mx} is a \code{\link[dembase:DemographicArray-class]{Values}}
#' object holding estimates of mortality rates, disaggregated by 
#'
#' \code{ax} specifies "separation factors", that is, the average amount of time
#' lived during an age interval by people who die during that interval.
#' \code{ax} can not include dimensions that \code{object} does not.  However,
#' \code{object} \emph{can} include dimensions that \code{ax} does not.  If it
#' does, then values for \code{ax} are assumed to be identical across these
#' dimensions. For instance, if \code{object} has a \code{"region"} dimension,
#' but \code{ax} does not, then the same values for \code{ax} are used for all
#' regions. See below for examples.
#'
#' When \code{mx} and \code{ax} share a dimension, all values for that
#' dimension that appear in \code{mx} must also appear in \code{ax}--except
#' for the age dimension.  If \code{mx} includes an age interval that
#' \code{ax} does not, then \code{ax} for that value is assumed to be
#' half the width of the age interval.
#'
#' If \code{ax} is not supplied, then \code{LifeTable} imputes values.
#' The imputed values equal half the length of the corresponding age group,
#' except at ages 0 and 1-4.  If \code{mx} includes rates for age 0,
#' then \code{LifeTables} obtains \code{ax} values for ages 0 and 1-4
#' by applyin the formulas provided by Preston et al (2001: Table 3.3).
#'
#' The arguments \code{showFun}, \code{showQuantiles} and \code{prob}
#' affect printing, plotting, and
#' \code{\link[=as.data.frame.LifeTable]{as.data.frame}}, but do not affect
#' the underlying data.  This means, for instance, that \code{lifeTableFun} can
#' be produce values for life table functions that are not included in
#' \code{showFun}.
#'
#' \code{l0}, the first value of \code{lx} is conventionally set to 100,000.
#' Alternative values can be supplied via the \code{radix} argument.  Changing
#' the value of \code{radix} also affects the \code{dx}, \code{Lx} and
#' \code{Tx} columns.
#'
#' @param mx An object of class
#' \code{\link[dembase:DemographicArray-class]{Values}} holding estimated
#' mortality rates.
#' @param ax An object of class 
#' \code{\link[dembase:DemographicArray-class]{Values}} holding estimated
#' separation factors. Optional.
#' @param showFun A character vector with the names of the functions that
#' are printed or plotted by default
#' @param radix A positive number.  Defaults to 100,000.
#' @param showQuantiles  Logical.  If \code{TRUE}, the default,
#' quantiles are shown, rather than iterations.  
#' @param showTotal  Logical.  If \code{TRUE}, the default,
#' and if there is a dimension with \code{\link[dembase:dimtype]{dimtype}}
#' \code{"sex"}, then a "total" category is shown, in addition to
#' "female" and "male" cateogies.
#' @param prob Values used to calculate quantiles.  Passed to function
#' \code{\link[stats]{quantile}}.
#'
#' @return An object of class \code{\linkS4class{LifeTable}}.
#' 
#' @seealso \code{\link{lifeTableFun}} returns values for a life table
#' function.  \code{\link[=as.data.frame.LifeTable]{as.data.frame}}
#' converts a \code{LifeTable} object to a (long form) data.frame,
#' typically as a step towards exporting the life table to a .csv file.
#'
#' @examples
#' mx <- dembase::ValuesOne(c(0.2, 0.05, 0.1, 0.4),
#'                 labels = c("0", "1-4", "5-9", "10+"),
#'                 name = "age")
#' LifeTable(mx)
#' LifeTable(mx, showTotal = TRUE)
#'
#' al <- demdata::afghan.life
#' al <- dembase::Values(al)
#' mx <- dembase::subarray(al,
#'                subarray = (fun == "mx") & (time == "2001-2005"))
#' ax <- dembase::subarray(al,
#'                subarray = (fun == "ax") & (time == "2001-2005"))
#' LifeTable(mx = mx, ax = ax)
#' @export
LifeTable <- function(mx, ax = NULL,
                      showFun = c("mx", "qx", "dx", "lx", "Lx", "ex"),
                      radix = 100000, showQuantiles = TRUE, showTotal = FALSE,
                      prob = c(0.025, 0.5, 0.975)) {
    if (!methods::is(mx, "Values"))
        stop(gettextf("'%s' has class \"%s\"",
                      "mx", class(mx)))
    if (any(dembase::dimtypes(mx) == "origin"))
        stop("cannot handle orig-dest yet")
    if (!methods::is(ax, "Values") && !is.null(ax))
        stop(gettextf("'%s' has class \"%s\"",
                      "ax", class(ax)))
    .Data.mx <- mx@.Data
    metadata.mx <- mx@metadata
    dim.mx <- dim(.Data.mx)
    dimtypes.mx <- dembase::dimtypes(metadata.mx,
                                     use.names = FALSE)
    i.age <- match("age", dimtypes.mx, nomatch = 0L)
    has.age <- i.age > 0L
    if (!has.age)
        stop(gettextf("'%s' does not have a dimension with %s \"%s\"",
                      "mx", "dimtype", "age"))
    dembase::checkAge(object = mx,
                      minAges = 2L,
                      regular = FALSE,
                      openLeftOK = FALSE,
                      openRightOK = TRUE,
                      expectedDimscale = "Intervals")
    checkLifeTableMetaData(mx)
    if (is.null(ax))
        ax <- dembase::makeAxStart(mx)
    ax <- dembase::expandAx(ax = ax,
                            object = mx)
    checkShowFun(showFun)
    radix <- checkAndTidyRadix(radix)
    checkShowQuantiles(showQuantiles)
    checkShowTotal(showTotal)
    prob <- checkAndTidyProb(prob)
    checkLifeTableInputValues(object = mx,
                              from = "mx",
                              radix = radix)
    checkLifeTableInputValues(object = ax,
                              from = "ax",
                              radix = radix)
    n.dim <- length(dim.mx)
    if (i.age != 1L) {
        s <- seq_len(n.dim)
        perm <- c(i.age, s[-i.age])
        mx <- aperm(mx, perm = perm)
        ax <- aperm(ax, perm = perm)
    }
    methods::new("LifeTable",
                 mx = mx,
                 ax = ax,
                 showFun = showFun,
                 radix = radix,
                 showQuantiles = showQuantiles,
                 showTotal = showTotal,
                 prob = prob)
}    

