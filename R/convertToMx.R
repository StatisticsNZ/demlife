#' Convert life table function into mortality rates.
#'
#' Given the column of a life table (eg 'qx'), calculate the implied mortality
#' rates ('mx').
#'
#' \code{convertToMx} is needed when calculating a life table from values
#' other than 'mx', since function \code{\link{LifeTable}} constructs the
#' life table from 'mx' values.
#'
#' \code{object} must have a dimension with \code{\link[dembase]{dimtype}}
#' \code{"age"}.  \code{from} must be one of  \code{"qx"}, \code{"px"},
#' \code{"dx"}, \code{"lx"}, \code{"Lx"}, \code{"Tx"}, or \code{"ex"}.
#' See \code{\link{LifeTable}} for definitions of these functions.
#'
#' Demographers typically obtain the 'lx' column of a life table by
#' multiplying the probability of surviving to age 'x' by a 'radix', usually
#' 100000. The 'dx', 'Lx', and 'Tx' columns are also scaled by the 'radix'.
#'
#' See the documentation for \code{\link{LifeTable}} for a discussion of
#' how to specify separation factor \code{ax}.
#' 
#' Life table functions \code{qx}, \code{px}, \code{dx}, \code{lx},
#' effectively contain no information about mortality conditions in the final
#' age group if, as is normally the case, the final age group is open (that
#' is, has no upper limit.) With \code{qx}, the final value is always
#' 1.  With \code{px} the final value is always 0.  With \code{dx} the
#' final value is always the radix minus deaths in earlier age groups.  With
#' \code{lx}, the final value describes mortality up to age \code{x}, but
#' says nothing about mortality beyond age \code{x}. (In contrast to the
#' other life table functions, the first value of \code{lx} contains
#' no information, since it always equals \code{radix}.)
#'
#' Because life table functions \code{qx}, \code{px}, \code{dx}, and \code{lx},
#' contain no information on the final age group, when \code{from} is one of
#' these functions, the return value from \code{convertToMx} drops the final
#' age group.  See below for an example.
#'
#' @param object An object of class
#' \code{\link[dembase:DemographicArray-class]{DemographicArray}}.
#' @param from The name of a life table function.
#' @param ax An object of class
#' \code{\link[dembase:DemographicArray-class]{DemographicArray}}
#' specifying 'separation factors'.
#' @param radix A positive number, defaulting to 100000.
#'
#' @return An object of class
#' \code{\link[dembase:DemographicArray-class]{Values}}.
#'
#' @seealso \code{convertToMx} may be needed to prepare the \code{mx}
#' argument for function \code{\link{LifeTable}}.
#'
#' @references Preston S, Heuveline P, Guillot M. 2000. \emph{Demography:
#' Measuring and Modeling Population Processes}. Blackwell.
#' @examples
#' Lx <- Counts(array(c(1800000, 1650000, 1500000, 900000, 100000,
#'                      1700000, 1500000, 1400000, 800000,  50000),
#'                    dim = c(5, 2),
#'                    dimnames = list(age = c("0-19", "20-39", "40-59",
#'                                            "60-79", "80+"),
#'                                    sex = c("Female", "Male"))))
#' ax <- Values(array(c(8.3, 5),
#'                    dim = c(1, 2),
#'                    dimnames = list(age = "0-19",
#'                                    sex = c("Female", "Male"))))
#' convertToMx(Lx, from = "Lx", ax = ax)
#'
#'
#' ax <- ValuesOne(3, labels = "0-19", name = "age")
#'
#' lx <- CountsOne(c(100000, 90000, 70000, 30000, 5000),
#'                 labels = c(0, 20, 40, 60, 80),
#'                 name = "age")
#' ax <- ValuesOne(5, labels = "0-19", name = "age")
#' convertToMx(lx, from = "lx", ax = ax)
#'
#' qx <- ValuesOne(c(0.1, 0.05, 0.15, 0.25, 1),
#'                 labels = c("0-19", "20-39", "40-59", "60-79", "80+"),
#'                 name = "age")
#' ax <- ValuesOne(5, labels = "0-19", name = "age")
#' convertToMx(qx, from = "qx", ax = ax)
#' @export
convertToMx <- function(object, from = c("qx", "px", "dx", "lx", "Lx", "Tx", "ex"),
                        ax, radix = 100000) {
    kUsesRadix <- c("dx", "lx", "Lx", "Tx")
    kLastAgeIntervalUninformative <- c("qx", "px", "dx") # 'lx' is not interval
    if (!methods::is(object, "DemographicArray"))
        stop(gettextf("'%s' has class \"%s\"",
                      "object", class(object)))
    if (!methods::is(ax, "Values"))
        stop(gettextf("'%s' has class \"%s\"",
                      "ax", class(ax)))
    if (any(dembase::dimtypes(object) == "origin"))
        stop("cannot handle orig-dest yet")
    from <- match.arg(from)
    .Data.obj <- object@.Data
    metadata.obj <- object@metadata
    dim.obj <- dim(.Data.obj)
    dimtypes.obj <- dembase::dimtypes(object, use.names = FALSE)
    n.dim.obj <- length(dim.obj)
    i.age <- match("age", dimtypes.obj, nomatch = 0L)
    has.age <- i.age > 0L
    if (!has.age)
        stop(gettextf("'%s' does not have a dimension with %s \"%s\"",
                      "object", "dimtype", "age"))
    expected.dimscale <- getLifeTableDimScale(from)
    dembase::checkAge(object = object,
                      minAges = 2L,
                      regular = FALSE,
                      openLeftOK = FALSE,
                      openRightOK = TRUE,
                      expectedDimscale = expected.dimscale)
    checkLifeTableMetaData(object)
    radix <- checkAndTidyRadix(radix)
    checkLifeTableInputValues(object = object,
                              from = from,
                              radix = radix)
    metadata.ans <- makeLifeTableMetaData(metadata = metadata.obj,
                                          from = from,
                                          to = "mx")
    dim.ans <- dim(metadata.ans)
    dimnames.ans <- dimnames(metadata.ans)
    .Data.template <- array(0,
                            dim = dim.ans,
                            dimnames = dimnames.ans)
    template.ans <- methods::new("Values",
                                 .Data = .Data.template,
                                 metadata = metadata.ans)
    ax <- dembase::expandAx(ax = ax,
                            object = template.ans)
    .Data.ax <- ax@.Data
    if (n.dim.obj > 1L) {
        s <- seq_len(n.dim.obj)
        perm <- c(i.age, s[-i.age])
        .Data.obj <- aperm(.Data.obj, perm = perm)
        .Data.ax <- aperm(.Data.ax, perm = perm)
    }
    n.age.obj <- dim.obj[i.age]
    n.age.ans <- dim.ans[i.age]
    .Data.obj <- matrix(.Data.obj, nrow = n.age.obj)
    .Data.ax <- matrix(.Data.ax, nrow = n.age.ans)
    DimScales.ans <- dembase::DimScales(metadata.ans,
                                        use.names = FALSE)
    DimScale.age.ans <- DimScales.ans[[i.age]]
    dv.age.ans <- DimScale.age.ans@dimvalues
    nx <- diff(dv.age.ans)
    open <- is.infinite(nx[n.age.ans])
    nx <- matrix(nx,
                 nrow = n.age.ans,
                 ncol = ncol(.Data.obj))
    if (from %in% kUsesRadix)
        .Data.obj <- .Data.obj / radix
    if (from %in% kLastAgeIntervalUninformative)
        .Data.obj <- .Data.obj[-n.age.obj, ]
    .Data.ans <- convertLifeTableFun(.Data = .Data.obj,
                                     from = from,
                                     to = "mx",
                                     nx = nx,
                                     ax = .Data.ax,
                                     open = open,
                                     mA = NULL)
    if (n.dim.obj > 1L) {
        .Data.ans <- array(.Data.ans,
                           dim = c(n.age.ans, dim.obj[-i.age]))
        .Data.ans <- aperm(.Data.ans,
                           perm = match(s, perm))
    }
    .Data.ans <- array(.Data.ans,
                       dim = dim.ans,
                       dimnames = dimnames.ans)
    methods::new("Values",
                 .Data = .Data.ans,
                 metadata = metadata.ans)
}
    
