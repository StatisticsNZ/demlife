
#' Coerce a life table to a data frame.
#'
#' Convert a \code{\linkS4class{LifeTable}} object to a "long form" data frame.
#' The life table functions that are included in the data frame are controlled
#' by the \code{\link{showFun}} slot of the life table.  If the life table
#' has iterations, the use of quantiles to summarise
#' these iterations is controlled by the \code{\link{showQuantiles}} and
#' \code{\link{prob}} slots.
#'
#' \code{as.data.frame} is often called as a step towards exporting
#' a life table as a .csv file.
#'
#' \code{as.data.frame} prints the dimensions of the life table in reverse
#' order, with the \code{"age"} dimension last.  This ordering is different
#' from the one used by base function \code{\link[base]{as.data.frame.array}},
#' but, with life tables, is usually what is wanted.
#' 
#' @param x An \code{\linkS4class{LifeTable}} object.
#' @param row.names See \code{\link[base]{as.data.frame}}.
#' @param optional See \code{\link[base]{as.data.frame}}.
#' @param stringsAsFactors See \code{\link[base]{as.data.frame}}.
#' @param responseName The name of the final column of the table.  Defaults to
#' \code{"value"}
#' @param \dots Additional arguments to be passed to or from methods.
#'
#' @return A \code{\link[base]{data.frame}}.
#'
#' @seealso Life tables are created by function \code{\link{LifeTable}}.
#'
#' @examples
#' mx <- ValuesOne(c(0.2, 0.05, 0.1, 0.4),
#'                labels = c("0", "1-4", "5-9", "10+"),
#'                name = "age")
#' lt <- LifeTable(mx)
#' lt
#' as.data.frame(lt)
#' 
#' showFun(lt) <- c("ex", "Tx")
#' as.data.frame(lt)
#'
#'
#' ## life table with iterations
#' mx <- ValuesOne(c(0.2, 0.05, 0.1, 0.4),
#'                labels = c("0", "1-4", "5-9", "10+"),
#'                name = "age")
#' mx <- perturb(1000 * mx, n = 10) / 1000
#' lt <- LifeTable(mx)
#' lt
#' as.data.frame(lt)
#' showQuantiles(lt) <- FALSE
#' lt
#' as.data.frame(lt)
#' showQuantiles(lt) <- TRUE
#' prob(lt) <- c(0.1, 0.8)
#' lt
#' as.data.frame(lt)
#' @name as.data.frame-LifeTable
NULL

#' @rdname as.data.frame-LifeTable
#' @export
as.data.frame.LifeTable <- function(x, row.names = NULL, optional = FALSE,
                                    stringsAsFactors = default.stringsAsFactors(),
                                    responseName = "value",
                                    ... ) {
    mult.by.radix <- getFunMultByRadix()
    mx <- x@mx
    ax <- x@ax
    radix <- x@radix
    showFun <- x@showFun
    showQuantiles <- x@showQuantiles
    showTotal <- x@showTotal
    prob <- x@prob
    if (showTotal) {
        l <- addTotalCategory(mx = mx,
                              ax = ax)
        mx <- l$mx
        ax <- l$ax
    }
    dimtypes <- dimtypes(mx, use.names = FALSE)
    life.table.funs <- calculateLifeTableFuns(mx = mx,
                                              ax = ax,
                                              radix = radix,
                                              funs = showFun,
                                              ltFunSecond = FALSE)
    has.iter <- "iteration" %in% dimtypes
    if (has.iter) {
        if (showQuantiles)
            life.table.funs <- dembase::collapseIterations(life.table.funs,
                                                           prob = prob)
    }
    if (radix > 1) {
        dimension <- length(dim(life.table.funs))
        for (fun in showFun) {
            if (fun %in% mult.by.radix) {
                value <- dembase::slab(life.table.funs,
                                       dimension = dimension,
                                       elements = fun)
                rounded.value <- round(value)
                dembase::slab(life.table.funs,
                              dimension = dimension,
                              elements = fun) <- rounded.value
            }
        }
    }
    ans <- dembase::as.data.frame(life.table.funs,
                                  row.names = row.names,
                                  optional = optional,
                                  stringsAsFactors = stringsAsFactors,
                                  responseName = responseName,
                                  direction = "long",
                                  ...)
    ncol <- length(ans)
    s <- c(rev(seq_len(ncol - 1L)), ncol)
    ans[s]
}

#' @rdname as.data.frame-LifeTable
#' @export
setMethod("as.data.frame",
          signature(x = "LifeTable"),
          as.data.frame.LifeTable)


#' Collapse intervals in a life table.
#'
#' Collapse intervals in an object of class \code{\linkS4class{LifeTable}}.
#' The intervals are typically age intervals, that is, age groups.
#' However, other intervals, other types of intervals, such as time periods,
#' can be collapsed instead.  See the documentation for
#' function \code{\link[dembase]{collapseIntervals}}
#' in package \code{dembase} for more information on collapsing intervals.
#'
#' In typical use, no \code{weights} argument is supplied.  Instead,
#' \code{collapseIntervals} calculates \code{Lx} values for the life table,
#' values and uses them as weights.
#' 
#' @param object Object of class \code{\linkS4class{DemographicArray}}.
#' @param dimension Name or index of the dimension where the intervals are
#' found.
#' @param breaks Numeric vector giving the breaks between intervals after the
#' merging has occurred.
#' @param width The length of the intervals after the merging has occurred.
#' @param old The labels of the intervals to be merged.
#' @param weights Object of class \code{\linkS4class{Counts}}. Optional.
#' @param \dots Not currently used.
#'
#' @return A \code{\linkS4class{LifeTable}}
#'
#' @seealso Life tables are created by function \code{\link{LifeTable}}.
#' 
#' @examples
#' mx <- ValuesOne(c(0.2, 0.05, 0.1, 0.4),
#'                labels = c("0", "1-4", "5-9", "10+"),
#'                name = "age")
#' lt <- LifeTable(mx)
#' lt
#' collapseIntervals(lt, dimension = "age", width = 5)
#' collapseIntervals(lt, dimension = "age", breaks = c(1, 10))
#' collapseIntervals(lt, dimension = "age", old = c("0", "1-4"))
#'
#' popn <- CountsOne(c(10, 80, 120, 200),
#'                   labels = c("0", "1-4", "5-9", "10+"),
#'                   name = "age")
#' collapseIntervals(lt, dimension = "age", width = 5, weights = popn)
#' @name collapseIntervals-LifeTable
NULL

## HAS_TESTS
#' @rdname collapseIntervals-LifeTable
#' @export
setMethod("collapseIntervals",
          signature(object = "LifeTable",
                    weights = "missing"),
          function(object, dimension, breaks = NULL, width = NULL, old = NULL, 
                   weights, ...) {
              mx <- object@mx
              ax <- object@ax
              Lx <- makeLx(mx = mx,
                           ax = ax)
              callGeneric(object = object,
                          dimension = dimension,
                          breaks = breaks,
                          width = width,
                          old = old,
                          weights = Lx,
                          ...)
          })

## HAS_TESTS
#' @rdname collapseIntervals-LifeTable
#' @export
setMethod("collapseIntervals",
          signature(object = "LifeTable",
                    weights = "Counts"),
          function(object, dimension, breaks = NULL, width = NULL, old = NULL, 
                   weights, ...) {
              mx <- object@mx
              ax <- object@ax
              radix <- object@radix
              showFun <- object@showFun
              showQuantiles <- object@showQuantiles
              showTotal <- object@showTotal
              prob <- object@prob
              mx <- collapseIntervals(object = mx,
                                      dimension = dimension,
                                      breaks = breaks,
                                      width = width,
                                      old = old,
                                      weights = weights)
              ax <- collapseIntervals(object = ax,
                                      dimension = dimension,
                                      breaks = breaks,
                                      width = width,
                                      old = old,
                                      weights = weights)
              new("LifeTable",
                  mx = mx,
                  ax = ax,
                  radix = radix,
                  showFun = showFun,
                  showQuantiles = showQuantiles,
                  showTotal = showTotal,
                  prob = prob)
          })
              

## ## NO_TESTS
## #' @rdname decompLifeExpPair
## #' @export
## setMethod("decompLifeExpPair",
##           signature(lx1 = "DemographicArray",
##                     lx2 = "DemographicArray",
##                     Lx1 = "DemographicArray",
##                     Lx2 = "DemographicArray"),
##           function(lx1, lx2, Lx1, Lx2) {
##               checkLifeTableInputValues(lx1, from = "lx")
##               checkLifeTableInputValues(lx2, from = "lx")
##               checkLifeTableInputValues(Lx1, from = "Lx")
##               checkLifeTableInputValues(Lx2, from = "Lx")
##               names.lx1 <- names(lx1)
##               dim.lx1 <- dim(lx1)
##               dimtypes.lx1 <- dimtypes(lx1, use.names = FALSE)
##               DimScales.lx1 <- DimScales(lx1, use.names = FALSE)
##               for (name in c("lx2", "Lx1", "Lx2")) {
##                   value <- get(name)
##                   if (!setequal(names.lx1, names(value)))
##                       stop(gettextf("'%s' and '%s' have different dimension names",
##                                     "lx1", name))
##                   value <- aperm(value, perm = names.lx1)
##                   if (!identical(dim.lx1, dim(value)))
##                       stop(gettextf("'%s' and '%s' have different dimensions",
##                                     "lx1", name))
##                   dimtypes.val <- dimtypes(value, use.names = FALSE)
##                   DimScales.val <- DimScales(value, use.names = FALSE)
##                   for (i in seq_along(names.lx1)) {
##                       dimtype.lx1 <- dimtypes.lx1[i]
##                       dimtype.val <- dimtypes.val[i]
##                       if (!identical(dimtype.lx1, dimtype.val))
##                           stop(gettextf("'%s' dimensions of '%s' and '%s' have different dimtypes",
##                                         names.lx1[i], "lx1", name))
##                       DS.lx1 <- DimScales.lx1[[i]]
##                       DS.val <- DimScales.val[[i]]
##                       if (identical(dimtype.lx1, "age")) {
##                           dv.lx1 <- DS.lx1@dimvalues
##                           dv.val <- DS.val@dimvalues
##                           dv.lx1 <- dv.lx1[is.finite(dv.lx1)]
##                           dv.val <- dv.val[is.finite(dv.val)]
##                           is.compatible <- isTRUE(all.equal(dv.lx1, dv.val))
##                       }
##                       else
##                           is.compatible <- isTRUE(all.equal(DS.lx1, DS.val))
##                       if (!is.compatible)
##                           stop(gettextf("'%s' dimensions of '%s' and '%s' have incompatible dimscales",
##                                         names.lx1[i], "lx", name))
##                   }
##                   assign(name, value)
##               }
##               Tx1 <- convertLifeTableFun(Lx1, from = "Lx", to = "Tx")
##               Tx2 <- convertLifeTableFun(Lx2, from = "Lx", to = "Tx")
##               i.age <- match("age", dimtypes(lx1)) ## same for Lx, Tx
##               n.age <- dim(lx1)[i.age]             ## same for Lx, Tx
##               not.first.age <- slice.index(lx1, MARGIN = i.age) > 1L
##               not.last.age <- slice.index(lx1, MARGIN = i.age) < n.age
##               lx1.start <- lx1[not.last.age]
##               lx2.start <- lx2[not.last.age]
##               lx1.end <- lx1[not.first.age]
##               lx2.end <- lx2[not.first.age]
##               Tx2.end <- Tx2[not.first.age]
##               lx1 <- as.numeric(lx1)
##               lx2 <- as.numeric(lx2)
##               lx1.start <- as.numeric(lx1.start)
##               lx2.start <- as.numeric(lx2.start)
##               lx1.end <- as.numeric(lx1.end)
##               lx2.end <- as.numeric(lx2.end)
##               Lx1.scaled <- Lx1 / lx1
##               Lx2.scaled <- Lx2 / lx2
##               direct.effect <- lx1 * (Lx2.scaled - Lx1.scaled)
##               indirect.effect <- Tx2.end * ((lx1.start / lx2.start) - (lx1.end / lx2.end))
##               ans <- direct.effect
##               ans[not.last.age] <- ans[not.last.age] + as.numeric(indirect.effect)
##               ans
##           })


#' @rdname lifeExpectancy
#' @export
setMethod("lifeExpectancy",
          signature(object = "LifeTable"),
          function(object, age = 0) {
              if (!identical(length(age), 1L))
                  stop(gettextf("'%s' does not have length %d",
                                "age", 1L))
              if (!is.numeric(age))
                  stop(gettextf("'%s' is not numeric",
                                "age"))
              if (is.na(age))
                  stop(gettextf("'%s' is missing",
                                "age"))
              mx <- object@mx
              ax <- object@ax
              showTotal <- object@showTotal
              if (showTotal) {
                  l <- addTotalCategory(mx = mx,
                                        ax = ax)
                  mx <- l$mx
                  ax <- l$ax
              }
              .Data.mx <- mx@.Data
              .Data.ax <- ax@.Data
              metadata.mx <- mx@metadata
              dim <- dim(.Data.mx)
              DimScales <- DimScales(metadata.mx,
                                     use.names = FALSE)
              DimScale.age <- DimScales[[1L]]
              dv.age <- DimScale.age@dimvalues
              i.age <- match(age, dv.age, nomatch = 0L)
              if (i.age == 0L)
                  stop(gettextf("life table does not have age interval starting at %s",
                                age))
              n.age <- dim[1L]
              .Data <- matrix(.Data.mx,
                              nrow = n.age)
              .Data.ax <- matrix(.Data.ax,
                                 nrow = n.age)
              nx <- diff(dv.age)
              nx <- matrix(nx,
                           nrow = n.age,
                           ncol = ncol(.Data))
              open <- is.infinite(nx[n.age, 1L])
              mA <- .Data[n.age, ]
              metadata.ans <- makeLifeTableMetaData(metadata = metadata.mx,
                                                    from = "mx",
                                                    to = "ex")
              .Data.ans <- convertLifeTableFun(.Data = .Data,
                                               from = "mx",
                                               to = "ex",
                                               nx = nx,
                                               ax = .Data.ax,
                                               open = open,
                                               mA = mA)
              .Data.ans <- array(.Data.ans,
                                 dim = dim(metadata.ans),
                                 dimnames = dimnames(metadata.ans))
              ans <- methods::new("Values",
                                  .Data = .Data.ans,
                                  metadata = metadata.ans)
              dembase::slab(ans,
                            dimension = 1L,
                            elements = i.age)
          })

## HAS_TESTS
#' @rdname lifeTableFun
#' @export
setMethod("lifeTableFun",
          signature(object = "LifeTable"),
          function(object, fun = c("mx", "qx", "px", "dx", "lx", "Lx", "Tx", "ex")) {
              fun <- match.arg(fun)
              mx <- object@mx
              ax <- object@ax
              showTotal <- object@showTotal
              if (showTotal) {
                  l <- addTotalCategory(mx = mx,
                                        ax = ax)
                  mx <- l$mx
                  ax <- l$ax
              }
              if (fun == "mx")
                  mx
              else if (fun == "ax")
                  ax
              else {
                  .Data.mx <- mx@.Data
                  .Data.ax <- ax@.Data
                  metadata.mx <- mx@metadata
                  dim <- dim(.Data.mx)
                  DimScales <- DimScales(metadata.mx,
                                         use.names = FALSE)
                  DimScale.age <- DimScales[[1L]]
                  dv.age <- DimScale.age@dimvalues
                  class.ans <- getLifeTableClass(fun)
                  metadata.ans <- makeLifeTableMetaData(metadata = metadata.mx,
                                                        from = "mx",
                                                        to = fun)
                  n.age <- dim[1L]
                  .Data <- matrix(.Data.mx,
                                  nrow = n.age)
                  .Data.ax <- matrix(.Data.ax,
                                     nrow = n.age)
                  nx <- diff(dv.age)
                  nx <- matrix(nx,
                               nrow = n.age,
                               ncol = ncol(.Data))
                  open <- is.infinite(nx[n.age, 1L])
                  mA <- .Data[n.age, ]
                  .Data.ans <- convertLifeTableFun(.Data = .Data,
                                                   from = "mx",
                                                   to = fun,
                                                   nx = nx,
                                                   ax = .Data.ax,
                                                   open = open,
                                                   mA = mA)
                  .Data.ans <- array(.Data.ans,
                                     dim = dim(metadata.ans),
                                     dimnames = dimnames(metadata.ans))
                  methods::new(class.ans,
                               .Data = .Data.ans,
                               metadata = metadata.ans)
              }
          })


#' @rdname prob
#' @export
setMethod("prob",
          signature(object = "LifeTable"),
          function(object) {
              object@prob
          })

#' @rdname prob
#' @export
setReplaceMethod("prob",
                 signature(object = "LifeTable"),
                 function(object, value) {
                     prob <- checkAndTidyProb(value)
                     object@prob <- prob
                     object
                 })

#' @rdname radix
#' @export
setMethod("radix",
          signature(object = "LifeTable"),
          function(object) {
              object@radix
          })

#' @rdname radix
#' @export
setReplaceMethod("radix",
                 signature(object = "LifeTable"),
                 function(object, value) {
                     radix <- checkAndTidyRadix(value)
                     object@radix <- radix
                     object
                 })

#' @rdname LifeTable-class
#' @export
setMethod("show",
          signature(object = "LifeTable"),
          function(object) {
              mult.by.radix <- getFunMultByRadix()
              mx <- object@mx
              ax <- object@ax
              radix <- object@radix
              showFun <- object@showFun
              showQuantiles <- object@showQuantiles
              showTotal <- object@showTotal
              prob <- object@prob
              if (showTotal) {
                  l <- addTotalCategory(mx = mx,
                                        ax = ax)
                  mx <- l$mx
                  ax <- l$ax
              }
              dimtypes <- dimtypes(mx, use.names = FALSE)
              dimensions <- makeDimensions(mx)
              life.table.funs <- calculateLifeTableFuns(mx = mx,
                                                        ax = ax,
                                                        radix = radix,
                                                        funs = showFun,
                                                        ltFunSecond = TRUE)              
              has.iter <- "iteration" %in% dimtypes
              if (has.iter) {
                  if (showQuantiles)
                      life.table.funs <- dembase::collapseIterations(life.table.funs,
                                                                     prob = prob)
              }
              if (radix > 1) {
                  for (fun in showFun) {
                      if (fun %in% mult.by.radix) {
                          value <- dembase::slab(life.table.funs,
                                                 dimension = 2L,
                                                 elements = fun)
                          rounded.value <- round(value)
                          dembase::slab(life.table.funs,
                                        dimension = 2L,
                                        elements = fun) <- rounded.value
                      }
                  }
              }
              life.table.funs <- life.table.funs@.Data
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              cat("dimensions:\n")
              print(dimensions, quote = FALSE)
              cat("\n\n")
              print(life.table.funs,
                    digits = 4)
          })

#' @rdname showFun
#' @export
setMethod("showFun",
          signature(object = "LifeTable"),
          function(object) {
              object@showFun
          })

#' @rdname showFun
#' @export
setReplaceMethod("showFun",
                 signature(object = "LifeTable"),
                 function(object, value) {
                     checkShowFun(value)
                     object@showFun <- value
                     object
                 })

#' @rdname showQuantiles
#' @export
setMethod("showQuantiles",
          signature(object = "LifeTable"),
          function(object) {
              object@showQuantiles
          })

#' @rdname showQuantiles
#' @export
setReplaceMethod("showQuantiles",
                 signature(object = "LifeTable"),
                 function(object, value) {
                     checkShowQuantiles(value)
                     object@showQuantiles <- value
                     object
                 })

#' @rdname showTotal
#' @export
setMethod("showTotal",
          signature(object = "LifeTable"),
          function(object) {
              object@showTotal
          })

#' @rdname showTotal
#' @export
setReplaceMethod("showTotal",
                 signature(object = "LifeTable"),
                 function(object, value) {
                     checkShowTotal(value)
                     object@showTotal <- value
                     object
                 })


#' @rdname Sx
#' @export
setMethod("Sx",
          signature(object = "LifeTable"),
          function(object, useLabelStart = TRUE) {
              checkLabelAgeStart(useLabelStart)
              mx <- object@mx
              ax <- object@ax
              showTotal <- object@showTotal
              if (showTotal) {
                  l <- addTotalCategory(mx = mx,
                                        ax = ax)
                  mx <- l$mx
                  ax <- l$ax
              }
              DS.age <- dembase::DimScales(mx)[[1L]]
              n.age <- length(DS.age)
              dv.age <- DS.age@dimvalues
              dv.age.finite <- dv.age[is.finite(dv.age)]
              if (length(dv.age.finite) > 2L) {
                  length.age.gp <- diff(dv.age.finite)
                  if (!all(length.age.gp[-1L] == length.age.gp[1L]))
                      stop(gettext("age groups have unequal lengths : consider using function 'collapseIntervals' to make lengths equal"))
              }
              Lx <- makeLx(mx = mx,
                           ax = ax)
              breaks <- dv.age[seq_len(n.age - 1L)]
              Lx.curr <- collapseIntervals(Lx,
                                           dimension = 1L,
                                           breaks = breaks)
              Lx.next <- dembase::slab(Lx,
                                       dimension = 1L,
                                       elements = seq.int(from = 2L, to = n.age))
              if (useLabelStart)
                  metadata.ans <- Lx.curr@metadata
              else
                  metadata.ans <- Lx.next@metadata
              .Data.curr <- as.numeric(Lx.curr)
              .Data.next <- as.numeric(Lx.next)
              .Data.ans <- .Data.next / .Data.curr
              .Data.ans <- array(.Data.ans,
                                 dim = dim(metadata.ans),
                                 dimnames = dimnames(metadata.ans))
              new("Values",
                  .Data = .Data.ans,
                  metadata = metadata.ans)
          })

              
                                       
              
