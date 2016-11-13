
## HAS_TESTS
calculateLifeTableFuns <- function(mx, ax, radix, funs,
                                   ltFunSecond) {
    mult.by.radix <- getFunMultByRadix()
    names <- names(mx)
    dimtypes <- dimtypes(mx, use.names = FALSE)
    DimScales <- dembase::DimScales(mx, use.names = FALSE)
    DS.age <- DimScales[[1L]]
    dv.age <- DS.age@dimvalues
    n.age <- length(dv.age) - 1L
    open <- is.infinite(dv.age[n.age + 1L])
    .Data.mx <- mx@.Data
    .Data.ax <- ax@.Data
    .Data.mx <- matrix(.Data.mx,
                       nrow = n.age)
    .Data.ax <- matrix(.Data.ax,
                       nrow = n.age)
    nx <- diff(dv.age)
    nx <- matrix(nx,
                 nrow = n.age,
                 ncol = ncol(.Data.mx))
    .Data.ans <- vector(mode = "list",
                        length = length(funs))
    for (i in seq_along(funs)) {
        fun <- funs[[i]]
        if (fun == "mx")
            .Data.ans[[i]] <- .Data.mx
        else if (fun == "ax")
            .Data.ans[[i]] <- .Data.ax
        else
            .Data.ans[[i]] <- convertLifeTableFun(.Data = .Data.mx,
                                                  from = "mx",
                                                  to = fun,
                                                  nx = nx,
                                                  ax = .Data.ax,
                                                  open = open,
                                                  mA = NULL)
        if (fun %in% mult.by.radix)
            .Data.ans[[i]] <- radix * .Data.ans[[i]]
    }
    dim.ans <- c(dim(mx), length(funs))
    n.dim.ans <- length(dim.ans)
    names.ans <- make.unique(c(names, "lt.fun"))
    dimtypes.ans <- c(dimtypes, "state")
    DS.funs <- methods::new("Categories", dimvalues = funs)
    DimScales.ans <- c(DimScales, list(DS.funs))
    name.lt.fun <- names.ans[n.dim.ans]
    .Data.ans <- do.call(c, args = .Data.ans)
    .Data.ans <- array(.Data.ans, dim = dim.ans)
    metadata.ans <- methods::new("MetaData",
                                 nms = names.ans,
                                 dimtypes = dimtypes.ans,
                                 DimScales = DimScales.ans)
    dimnames(.Data.ans) <- dimnames(metadata.ans)
    ans <- methods::new("Values",
                        .Data = .Data.ans,
                        metadata = metadata.ans)
    if (ltFunSecond) {
        s <- seq_len(n.dim.ans)
        perm <- append(s[-n.dim.ans],
                       values = n.dim.ans,
                       after = 1L)
        ans <- aperm(ans,
                     perm = perm)
    }
    ans
}


## HAS_TESTS
checkAndTidyProb <- function(prob) {
    ## 'prob' does not have length 0
    if (identical(length(prob), 0L))
        stop(gettextf("'%s' has length %d",
                      "prob", 0L))
    ## 'prob' is numeric
    if (!is.numeric(prob))
        stop(gettextf("'%s' is non-numeric",
                      "prob"))
    ## 'prob' has no missing values
    if (any(is.na(prob)))
        stop(gettextf("'%s' has missing values",
                      "prob"))
    ## 'prob' all non-negative
    if (any(prob < 0))
        stop(gettextf("'%s' has negative values",
                      "prob"))
    ## 'prob' all less than or equal to 1
    if (any(prob > 1))
        stop(gettextf("'%s' has values greater than %d",
                      "prob", 1L))
    ## 'prob' monotonically increasing
    if (!all(diff(prob) > 0))
        stop(gettextf("values of '%s' not increasing",
                      "prob"))
    prob <- as.double(prob)
    prob
}


## HAS_TESTS
checkAndTidyRadix <- function(radix) {
    ## 'radix' has length 1
    if (!identical(length(radix), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "radix", 1L))
    ## 'radix' is numeric
    if (!is.numeric(radix))
        stop(gettextf("'%s' is non-numeric",
                      "radix"))
    ## 'radix' is not missing
    if (is.na(radix))
        stop(gettextf("'%s' is missing",
                      "radix"))
    ## 'radix' is positive
    if (radix <= 0)
        stop(gettextf("'%s' is non-positive",
                      "radix"))
    radix <- as.double(radix)
    radix
}

## HAS_TESTS
## assume there is an age dimension with length >= 2
checkLifeTableInputValues <- function(object, from, radix) {
  .Data <- object@.Data
  i.age <- match("age", dembase::dimtypes(object))
  n.age <- dim(.Data)[i.age]
  DimScale.age <- dembase::DimScales(object)[[i.age]]
  nx <- diff(DimScale.age@dimvalues)
  index <- slice.index(x = .Data, MARGIN = i.age)
  ## length > 0
  if (identical(length(.Data), 0L))
    stop(gettextf("'%s' has length %d",
                  "object", 0L))
  ## no missing values
  if (any(is.na(.Data)))
    stop(gettextf("'%s' has missing values",
                  "object"))
  ## no negative values
  if (any(.Data < 0))
    stop(gettextf("'%s' has negative values",
                  "object"))
  ## if probability or dx, no values > radix
  if (from %in% c("qx", "px", "lx", "dx")) {
    if (any(.Data > radix))
      stop(gettextf("'%s' is \"%s\" but '%s' has values greater than '%s'",
                    "from", from, "object", "radix"))
  }
  ## If lx, all inital values equal radix
  if (from == "lx") {
    if (any(.Data[index == 1L] != radix))
      stop(gettextf("'%s' is \"%s\" but some values for first age group do not equal '%s'",
                    "from", from, "radix"))
  }
  ## if Lx, all values less than radix * nx
  if (from == "Lx") {
    for (i in seq_len(n.age))
      if (any(.Data[index == i] > radix * nx[i]))
        stop(gettextf("'%s' is \"%s\" but some values are larger than '%s' times the width of the age group",
                      "from", from, "radix"))
  }
  ## if lx, Tx, values are non-increasing
  if (from %in% c("lx", "Tx")) {
    for (i in seq.int(from = 2L, to = n.age)) {
      has.higher <- any(.Data[index == i] > .Data[index == i - 1L])
      if (has.higher)
        stop(gettextf("'%s' is \"%s\" but some values increase with age",
                      "from", from))
    }
  }
  NULL
}

## HAS_TESTS
checkShowFun <- function(showFun) {
    kValidShowFun <- c("mx", "qx", "px", "dx", "lx", "Lx", "Tx", "ex", "ax")
    ## 'showFun' does not have length 0
    if (identical(length(showFun), 0L))
        stop(gettextf("'%s' has length %d",
                      "showFun", 0L))
    ## 'showFun' has no missing values
    if (any(is.na(showFun)))
        stop(gettextf("'%s' has missing values",
                      "showFun"))
    ## 'showFun' has no blanks
    if (any(!nzchar(showFun)))
        stop(gettextf("'%s' has blanks",
                      "showFun"))
    ## 'showFun' has no duplicates
    if (any(duplicated(showFun)))
        stop(gettextf("'%s' has duplicates",
                      "showFun"))
    ## all elements of 'showFun' are valid life table functions
    is.invalid <- !(showFun %in% kValidShowFun)
    if (any(is.invalid)) {
        i.first.invalid <- which(is.invalid)[1L]
        stop(gettextf("\"%s\" is not a valid life table function",
                      showFun[i.first.invalid]))
    }
    NULL
}

## HAS_TESTS
checkShowQuantiles <- function(showQuantiles) {
    ## 'showQuantiles' is logical
    if (!is.logical(showQuantiles))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "showQuantiles", "logical"))
    ## 'showQuantiles' has length 1
    if (!identical(length(showQuantiles), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "showQuantiles", 1L))
    ## 'showQuantiles' is not missing
    if (is.na(showQuantiles))
        stop(gettextf("'%s' is missing",
                      "showQuantiles"))
    NULL
}


## NO_TESTS
checkLifeTableMetaData <- function(object) {
    dimtypes <- dembase::dimtypes(object, use.names = FALSE)
    DimScales <- dembase::DimScales(object, use.names = FALSE)
    ## if has time dimension, dimscale must be "Intervals"
    i.time <- match("time", dimtypes, nomatch = 0L)
    has.time <- i.time > 0L
    if (has.time) {
        dimscale.time <- DimScales[[i.time]]
        if (!methods::is(dimscale.time, "Intervals"))
            stop(gettextf("dimension with %s \"%s\" has %s \"%s\"",
                          "dimtype", "time", "dimscale", dimscale.time))
    }
    ## does not have quantile dimension
    i.quantile <- match("quantile", dimtypes, nomatch = 0L)
    has.quantile <- i.quantile > 0L
    if (has.quantile)
        stop(gettextf("dimension with dimtype \"%s\"",
                      "quantile"))
    NULL             
}

getFunMultByRadix <- function()
    c("dx", "lx", "Lx", "Tx")

## HAS_TESTS
getLifeTableClass <- function(name) {
  switch(EXPR = name,
         mx = "Values",
         qx = "Values",
         px = "Values",
         dx = "Counts",
         lx = "Counts",
         Lx = "Counts",
         Tx = "Counts",
         ex = "Values",
         ax = "Values",
         stop(gettextf("\"%s\" is not a valid life table function",
                       name)))
}

## HAS_TESTS
getLifeTableDimScale <- function(name) {
  switch(EXPR = name,
         mx = "Intervals",
         qx = "Intervals",
         px = "Intervals",
         dx = "Intervals",
         lx = "Points",
         Lx = "Intervals",
         Tx = "Points",
         ex = "Points",
         stop(gettextf("\"%s\" is not a valid life table function",
                       name)))
}

## NO_TESTS
makeDimensions <- function(mx) {
    limits <- limits(mx)
    limits[] <- lapply(limits, as.character)
    first <- limits["first", ]
    last <- limits["last", ]
    first <- as.character(first)
    last <- as.character(last)
    ans <- rbind(`name:` = names(mx),
                 `length:` = dim(mx), 
                 `dimtype:` = dembase::dimtypes(mx),
                 `dimscale:` = dembase::dimscales(mx), 
                 `first:` = first,
                 `last:` = last)
    colnames(ans) <- rep("", ncol(ans))
    ans
}

## HAS_TESTS
## assume 'object' has age dimension with at least 2 age groups
makeLifeTableMetaData <- function(metadata, from, to) {
    if (identical(from, to))
        return(metadata)
    i.age <- match("age", dembase::dimtypes(metadata))
    n.age <- dim(metadata)[i.age]
    DimScale.age.from <- dembase::DimScales(metadata)[[i.age]]
    from.is.int <- methods::is(DimScale.age.from, "Intervals")
    to.is.int <- identical(getLifeTableDimScale(to), "Intervals")
    dimvalues <- DimScale.age.from@dimvalues
    if (from.is.int) {
        from.is.open <- is.infinite(dimvalues[length(dimvalues)])
        if (to.is.int) {
            if ((from %in% c("mx", "Lx"))
                || (to %in% c("qx", "px", "dx"))
                || !from.is.open)
                return(metadata)
            else {
                dimvalues <- dimvalues[-length(dimvalues)]
                DimScale.age.to <- methods::new("Intervals", dimvalues = dimvalues)
            }
        }
        else {
            if (from.is.open) {
                dimvalues <- dimvalues[-length(dimvalues)]
                DimScale.age.to <- methods::new("Points", dimvalues = dimvalues)
            }
            else {
                if (to == "lx")
                    DimScale.age.to <- methods::new("Points", dimvalues = dimvalues)
                else if (to %in% c("Tx", "ex"))
                    stop(gettextf("cannot calculate \"%s\" when last age group is closed",
                                  to))
                else
                    stop(gettextf("\"%s\" is not a valid life table function",
                                  from))
            }
        }
    }
    else {
        if (to.is.int) {
            if (from == "lx")
                DimScale.age.to <- methods::new("Intervals", dimvalues = dimvalues)
            else if (from %in% c("Tx", "ex")) {
                dimvalues <- c(dimvalues, Inf)
                DimScale.age.to <- methods::new("Intervals", dimvalues = dimvalues)
            }
            else
                stop(gettextf("\"%s\" is not a valid life table function",
                              to))
        }
        else {
            if (from == "lx") {
                if (to %in% c("Tx", "ex"))
                    stop(gettextf("cannot calculate \"%s\" from \"%s\"",
                                  to, from))
                else
                    stop(gettextf("\"%s\" is not a valid life table function",
                                  to))
            }
            else if (from %in% c("Tx", "ex"))
                return(metadata)
            else
                stop(gettextf("\"%s\" is not a valid life table function",
                              from))
        }
    }
    nms <- names(metadata)
    dimtypes <- dembase::dimtypes(metadata, use.names = FALSE)
    DimScales <- dembase::DimScales(metadata, use.names = FALSE)
    DimScales <- replace(DimScales, list = i.age, values = list(DimScale.age.to))
    methods::new("MetaData", nms = nms, dimtypes = dimtypes, DimScales = DimScales)
}
