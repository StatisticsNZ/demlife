
## NO_TESTS
convertLifeTableFun <- function(.Data, from, to, nx, ax, open, mA) {
    if (from == to)
        return(.Data)
    if (from == "mx") {
        if (open)
            mA <- .Data[nrow(.Data), ]
        .Data <- mx2qx(.Data = .Data, nx = nx, ax = ax, open = open)
        Recall(.Data, from = "qx", to = to, nx = nx, ax = ax, open = open, mA = mA)
    }
    else if (from == "qx") {
        if (to == "mx")
            qx2mx(.Data, nx = nx, ax = ax, open = open, mA = mA)
        else {
            .Data <- qx2px(.Data = .Data)
            Recall(.Data, from = "px", to = to, nx = nx, ax = ax, open = open, mA = mA)
        }
    }
    else if (from == "px") {
        if (to %in% c("mx", "qx")) {
            .Data <- px2qx(.Data)
            Recall(.Data, from = "qx", to = to, nx = nx, ax = ax, open = open, mA = mA)
        }
        else {
            .Data <- px2lx(.Data, open = open)
            Recall(.Data, from = "lx", to = to, nx = nx, ax = ax, open = open, mA = mA)
        }
    }
    else if (from == "dx") {
        .Data <- dx2lx(.Data, open = open)
        Recall(.Data, from = "lx", to = to, nx = nx, ax = ax, open = open, mA = mA)
    }
    else if (from == "lx") {
        if (to %in% c("mx", "qx", "px")) {
            .Data <- lx2px(.Data, open = open)
            Recall(.Data, from = "px", to = to, nx = nx, ax = ax, open = open, mA = mA)
        }
        else if (to == "dx")
            lx2dx(.Data, open = open)
        else if (to == "ex") {
            lx2ex(.Data, nx = nx, ax = ax, open = open, mA = mA)
        }
        else {
            .Data <- lx2Lx(.Data, nx = nx, ax = ax, open = open, mA = mA)
            Recall(.Data, from = "Lx", to = to, nx = nx, ax = ax, open = open, mA = mA)
        }
    }
    else if (from == "Lx") {
        if (to == "Tx")
            Lx2Tx(.Data, open = open)
        else if (to == "ex")
            Lx2ex(.Data = .Data, nx = nx, ax = ax, open = open)
        else {
            if (open) {
                Lx.open <- .Data[nrow(.Data), ]
                .Data <- Lx2lx(.Data, nx = nx, ax = ax, open = open)
                mA <- .Data[nrow(.Data), ] / Lx.open
            }
            else
                .Data <- Lx2lx(.Data, nx = nx, ax = ax, open = open)
            Recall(.Data, from = "lx", to = to, nx = nx, ax = ax, open = open, mA = mA)
        }
    }
    else if (from == "Tx") {
        .Data <- Tx2Lx(.Data)
        Recall(.Data, from = "Lx", to = to, nx = nx, ax = ax, open = open, mA = mA)
    }
    else if (from == "ex") {
        .Data <- ex2Lx(.Data, nx = nx, ax = ax)
        Recall(.Data, from = "Lx", to = to, nx = nx, ax = ax, open = open, mA = mA)
    }
    else
        stop(gettextf("'%s' is not a valid life table function",
                      to))
}



## HAS_TESTS
mx2qx <- function(.Data, nx, ax, open) {
  ans <- (nx * .Data) / (1 + (nx - ax) * .Data)
  if (open)
    ans[nrow(ans), ] <- 1
  ans
}

## HAS_TESTS
qx2mx <- function(.Data, nx, ax, open, mA) {
  if (open && is.null(mA))
    stop(gettext("insufficient information to calculate values for open age group"))
  ans <- .Data / (ax * .Data + nx * (1 - .Data))
  if (open)
    ans[nrow(ans), ] <- mA
  ans
}

## HAS_TESTS
qx2px <- function(.Data) {
  1 - .Data
}

## HAS_TESTS
px2qx <- function(.Data) {
  1 - .Data
}


px2lx <- function(.Data, open) {
  n <- if (open) nrow(.Data) else nrow(.Data) + 1L
  ans <- matrix(nrow = n, ncol = ncol(.Data))
  ans[1L, ] <- 1
  for (i in seq.int(from = 2L, to = n))
    ans[i, ] <- .Data[i - 1L, ] * ans[i - 1L, ]
  ans
}


lx2px <- function(.Data, open) {
  n <- nrow(.Data)
  if (open) {
    ans <- matrix(nrow = n, ncol = ncol(.Data))
    for (i in seq.int(from = 1L, to = n - 1L))
      ans[i, ] <- .Data[i + 1L, ] / .Data[i, ]
    ans[n, ] <- 0
  }
  else {
    ans <- matrix(nrow = n - 1L, ncol = ncol(.Data))
    for (i in seq.int(from = 1L, to = n - 1L))
      ans[i, ] <- .Data[i + 1L, ] / .Data[i, ]
  }
  ans
}

lx2dx <- function(.Data, open) {
  n <- if (open) nrow(.Data) else nrow(.Data) - 1L
  ans <- matrix(nrow = n, ncol = ncol(.Data))
  for (i in seq_len(nrow(.Data) - 1L))
    ans[i, ] <- .Data[i, ] - .Data[i + 1L, ]
  if (open)
    ans[n, ] <- .Data[n, ]
  ans
}

dx2lx <- function(.Data, open) {
  n <- if (open) nrow(.Data) else nrow(.Data) + 1L
  ans <- matrix(nrow = n, ncol = ncol(.Data))
  ans[1L, ] <- 1
  for (i in seq.int(from = 2L, to = n))
    ans[i, ] <- ans[i - 1L, ] - .Data[i - 1L, ]
  ans
}

lx2Lx <- function(.Data, nx, ax, open, mA) {
  n <- if (open) nrow(.Data) else nrow(.Data) - 1L
  ans <- matrix(nrow = n, ncol = ncol(.Data))
  for (i in seq_len(nrow(.Data) - 1L))
    ans[i, ] <- nx[i, ] * .Data[i + 1L, ] + ax[i, ] * (.Data[i, ] - .Data[i + 1, ])
  if (open)
    ans[n, ] <- .Data[n, ] / mA
  ans
}


lx2ex <- function(.Data, nx, ax, open, mA) {
  if (!open)
    stop(gettextf("cannot calculate '%s' unless last age group is open",
                  "ex"))
  if (is.null(mA))
    stop(gettext("insufficient information to calculate values for open age group"))
  Lx <- lx2Lx(.Data, nx = nx, ax = ax, open = open, mA = mA)
  Tx <- Lx2Tx(Lx, open = open)
  Tx / .Data
}



Lx2lx <- function(.Data, nx, ax, open) {
  n <- if (open) nrow(.Data) else nrow(.Data) + 1L
  ans <- matrix(nrow = n, ncol = ncol(.Data))
  ans[1L, ] <- 1
  for (i in seq.int(from = 2L, to = n))
    ans[i, ] <- ((.Data[i - 1L, ] - ax[i - 1L, ] * ans[i - 1L, ]) /
                   (nx[i - 1L, ] - ax[i - 1L, ]))
  ans
}

Lx2Tx <- function(.Data, open) {
  if (!open)
    stop(gettextf("cannot calculate '%s' unless last age group is open",
                  "Tx"))
  ans <- .Data
  for (i in seq.int(from = nrow(ans) - 1L, to = 1L))
    ans[i, ] <- ans[i + 1L, ] + .Data[i, ]
  ans
}

Tx2Lx <- function(.Data) {
  ans <- .Data
  for (i in seq.int(from = nrow(ans) - 1L, to = 1L))
    ans[i, ] <- .Data[i, ] - .Data[i + 1L, ]
  ans
}


Lx2ex <- function(.Data, nx, ax, open, mA) {
  lx <- Lx2lx(.Data, nx = nx, ax = ax, open = open)
  Tx <- Lx2Tx(.Data, open = open)
  Tx / lx
}

ex2Lx <- function(.Data, nx, ax) {
  lx <- matrix(1, nrow = nrow(.Data), ncol = ncol(.Data))
  for (i in seq.int(from = 2L, to = nrow(.Data)))
    lx[i, ] <- ((.Data[i - 1L, ] - ax[i - 1L, ]) /
                  (.Data[i, ] - ax[i - 1L, ] + nx[i - 1L, ])) * lx[i - 1L, ]
  mA <- 1 / .Data[nrow(.Data), ]
  lx2Lx(.Data = lx, nx = nx, ax = ax, open = TRUE, mA = mA)
}

## HAS_TESTS
## assume that 'mx' and 'ax' have passed
## validity tests for LifeTable
makeLx <- function(mx, ax) {
    metadata <- mx@metadata
    DimScales <- DimScales(metadata, use.names = FALSE)
    DimScale.age <- DimScales[[1L]]
    dv.age <- DimScale.age@dimvalues
    nx <- diff(dv.age)
    mx <- matrix(mx@.Data,
                 nrow = nrow(mx@.Data))
    ax <- matrix(ax@.Data,
                 nrow = nrow(ax@.Data))
    nAge <- nrow(mx)
    nOther <- ncol(mx)
    .Data.ans <- makeLxInner(mx = mx,
                             ax = ax,
                             nx = nx,
                             nAge = nAge,
                             nOther = nOther,
                             useC = FALSE) ## change to TRUE when C version finished
    .Data.ans <- array(.Data.ans,
                       dim = dim(metadata),
                       dimnames = dimnames(metadata))
    new("Counts",
        .Data = .Data.ans,
        metadata = metadata)
}

## TRANSLATED
## HAS_TESTS
makeLxInner <- function(mx, ax, nx, nAge, nOther, useC = FALSE) {
    ## mx
    stopifnot(is.matrix(mx))
    stopifnot(is.double(mx))
    stopifnot(!any(is.na(mx)))
    stopifnot(all(mx >= 0))
    ## ax
    stopifnot(is.matrix(ax))
    stopifnot(is.double(ax))
    stopifnot(!any(is.na(ax)))
    stopifnot(all(ax >= 0))
    ## nx
    stopifnot(is.double(nx))
    stopifnot(!any(is.na(nx)))
    stopifnot(all(nx >= 0))
    ## nAge
    stopifnot(is.integer(nAge))
    stopifnot(identical(length(nAge), 1L))
    stopifnot(!is.na(nAge))
    stopifnot(nAge >= 2L)
    ## nOther
    stopifnot(is.integer(nOther))
    stopifnot(identical(length(nOther), 1L))
    stopifnot(!is.na(nOther))
    stopifnot(nOther >= 1L)
    ## mx, nAge, nOther
    stopifnot(identical(dim(mx), c(nAge, nOther)))
    ## ax, nAge, nOther
    stopifnot(identical(dim(ax), c(nAge, nOther)))
    if (useC) {
        .Call(makeLxInner_R, mx, ax, nx, nAge, nOther)
    }
    else {
        ans <- double(length = nAge * nOther)
        for (j in seq_len(nOther)) {
            lx.i <- 1
            for (i in seq_len(nAge - 1L)) {
                i.ans <- i + (j - 1L) * nAge
                nx.i <- nx[i]
                mx.i <- mx[i.ans]
                ax.i <- ax[i.ans]
                qx.i <- nx.i * mx.i / (1 + (nx.i - ax.i) * mx.i)
                lx.iplus1 <- lx.i * (1 - qx.i)
                ans[i.ans] <- lx.iplus1 * nx.i + (lx.i - lx.iplus1) * ax.i
                lx.i <- lx.iplus1
            }
            i.ans <- j * nAge
            mx.i <- mx[i.ans]
            ans[i.ans] <- lx.i / mx.i
        }
        ans
    }
}
