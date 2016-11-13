

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
