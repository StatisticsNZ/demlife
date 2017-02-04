
#' Functions and data structures for life tables.
#'
#' Tools for creating life tables and extracting life table functions
#' (also known as life table columns).  Uses S4 classes and methods,
#' and depends on package \code{dembase}.
#'
#' The package is still under development.  Future features will include
#' \itemize{
#'   \item Plotting methods for life tables.
#'   \item Multi-decrement life tables.
#'   \item Mult-state life tables (eventually).
#' }
#'
#' @docType package
#' @name demlife
#' @importFrom dembase collapseIntervals
#' @import methods
#' @useDynLib demlife, .registration = TRUE
NULL

