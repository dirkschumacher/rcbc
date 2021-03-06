#' CBC bindigns for R
#'
#' Add in dummy description.
#'
#' @docType package
#' @keywords package
#' @useDynLib rcbc, .registration = TRUE
#' @name rcbc
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("rcbc", libpath)
}
