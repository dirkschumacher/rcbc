#' CBC bindigns for R
#'
#' @docType package
#' @keywords package
#' @useDynLib rcbc, .registration = TRUE
#' @name rcbc
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("rcbc", libpath)
}
