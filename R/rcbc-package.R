#' \emph{CBC} bindings for R
#'
#' The \pkg{rcbc} package provides an interface to the
#' \href{https://projects.coin-or.org/Cbc}{\emph{CBC} (COIN-OR branchand cut)}
#' solver (Forrest & Lougee-Heimer 2005). Specifically, \emph{CBC} is an
#' open-source mixed integer programming solver that is developed as part of
#' the Computational Infrastructure for Operations Research (COIN-OR) project.
#' By interfacing with the \emph{CBC} solver, the \pkg{rcbc} package can be
#' used to generate optimal solutions to optimization problems (using the
#' \code{\link{cbc_solve}} function).
#'
#' @inherit cbc_solve examples references
#'
#' @seealso \url{https://projects.coin-or.org/Cbc}.
#'
#' @docType package
#' @keywords package
## usethis namespace: start
#' @useDynLib rcbc, .registration = TRUE
## usethis namespace: end
#' @name rcbc
NULL
