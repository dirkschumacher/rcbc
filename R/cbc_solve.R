#' Solves a linear (mixed) integer program with CBC
#'
#' @param obj coeffcients for the objective function. One number per column.
#' @param mat the constraint matrix. Needs to be an object that can be coerced
#'            to a sparse triplet based matrix.
#' @param row_ub numeric upper bounds for each row
#' @param row_lb numeric lower bounds for each row
#' @param col_lb numeric lower bounds for each column
#' @param col_ub numeric upper bounds for each column
#' @param is_integer logical vector for each column if variable is integer. By
#'                   default all columns are not integers.
#' @param max boolean TRUE iff problem is maximization problem. Default is FALSE.
#' @param cbc_args list cbc arguments
#'
#' @return a named list. "column_solution" contains the column solution in the order
#'         of the constraint matrix. "status" is the status code.
#'
#' @examples
#' # max 1 * x + 2 * y
#' # s.t.
#' #   x + y <= 1
#' #   x, y integer
#' A <- matrix(c(1, 1), ncol = 2, nrow = 1)
#' result <- CBC_solve(
#'   obj = c(1, 2),
#'   mat = A,
#'   is_integer = c(TRUE, TRUE),
#'   row_lb = c(-Inf),
#'   row_ub = c(1),
#'   max = TRUE)
#' @export
CBC_solve <- function(obj,
                      mat,
                      row_ub,
                      row_lb = rep.int(-Inf, length(row_ub)),
                      col_lb = rep.int(-Inf, length(obj)),
                      col_ub = rep.int(Inf, length(obj)),
                      is_integer = rep.int(FALSE, length(obj)),
                      max = FALSE, cbc_args = list()) {
  stopifnot(is.numeric(obj))
  stopifnot(is.numeric(row_ub))
  if (!"dgTMatrix" %in% class(mat)) {
    mat <- methods::as(Matrix::Matrix(mat), "TsparseMatrix")
  }
  stopifnot(length(obj) == ncol(mat))
  stopifnot(length(col_lb) == ncol(mat))
  stopifnot(length(col_ub) == ncol(mat))
  stopifnot(length(is_integer) == ncol(is_integer))
  stopifnot(length(row_lb) == nrow(mat))
  stopifnot(length(row_ub) == nrow(mat))
  stopifnot(length(is_integer) == ncol(mat))
  cbc_args <- unlist(lapply(cbc_args, as.character),
                                  use.names = TRUE)
  if (!is.character(cbc_args)) {
    cbc_args <- character(0)
  }
  if (!is.character(names(cbc_args))) {
    names(cbc_args) <- rep.int("", length(cbc_args))
  }
  arg_keys <- nchar(names(cbc_args)) > 0
  names(cbc_args)[arg_keys] <- paste0("-", names(cbc_args)[arg_keys])
  cbc_args[!arg_keys] <- paste0("-", cbc_args[!arg_keys])
  cpp_cbc_solve(obj,
                max,
                mat@i,
                mat@j,
                mat@x,
                as.logical(is_integer),
                col_lb,
                col_ub,
                row_lb,
                row_ub,
                cbc_args)
}
