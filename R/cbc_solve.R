#' Solve a linear (mixed) integer program with CBC
#'
#' @param obj coeffcients for the objective function. One number per column.
#' @param mat the constraint matrix. Needs to be an object that can be coerced
#'            to a sparse triplet based matrix via \code{as(mat, "dgTMatrix")}.
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
#' result <- cbc_solve(
#'   obj = c(1, 2),
#'   mat = A,
#'   is_integer = c(TRUE, TRUE),
#'   row_lb = c(-Inf),
#'   row_ub = c(1),
#'   max = TRUE)
#' @importFrom assertthat assert_that
#' @importFrom methods as
#' @import Matrix
#' @export
cbc_solve <- function(obj,
                      mat,
                      row_ub = rep.int(Inf, nrow(mat)),
                      row_lb = rep.int(-Inf, nrow(mat)),
                      col_lb = rep.int(-Inf, ncol(mat)),
                      col_ub = rep.int(Inf, ncol(mat)),
                      is_integer = rep.int(FALSE, ncol(mat)),
                      max = FALSE, cbc_args = list()) {
  assert_that(is.numeric(obj), is.numeric(row_ub))
  is_sparse_matrix <- inherits(mat, "dgTMatrix")
  if (!is_sparse_matrix) {
    mat <- as(mat, "dgTMatrix")
  }
  assert_that(
    length(obj) == ncol(mat),
    length(col_lb) == ncol(mat),
    length(col_ub) == ncol(mat),
    length(is_integer) == ncol(mat),
    length(row_lb) == nrow(mat),
    length(row_ub) == nrow(mat)
  )

  cbc_args <- do.call(prepare_cbc_args, cbc_args)

  # sanitize non-finite values for C interface (it doesn't like NA/Inf values)
  ## identify Inf and -Inf values in col bounds
  colLowerInf <- which(col_lb == Inf)
  colLowerNInf <- which(col_lb == -Inf)
  colUpperInf <- which(col_ub == Inf)
  colUpperNInf <- which(col_ub == -Inf)
  ## identify Inf and -Inf values in row bounds
  rowLowerInf <- which(row_lb == Inf)
  rowLowerNInf <- which(row_lb == -Inf)
  rowUpperInf <- which(row_ub == Inf)
  rowUpperNInf <- which(row_ub == -Inf)
  ## convert non-finite values to -9999
  col_lb[colLowerInf] = -9999
  col_lb[colLowerNInf] = -9999
  col_ub[colUpperInf] = -9999
  col_ub[colUpperNInf] = -9999
  row_lb[rowLowerInf] = -9999
  row_lb[rowLowerNInf] = -9999
  row_ub[rowUpperInf] = -9999
  row_ub[rowUpperNInf] = -9999

  raw <- .C(
    rcbc_cbc_solve,
    # dimensionality of arguments
    nCols = as.integer(ncol(mat)),
    nRows = as.integer(nrow(mat)),
    nElements = as.integer(length(mat@x)),
    nIntegerIndices = as.integer(sum(is_integer)),
    nArgs = as.integer(length(cbc_args)),
    # dimensionality of non-finite indices
    nColLowerInf = as.integer(length(colLowerInf)),
    nColLowerNInf = as.integer(length(colLowerNInf)),
    nColUpperInf = as.integer(length(colUpperInf)),
    nColUpperNInf = as.integer(length(colUpperNInf)),
    nRowLowerInf = as.integer(length(rowLowerInf)),
    nRowLowerNInf = as.integer(length(rowLowerNInf)),
    nRowUpperInf = as.integer(length(rowUpperInf)),
    nRowUpperNInf = as.integer(length(rowUpperNInf)),
    # non-finite indices
    colLowerInf = as.integer(colLowerInf - 1L),
    colLowerNInf = as.integer(colLowerNInf - 1L),
    colUpperInf = as.integer(colUpperInf - 1L),
    colUpperNInf = as.integer(colUpperNInf - 1L),
    rowLowerInf = as.integer(rowLowerInf - 1L),
    rowLowerNInf = as.integer(rowLowerNInf - 1L),
    rowUpperInf = as.integer(rowUpperInf - 1L),
    rowUpperNInf = as.integer(rowUpperNInf - 1L),
    # optimization problem
    obj = as.double(obj),
    isMaximization = as.integer(as.logical(max)),
    rowIndices = as.integer(mat@i),
    colIndices = as.integer(mat@j),
    elements = as.double(mat@x),
    integerIndices = as.integer(which(is_integer) - 1),
    colLower = as.double(col_lb),
    colUpper = as.double(col_ub),
    rowLower = as.double(row_lb),
    rowUpper = as.double(row_ub),
    # CBC arguments
    argsList = cbc_args,
    # outputs
    column_solution = double(length(obj)),
    objective_value = double(1L),
    is_proven_optimal = integer(1L),
    is_proven_dual_infeasible = integer(1L),
    is_proven_infeasible = integer(1L),
    is_node_limit_reached = integer(1L),
    is_solution_limit_reached = integer(1L),
    is_abandoned = integer(1L),
    is_iteration_limit_reached = integer(1L),
    is_seconds_limit_reached = integer(1L)
  )

  # prepare output
  structure(
    list(
      column_solution = raw$column_solution,
      objective_value = raw$objective_value,
      is_proven_optimal = as.logical(raw$is_proven_optimal),
      is_proven_dual_infeasible = as.logical(raw$is_proven_dual_infeasible),
      is_proven_infeasible = as.logical(raw$is_proven_infeasible),
      is_node_limit_reached = as.logical(raw$is_node_limit_reached),
      is_solution_limit_reached = as.logical(raw$is_solution_limit_reached),
      is_abandoned = as.logical(raw$is_abandoned),
      is_iteration_limit_reached = as.logical(raw$is_iteration_limit_reached),
      is_seconds_limit_reached = as.logical(raw$is_seconds_limit_reached)),
   class = "rcbc_milp_result")
}

#' Prepares list of arguments into format accepted by cbc
#'
#' @noRd
#' @param ... parameters for the cbc optimisation
prepare_cbc_args <- function(...) {
  cbc_args <- list(...)

  names <- rename_cbc_args(names(cbc_args), cbc_args)
  assert_that(all(grepl("-\\w.*", names)))
  values <- revalue_cbc_args(names(cbc_args), cbc_args)
  n <- length(values)
  args <- NULL
  if (n > 0L) {
    names.index <- rep(c(TRUE, FALSE), n)
    args[names.index] <- names
    args[!names.index] <- values
    args <- args[args != ""] # remove empty values
  }

  # combine custom model arguments with global package level arguments
  args <-  c("problem", args, "-solve", "-quit");
}

# Appends prefix to argument names with value
# @noRd
rename_cbc_args <- function(names, values) {
  if (length(values) == 0L) {
    return(character())
  }

  prefix_cbc_args(ifelse(has_name(names, values), names, values))
}

#' Appends prefix to argument names without value
#' @noRd
revalue_cbc_args <- function(names, values) {
  if (length(values) == 0L) {
    return(character())
  }

  values <- as.character(values)
  ifelse(has_name(names, values), values, "")
}

#' Returns permutation vector of named elments
#' @noRd
has_name <- function(names, values) {
  if (!is.character(names)) {
    names <- rep.int("", length(values))
  }
  nchar(names) > 0L
}

#' Appends prefix to parameter name
#' @noRd
prefix_cbc_args <- function(x) paste0("-", x)

#' Return the column solution
#' @param result a cbc solution object
#' @return numeric vector. One component per column in the
#'          order of the column vector.
#' @export
column_solution <- function(result) {
  UseMethod("column_solution")
}

#' @export
column_solution.rcbc_milp_result <- function(result) {
  result$column_solution
}

#' Return the objective value of a solution
#' @param result a cbc solution object
#' @return a single number
#' @export
objective_value <- function(result) {
  UseMethod("objective_value")
}

#' @export
objective_value.rcbc_milp_result <- function(result) {
  result$objective_value
}


#' Return the solution status.
#' @param result a cbc solution object
#' @return a string being either "optimal", "infeasible" or "unbounded"
#' @export
solution_status <- function(result) {
  UseMethod("solution_status")
}

#' @export
solution_status.rcbc_milp_result <- function(result) {

  status_map <- list(
    is_proven_optimal = "optimal",
    is_proven_dual_infeasible = "unbounded",
    is_proven_infeasible = "infeasible",
    is_node_limit_reached = "nodelimit",
    is_solution_limit_reached = "solutionlimit",
    is_abandoned = "abandoned",
    is_iteration_limit_reached = "iterationlimit",
    is_seconds_limit_reached = "timelimit"
    )

  result <- Filter(function(x) is.logical(x) && x == TRUE, result)
  if (length(result) > 0L) {
    status_map[names(result)][[1L]]
  }
  else {
    "unknown"
  }
}
