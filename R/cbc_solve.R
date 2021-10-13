#' Solve a mixed integer problem with \emph{CBC}
#'
#' \href{https://projects.coin-or.org/Cbc}{\emph{CBC} (COIN-OR branch and cut)}
#' is an open-source mixed integer programming
#' solver (Forrest & Lougee-Heimer 2005). It is developed as part of the
#' \href{https://projects.coin-or.org/Cbc}{Computational Infrastructure for Operations Research (COIN-OR) project}.
#' By leveraging the \emph{CBC} solver, this function can be used to generate
#' solutions to optimization problems.
#'
#' @param obj \code{numeric} \code{vector} if coefficients to specify the
#' objective function. Note that arguments should have one value per decision
#' variable (i.e. column in \code{mat}).
#'
#' @param mat matrix (i.e. \code{matrix} or \code{\link{Matrix-class}}) of
#' constraint coefficients. Here, each column corresponds to a different
#' decision variable, and each row corresponds to a different constraint.
#' To improve performance, it is recommended to specify the matrix using
#' a sparse format (see \code{\link[Matrix]{sparseMatrix}}).
#'
#' @param row_ub \code{numeric} \code{vector} of upper bounds for constraints.
#' Note that arguments should have one value per constraint
#' (i.e. row in \code{mat}).
#' Defaults to a \code{vector} of infinity (\code{Inf}) values for each
#' constraint.
#'
#' @param row_lb \code{numeric} \code{vector} of lower bounds for constraints.
#'  Note that arguments should have one value per constraint
#' (i.e. row in \code{mat}).
#' Defaults to a \code{vector} of negative infinity (\code{-Inf}) values for
#' each constraint.
#'
#' @param col_ub \code{numeric} \code{vector} of upper bounds for decision
#'  variables. Note that arguments should have one value per decision variable
#' (i.e. column in \code{mat}).
#' Defaults to a \code{vector} of infinity (\code{Inf}) values for each
#' variable.
#'
#' @param col_lb \code{numeric} \code{vector} of lower bounds for decision
#' variables. Note that arguments should have one value per decision variable
#' (i.e. column in \code{mat}).
#' Defaults to a \code{vector} of negative infinity (\code{-Inf}) values for
#' each variable.
#'
#' @param is_integer \code{logical} \code{vector} indicating if (\code{TRUE})
#' each decision variable is integer or (\code{FALSE}) not an integer.
#' Note that arguments should have one value per decision variable
#' (i.e. column in \code{mat}).
#' Defaults to a \code{vector} of \code{FALSE} values for each variable
#' (meaning that all variables are continuous).
#'
#' @param max \code{logical} (i.e. \code{TRUE} or \code{FALSE}) should the
#' solver aim to maximize the objective function? Defaults to \code{FALSE}.
#'
#' @param cbc_args \code{list} of \code{character} \emph{CBC} arguments to customize the optimization
#' procedures (see \emph{CBC} parameters section for details). Defaults to
#' \code{list()}, such that default settings are used to generate solutions.
#' Note that all arguments must be supplied as a \code{character} object
#' (see below for examples).
#'
#' @param initial_solution \code{numeric} initial values
#' for starting solution. Missing (\code{NA}) values can be used to indicate
#' that the starting value for a solution should be automatically calculated.
#' Defaults to \code{NULL} such that the starting solution is automatically
#' generated.
#'
#' @section \emph{CBC} parameters:
#' Many different parameters can be specified to customize the optimization
#' process (see the \href{https://projects.coin-or.org/CoinBinary/export/1059/OptimizationSuite/trunk/Installer/files/doc/cbcCommandLine.pdf}{user manual} full list). Among all these parameters, some of the most useful parameters
#' include the following:
#'
#' \describe{
#'
#' \item{log}{Should information be printed during the solve process? Defaults
#' to \code{"1"} meaning that information will be printed to the console.
#' To prevent this, a value of \code{"0"} can be specified.}
#'
#' \item{verbose}{The level of detail to provide when printing information
#' to the console. Defaults to \code{"1"} for a relatively small amount of
#' detail. For maximum detail, a value of \code{"15"} can be specified.}
#'
#' \item{presolve}{Should the optimization process include a pre-processing
#' step that attempts to simplify the optimization problem? Defaults to
#' \code{"On"} such that this pre-processing step is performed. To disable
#' this step, a value of \code{"Off"} can be specified.}
#'
#' \item{ratio}{Optimality gap for solving the problem. The optimization
#'  process will stop when the performance of the best found solution
#' is near enough to optimality based on this parameter. For example,
#' a value of 0.15 means that the optimization process will stop
#' when the best found solution is within 15% of the optimal solution.
#' Defaults to \code{"0"}, such that only an optimal solution is returned.}
#'
#' \item{sec}{Maximum amount of time permitted for completing the
#' optimization process. Defaults to \code{"1e+100"}. Note that time is
#' measured based on the \code{TIMEM} parameter.}
#'
#' \item{timem}{Method to measure time. Defaults to \code{"cpu"} such that
#' timing is measured using CPU time. To specify that time should be
#' measured based on overall time elapsed (e.g. based on a wall clock),
#' a value of \code{"elapsed"} can be specified.}
#'
#' \item{threads}{Number of threads to use for solving the optimization
#' problem. For example, a value of \code{"4"} indicates that four
#' threads should be used. Defaults to \code{"0"}, such that the number of
#' threads is automatically determined.}
#'
#' \item{maxso}{Maximum number of solutions permitted to examine during the
#' optimization process. For example, a value of \code{"1"} can be used
#' to obtain the first feasible solution.}
#'
#' }
#'
#' @return A \code{list} containing the solution and additional information.
#' Specifically, it contains the following elements:
#'
#' \describe{
#'
#' \item{column_solution}{\code{numeric} values of the decision variables
#' in the solution (same order as columns in the constraint matrix).}
#'
#' \item{objective_value}{\code{numeric} objective value of the solution.}
#'
#' \item{status}{\code{character} description of the optimization process
#' when it finished. This description is based on the result of the
#' following elements. For example, an \code{"optimal"} status indicates
#' that the optimization process finished because it found an optimal solution.
#' Also, if a maximum time limit was specified (using \code{cbc_args}), an
#' \code{"timelimit"} status indicates that the optimization process
#' finished because it ran out of time. Furthermore, an \code{"infeasible"}
#' status indicates that there is no possible solution to the specified
#' optimization problem. This \code{"infeasible"} status could potentially
#' mean that there was a mistake when constructing the input data.}
#'
#' \item{is_proven_optimal}{\code{logical} indicating if the solution proven
#' to be optimal.}
#'
#' \item{is_proven_dual_infeasible}{\code{logical} indicating if the
#' dual is proven to be infeasible.}
#'
#' \item{is_proven_infeasible}{\code{logical} indicating if the optimization
#' problem is proven to be infeasible.}
#'
#' \item{is_node_limit_reached}{\code{logical} indicating if the maximum
#' number of nodes permitted for solving the optimization problem was
#' reached.}
#'
#' \item{is_solution_limit_reached}{\code{logical} indicating if the maximum
#' number of solutions (including those generated before finding the final
#' solution) permitted for solving the optimization problem was reached.}
#'
#' \item{is_abandoned}{\code{logical} indicating if the optimization process
#' was abandoned part way through solving the optimization problem.}
#'
#' \item{is_iteration_limit_reached}{\code{logical} indicating if the maximum
#' number of iterations permitted for solving the optimization problem was
#' reached.}
#'
#' \item{is_seconds_limit_reached}{\code{logical} indicating if the maximum
#' number of seconds permitted for for solving the optimization problem was
#' reached.}
#'
#' }
#'
#' @seealso
#' \code{\link{objective_value}},
#' \code{\link{column_solution}},
#' \code{\link{solution_status}}.
#'
#' @references
#' Forrest J and Lougee-Heimer R (2005) CBC User Guide. In Emerging theory,
#' Methods, and Applications (pp. 257--277). INFORMS, Catonsville, MD.
#' \doi{10.1287/educ.1053.0020}.
#'
#' @examples
#' \dontrun{
#' # Mathematically define a mixed integer programming problem
#' ## maximize:
#' ##   1 * x + 2 * y + 0.5 * z (eqn 1a)
#' ## subject to:
#' ##   x + y <= 1              (eqn 1b)
#' ##   3 * x + 4 * z >= 5      (eqn 1c)
#' ##   z = 4                   (eqn 1d)
#' ##  0 <= x <= 10             (eqn 1e)
#' ##  0 <= y <= 11             (eqn 1f)
#' ##  0 <= z <= 13             (eqn 1g)
#' ##  x, y, z is integer       (eqn 1h)
#'
#' # Create variables to represent this problem
#' ### define objective function (eqn 1a)
#' obj <- c(1, 2, 3)
#'
#' ## define constraint matrix (eqns 1c--1d)
#' A <- matrix(c(1, 1, 0, 3, 0, 4, 0, 0, 1), byrow = TRUE, nrow = 3)
#' print(A)
#'
#' ## note that we could also define the constraint matrix using a
#' ## sparse format to reduce memory consumption
#' ## (though not needed for such a small problem)
#' library(Matrix)
#' A_sp <- sparseMatrix(
#'   i = c(1, 2, 1, 2, 3),
#'   j = c(1, 1, 2, 3, 3),
#'   x = c(1, 3, 1, 4, 1))
#' print(A_sp)
#'
#' ## define upper and lower bounds for constraints (eqns 1c--1d)
#' row_ub <- c(1, Inf, 4)
#' row_lb <- c(-Inf, 5, 4)
#'
#' ## define upper and lower bounds for decision variables (eqns 1e--1g)
#' col_ub <- c(10, 11, 13)
#' col_lb <- c(0, 0, 0)
#'
#' ## specify which decision variables are integer (eqn 1h)
#' is_integer <- c(TRUE, TRUE, TRUE)
#'
#' # Generate solution
#' ## run solver (with default settings)
#' result <- cbc_solve(
#'   obj = obj, mat = A, is_integer = is_integer,
#'   row_lb = row_lb, row_ub = row_ub,
#'   col_lb = col_lb, col_ub = col_ub,
#'   max = TRUE)
#'
#' ## print result
#' print(result)
#'
#' ## extract information from result
#' objective_value(result) # objective value of solution
#' column_solution(result) # decision variable values in solution
#' solution_status(result) # status description
#'
#' # Generate a solution with customized settings
#' ## specify that only a single thread should be used,
#' ## we only need a solution within 20% of optimality,
#' ## and that we can only 2 (wall clock) seconds for the solution
#' cbc_args <- list(threads = "1", ratio = "0.2", sec = "2", timem = "elapsed")
#'
#' ## run solver (with customized settings)
#' result2 <- cbc_solve(
#'   obj = obj, mat = A, is_integer = is_integer,
#'   row_lb = row_lb, row_ub = row_ub,
#'   col_lb = col_lb, col_ub = col_ub,
#'   max = TRUE, cbc_args = cbc_args)
#'
#' ## print result
#' ## we can see that this result is exactly the same as the previous
#' ## result, so these customized settings did not really any influence.
#' ## this is because the optimization problem is incredibly simple
#' ## and so \emph{CBC} can find the optimal solution pretty much instantly
#' ## we would expect such customized settings to have an influence
#' ## when solving more complex problems
#' print(result2)
#' }
#' @importFrom assertthat assert_that
#' @importFrom assertthat noNA
#' @importFrom assertthat is.flag
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
                      max = FALSE,
                      cbc_args = list(),
                      initial_solution = NULL) {
  # assert arguments are valid and prepare data
  ## argument classes
  assert_that(
    is.numeric(obj), inherits(mat, c("matrix", "Matrix")),
    is.numeric(row_ub), is.numeric(row_lb),
    is.numeric(col_ub), is.numeric(col_lb),
    is.logical(is_integer), is.flag(max), is.list(cbc_args),
    inherits(initial_solution, c("NULL", "numeric"))
  )
  ## coerce mat to sparse matrix
  if (!inherits(mat, "dgTMatrix")) {
    mat <- as(mat, "dgTMatrix")
  }
  ## check for missing values
  assert_that(
    noNA(obj), noNA(row_ub), noNA(row_lb), noNA(col_ub), noNA(col_lb),
    noNA(is_integer), noNA(max)
  )
  assert_that(noNA(mat@x), msg = "argument to mat contains missing values")
  ## dimensionality
  assert_that(
    length(obj) == ncol(mat),
    length(col_lb) == ncol(mat),
    length(col_ub) == ncol(mat),
    length(is_integer) == ncol(mat),
    length(row_lb) == nrow(mat),
    length(row_ub) == nrow(mat)
  )
  ## feasible lower and upper bounds for constraints and variables
  assert_that(
    all(row_ub >= row_lb),
    all(col_ub >= col_lb))
  ## assert valid initial solution
  if (!is.null(initial_solution)) {
    ### verify sane values and dimensionality
    assert_that(length(initial_solution) == length(obj))
    assert_that(
      all(
        initial_solution[is_integer] == round(initial_solution[is_integer]),
        na.rm = TRUE),
      msg = "argument to initial solution does not obey is_integer"
    )
    assert_that(
      all(initial_solution <= col_ub, na.rm = TRUE),
      msg = "argument to initial solution does not obey col_ub"
    )
    assert_that(
      all(initial_solution >= col_lb, na.rm = TRUE),
      msg = "argument to initial solution does not obey col_lb"
    )
    use_initial_solution <- TRUE
  } else {
    use_initial_solution <- FALSE
  }

  # prepare cbc arguments
  cbc_args <- do.call(prepare_cbc_args, cbc_args)

  # prepare arguments for initial solution
  if (use_initial_solution) {
    ## store indices for non-NA values
    initial_index <- which(is.finite(initial_solution[is_integer]))
    ## create names for starting solution variables
    ## note: this is because the C++ CBC interface need them for starting values
    initial_names <- paste0("C", seq_along(initial_index))
    assert_that(identical(anyDuplicated(initial_names), 0L))
    # note: we only pass starting values for integer variables
    # because CBC automatically computes values for non-integer variables
    # for details, see Cbc_setMIPStart section in
    # https://www.coin-or.org/Doxygen/Cbc/Cbc__C__Interface_8h.html
    initial_solution <- round(initial_solution[is_integer])[initial_index]
    # note: since CBC only let's us specify a starting solution for
    # integer variables, we won't bother specifying a starting solution
    # if there are no non-integer variables
    if (length(initial_solution) == 0L) {
      use_initial_solution <- FALSE
      warning("ignoring initial_solution because no integer variables")
    }
  } else {
    initial_solution <- 0
    initial_index <- 0L
    initial_names <- NA_character_
  }

  # run cbc
  result <- cpp_cbc_solve(
    obj = obj,
    isMaximization = max,
    rowIndices = mat@i,
    colIndices = mat@j,
    elements = mat@x,
    integerIndices = which(is_integer) - 1,
    colLower = col_lb,
    colUpper = col_ub,
    rowLower = row_lb,
    rowUpper = row_ub,
    arguments = cbc_args,
    initialIndex = initial_index - 1,
    initialSolution = initial_solution,
    initialNames = initial_names,
    useInitialSolution = use_initial_solution
  )

  # return result
  structure(result, class = "rcbc_milp_result")
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

#' Appends prefix to argument names with value
#' @noRd
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

#' Returns permutation vector of named elements
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

#' Column solution
#'
#' Extract the values of decision variables from a solution generated using the
#' \href{https://projects.coin-or.org/Cbc}{\emph{CBC} (COIN-OR branch and cut)}
#' solver (via \code{\link{cbc_solve}}).
#'
#' @param result \emph{CBC} result (\code{rcbc_milp_result}) object.
#'
#' @return \code{numeric} values of the decision variables
#'   in the solution (same order as columns in the constraint matrix used
#'   to formulate the problem).
#'
#' @seealso
#' \code{\link{cbc_solve}},
#' \code{\link{solution_status}},
#' \code{\link{objective_value}}.
#'
#' @examples
#' \dontrun{
#' # Define optimization problem
#' ## max 1 * x + 2 * y
#' ## s.t.
#' ##   x + y <= 1
#' ##   x, y integer
#'
#' # Generate solution
#' A <- matrix(c(1, 1), ncol = 2, nrow = 1)
#' result <- cbc_solve(
#'   obj = c(1, 2),
#'   mat = A,
#'   is_integer = c(TRUE, TRUE),
#'   row_lb = c(-Inf),
#'   row_ub = c(1),
#'   max = TRUE)
#'
#' # Extract column solution
#' column_solution(result)
#' }
#' @export
column_solution <- function(result) {
  UseMethod("column_solution")
}

#' @export
column_solution.rcbc_milp_result <- function(result) {
  result$column_solution
}

#' Objective value
#'
#' Extract the objective value of a solution generated using the
#' \href{https://projects.coin-or.org/Cbc}{\emph{CBC} (COIN-OR branch and cut)}
#' solver (via \code{\link{cbc_solve}}).
#'
#' @inheritParams column_solution
#'
#' @return A \code{numeric} value.
#'
#' @seealso
#' \code{\link{cbc_solve}},
#' \code{\link{column_solution}},
#' \code{\link{solution_status}}.
#'
#' @examples
#' \dontrun{
#' # Define optimization problem
#' ## max 1 * x + 2 * y
#' ## s.t.
#' ##   x + y <= 1
#' ##   x, y integer
#'
#' # Generate solution
#' A <- matrix(c(1, 1), ncol = 2, nrow = 1)
#' result <- cbc_solve(
#'   obj = c(1, 2),
#'   mat = A,
#'   is_integer = c(TRUE, TRUE),
#'   row_lb = c(-Inf),
#'   row_ub = c(1),
#'   max = TRUE)
#'
#' # Extract objective value
#' objective_value(result)
#' }
#' @export
objective_value <- function(result) {
  UseMethod("objective_value")
}

#' @export
objective_value.rcbc_milp_result <- function(result) {
  result$objective_value
}


#' Solution status
#'
#' Extract the status of a solution generated using the
#' \href{https://projects.coin-or.org/Cbc}{\emph{CBC} (COIN-OR branch and cut)}
#' solver (via \code{\link{cbc_solve}}).
#'
#' @inheritParams column_solution
#'
#' @return A \code{character} status description.
#' For example, an \code{"optimal"} status indicates
#' that the optimization process finished because it found an optimal solution.
#' Also, if a maximum time limit was specified, an
#' \code{"timelimit"} status indicates that the optimization process
#' finished because it ran out of time. Furthermore, an \code{"infeasible"}
#' status indicates that there is no possible solution to the specified
#' optimization problem. This \code{"infeasible"} status could potentially
#' mean that there was a mistake when constructing the input data.
#'
#' @seealso
#' \code{\link{cbc_solve}},
#' \code{\link{column_solution}},
#' \code{\link{objective_value}}.
#'
#' @examples
#' \dontrun{
#' # Define optimization problem
#' ## max 1 * x + 2 * y
#' ## s.t.
#' ##   x + y <= 1
#' ##   x, y integer
#'
#' # Generate solution
#' A <- matrix(c(1, 1), ncol = 2, nrow = 1)
#' result <- cbc_solve(
#'   obj = c(1, 2),
#'   mat = A,
#'   is_integer = c(TRUE, TRUE),
#'   row_lb = c(-Inf),
#'   row_ub = c(1),
#'   max = TRUE)
#'
#' # Extract solution status
#' solution_status(result)
#' }
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
