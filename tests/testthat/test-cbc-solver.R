context("cbc_solve")

describe("cbc_solve", {
  it("solves a simple MIP", {
    A <- as(Matrix::Matrix(matrix(c(1, 1, 1, 1), ncol = 2, nrow = 2)),
            "TsparseMatrix")
    result <- cbc_solve(
              obj = c(1, 2),
              mat = A,
              is_integer = c(TRUE, TRUE),
              row_lb = c(-Inf, -Inf),
              row_ub = c(1, 1),
              max = TRUE,
              cbc_args = list("logLevel" = 0))
    expect_equal(2, objective_value(result))
    expect_equal(c(0, 1), column_solution(result))
    expect_equal("optimal", solution_status(result))
  })
  it("works with normal matrices", {
    A <- matrix(c(1, 1, 1, 1), ncol = 2, nrow = 2)
    result <- cbc_solve(
      obj = c(1, 2),
      mat = A,
      is_integer = c(TRUE, TRUE),
      row_lb = c(-Inf, -Inf),
      row_ub = c(1, 1),
      max = TRUE,
      cbc_args = list("logLevel" = 0))
    expect_equal(2, objective_value(result))
    expect_equal(c(0, 1), column_solution(result))
    expect_equal("optimal", solution_status(result))
  })
  it("returns multiple solution status", {
    A <- matrix(c(1, 1, 1, 1), ncol = 2, nrow = 2)
    result <- cbc_solve(
      obj = c(1, 2),
      mat = A,
      is_integer = c(TRUE, TRUE),
      row_lb = c(-Inf, -Inf),
      row_ub = c(1, 1),
      max = TRUE,
      cbc_args = list("logLevel" = 0))
    expect_equal("optimal", solution_status(result))
    # TODO: these status will get S3 methods..
    expect_true(result$is_proven_optimal)
    expect_false(result$is_proven_infeasible)
    expect_false(result$is_proven_dual_infeasible)
    expect_false(result$is_iteration_limit_reached)
    expect_false(result$is_node_limit_reached)
    expect_false(result$is_solution_limit_reached)
  })
  it("handles infeasible problems", {
    A <- matrix(c(1, 1, 1, 1), ncol = 2, nrow = 2)
    result <- cbc_solve(
      obj = c(1, 2),
      mat = A,
      is_integer = c(TRUE, TRUE),
      row_lb = c(-Inf, 1),
      row_ub = c(1, 1),
      col_ub = c(1, 0),
      col_lb = c(0, 0),
      max = TRUE,
      cbc_args = list("logLevel" = 0))
    expect_equal("infeasible", solution_status(result))
  })
  it("handles unbounded problems", {
    A <- matrix(c(1, 1, 1, 1), ncol = 2, nrow = 2)
    result <- cbc_solve(
      obj = c(1, 2),
      mat = A,
      is_integer = c(TRUE, TRUE),
      row_lb = c(-Inf, -Inf),
      row_ub = c(Inf, Inf),
      max = TRUE,
      cbc_args = list("logLevel" = 0))
    expect_equal("unbounded", solution_status(result))
  })
  it("fails if constraints and obj lengths do not match", {
    expect_error(cbc_solve(obj = c(1, 2),
                           matrix(c(1, 2), ncol = 1, nrow = 2),
                           row_lb = c(-Inf, -Inf),
                           row_ub = c(1, 2)))
  })
  it("fails if constraints and row lb lengths do not match", {
    expect_error(cbc_solve(obj = c(1),
                           matrix(c(1, 2), ncol = 1, nrow = 2),
                           row_lb = c(-Inf),
                           row_ub = c(1, 2)))
  })
  it("fails if constraints and row ub lengths do not match", {
    expect_error(cbc_solve(obj = c(1),
                           matrix(c(1, 2), ncol = 1, nrow = 2),
                           row_lb = c(-Inf, -Inf),
                           row_ub = c(2)))
  })
  it("can handle integer variables", {
    set.seed(1)
    max_capacity <- 1000
    n <- 10
    weights <- round(runif(n, max = max_capacity))
    cost <- round(runif(n) * 100)

    A <- matrix(weights, ncol = n, nrow = 1)
    result <- cbc_solve(
      obj = cost,
      mat = A,
      is_integer = rep.int(TRUE, n),
      row_lb = 0, row_ub = max_capacity, max = TRUE,
      col_lb = rep.int(0, n), col_ub = rep.int(1, n),
      cbc_args = list("logLevel" = 0))
    expect_true(all(column_solution(result) %in% c(0, 1)))
  })
  it("passes all remaining parameters as args to cbc", {
    A <- matrix(c(1, 1, 1, 1), ncol = 2, nrow = 2)
    result <- cbc_solve(
      obj = c(1, 2),
      mat = A,
      is_integer = c(TRUE, TRUE),
      row_lb = c(-Inf, -Inf),
      row_ub = c(1, 1),
      max = TRUE,
      cbc_args = list("max", "presolve",
                      "maxSolutions" = 23, "logLevel" = 0))
    expect_equal(2, objective_value(result))
  })
})

describe("prepare_cbc_args", {
  it("provides default arguments for empty call", {
    res <- prepare_cbc_args()
    expected <- c("problem", "-solve", "-quit")
    expect_equal(res, expected,
                 label = "no arguments turns into default character vector")

  })
  it("converts arguments to a character vector", {
    res <- prepare_cbc_args(OsiMaxNumIteration = 10L, OsiPrimalTolerance = 0.001)
    expected <- c("problem",
                  "-OsiMaxNumIteration", "10",
                  "-OsiPrimalTolerance", "0.001",
                  "-solve", "-quit")
    expect_equal(res, expected,
                 label = "arguments converted to named character vector")
  })
  it("turns unnamed argument into a value-less attribute", {
    res <- prepare_cbc_args(OsiMaxNumIteration = 10L, "max")
    expected <- c("problem",
                  "-OsiMaxNumIteration", "10",
                  "-max",
                  "-solve", "-quit")

    expect_equal(res, expected,
                 label = "argument without a name is a parameter")
  })
  it("errors if empty string is passed as argument", {
    expect_error(prepare_cbc_args(""),
                 regexp = "grepl.* are not true",
                 label = "all parameter names should contain word characters")
  })
})

test_that("status is assigned correct value", {
  result <- list(
    is_proven_optimal = FALSE,
    is_proven_infeasible = FALSE,
    is_proven_dual_infeasible = FALSE,
    is_node_limit_reached = TRUE,
    is_solution_limit_reached = FALSE,
    is_abandoned = FALSE,
    is_iteration_limit_reached = TRUE
  )
  result <- structure(result, class = "rcbc_milp_result")
  expect_equal(solution_status(result), "nodelimit",
               label = "status corresponding to the first true value is returned")

  result <- list(
    is_proven_optimal = FALSE,
    is_proven_infeasible = FALSE,
    is_abandoned = FALSE,
    is_iteration_limit_reached = TRUE
  )
  result <- structure(result, class = "rcbc_milp_result")
  expect_equal(solution_status(result), "iterationlimit",
               label = "corresponding status returned")

  result <- list(
    is_proven_optimal = FALSE,
    is_proven_infeasible = FALSE,
    is_abandoned = FALSE,
    is_iteration_limit_reached = FALSE
  )
  result <- structure(result, class = "rcbc_milp_result")
  expect_equal(solution_status(result), "unknown",
               lable = "unknown is returned if non of values is set to TRUE")

})
