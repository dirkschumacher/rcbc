context("CBC_solve")

describe("CBC_solve", {
  it("solves a simple MIP", {
    A <- as(Matrix::Matrix(matrix(c(1, 1, 1, 1), ncol = 2, nrow = 2)),
            "TsparseMatrix")
    result <- CBC_solve(
              obj = c(1, 2),
              mat = A,
              is_integer = c(TRUE, TRUE),
              row_lb = c(-Inf, -Inf),
              row_ub = c(1, 1),
              max = TRUE)
    expect_equal(2, result$objective_value)
    expect_equal(c(0, 1), result$column_solution)
    expect_equal("optimal", result$status)
  })
  it("works with normal matrices", {
    A <- matrix(c(1, 1, 1, 1), ncol = 2, nrow = 2)
    result <- CBC_solve(
      obj = c(1, 2),
      mat = A,
      is_integer = c(TRUE, TRUE),
      row_lb = c(-Inf, -Inf),
      row_ub = c(1, 1),
      max = TRUE)
    expect_equal(2, result$objective_value)
    expect_equal(c(0, 1), result$column_solution)
    expect_equal("optimal", result$status)
  })
  it("handles infeasible problems", {
    A <- matrix(c(1, 1, 1, 1), ncol = 2, nrow = 2)
    result <- CBC_solve(
      obj = c(1, 2),
      mat = A,
      is_integer = c(TRUE, TRUE),
      row_lb = c(-Inf, 1),
      row_ub = c(1, 1),
      col_ub = c(1, 0),
      col_lb = c(0, 0),
      max = TRUE)
    expect_equal("infeasible", result$status)
  })
  it("handles unbounded problems", {
    A <- matrix(c(1, 1, 1, 1), ncol = 2, nrow = 2)
    result <- CBC_solve(
      obj = c(1, 2),
      mat = A,
      is_integer = c(TRUE, TRUE),
      row_lb = c(-Inf, -Inf),
      row_ub = c(Inf, Inf),
      max = TRUE)
    expect_equal("unbounded", result$status)
  })
  it("fails if constraints and obj lengths do not match", {
    expect_error(CBC_solve(obj = c(1, 2),
                           matrix(c(1, 2), ncol = 1, nrow = 2),
                           row_lb = c(-Inf, -Inf),
                           row_ub = c(1, 2)))
  })
  it("fails if constraints and row lb lengths do not match", {
    expect_error(CBC_solve(obj = c(1),
                           matrix(c(1, 2), ncol = 1, nrow = 2),
                           row_lb = c(-Inf),
                           row_ub = c(1, 2)))
  })
  it("fails if constraints and row ub lengths do not match", {
    expect_error(CBC_solve(obj = c(1),
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
    result <- CBC_solve(
      obj = cost,
      mat = A,
      is_integer = rep.int(TRUE, n),
      row_lb = 0, row_ub = max_capacity, max = TRUE,
      col_lb = rep.int(0, n), col_ub = rep.int(1, n))
    expect_true(all(result$column_solution %in% c(0, 1)))
  })
  it("passes all remaining parameters as args to cbc", {
    A <- matrix(c(1, 1, 1, 1), ncol = 2, nrow = 2)
    result <- CBC_solve(
      obj = c(1, 2),
      mat = A,
      is_integer = c(TRUE, TRUE),
      row_lb = c(-Inf, -Inf),
      row_ub = c(1, 1),
      max = TRUE,
      cbc_args = list("max", "presolve",  "maxSolutions" = 23, "logLevel" = 1))
    expect_equal(2, result$objective_value)

  })
})
