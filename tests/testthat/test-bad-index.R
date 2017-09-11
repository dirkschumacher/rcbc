library(rcbc)

load("data-bad-index.rda")

n <- length(profit)

result <- cbc_solve(
  obj = profit,
  mat = rbind(volume, moq.constraints),
  is_integer = rep.int(TRUE, n),
  row_lb = c(0L, rep(0, moq.lines)),
  row_ub = c(cap, rep(1, moq.lines)),
  max = TRUE,
  col_lb = rep.int(0L, n),
  col_ub = rep.int(1L, n),
  cbc_args = list(logLevel = 0));

sol <- column_solution(result)
print(tail(sol))
