
<!-- README.md is generated from README.Rmd. Please edit that file -->
CBC bindings for R
==================

This package provides bindings to the [COIN-CBC solver](https://projects.coin-or.org/Cbc).

It is currently work in progress. I have only tested it on a Mac so far.

Quickstart
----------

-   `-lCbc -lCgl -lOsiClp -lClp -lCoinUtils` need to be in your `PKG_LIBS` flags.
-   The header files of the above mentioned libraries need to be in the include paths of your cpp compiler.

``` r
devtools::install_github("dirkschumacher/rcbc")
```

``` r
library(rcbc)
# max 1 * x + 2 * y
# s.t.
#   x + y <= 1
#   x, y integer
A <- matrix(c(1, 1), ncol = 2, nrow = 1)
result <- CBC_solve(
 obj = c(1, 2),
 mat = A, # <- can also be a sparse matrix
 is_integer = c(TRUE, TRUE),
 row_lb = -Inf, row_ub = 1, max = TRUE)
result
#> $column_solution
#> [1] 0 1
#> 
#> $status
#> [1] "optimal"
#> 
#> $objective_value
#> [1] 2
```

TODO
----

-   Easy installation on all platforms
-   Parallel solving
-   Add configuration for cut generators
-   Add callback support
-   Write a ROI plugin.

Contribution
------------

Feel free to open issues and send PRs.
