
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CBC bindings for R

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/Lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check-Ubuntu](https://img.shields.io/github/workflow/status/dirkschumacher/rcbc/Ubuntu/master.svg?label=Ubuntu)](https://github.com/dirkschumacher/rcbc/actions)
[![R-CMD-check-Windows](https://img.shields.io/github/workflow/status/dirkschumacher/rcbc/Windows/master.svg?label=Windows)](https://github.com/dirkschumacher/rcbc/actions)
[![R-CMD-check-Mac-OSX](https://img.shields.io/github/workflow/status/dirkschumacher/rcbc/Mac%20OSX/master.svg?label=Mac%20OSX)](https://github.com/dirkschumacher/rcbc/actions)
[![Documentation](https://img.shields.io/github/workflow/status/dirkschumacher/rcbc/Documentation/master.svg?label=Documentation)](https://github.com/dirkschumacher/rcbc/actions)
[![codecov](https://codecov.io/gh/dirkschumacher/rcbc/branch/master/graph/badge.svg)](https://codecov.io/gh/dirkschumacher/rcbc)
[![CRAN
status](https://www.r-pkg.org/badges/version/rcbc)](https://CRAN.R-project.org/package=rcbc)
<!-- badges: end -->

The *rcbc* package provides an interface to the [*CBC* (COIN-OR branch
and cut)](https://projects.coin-or.org/Cbc) solver. Specifically, *CBC*
is an open-source mixed integer programming solver that is developed as
part of the [Computational Infrastructure for Operations Research
(COIN-OR) project](http://coin-or.org/). By interfacing with the *CBC*
solver, the *rcbc* package can be used to generate optimal solutions to
optimization problems. Please note that this package is under active
development and is still a work in progress.

## Installation

The package is not yet available on [The Comprehensive R Archive
Network](https://cran.r-project.org/). To install this package, please
use the following *R* code to install it from the [source code
repository on GitHub](https://github.com/dirkschumacher/rcbc). Please
note that [CBC solver](https://projects.coin-or.org/Cbc) header and
library files also need be installed prior to installing this *R*
package (see below for details).

``` r
if (!require(remotes))
  install.packages("remotes")
remotes::install_github("dirkschumacher/rcbc")
```

### Windows

The package can be installed from source when the
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) software is
installed. Specifically, the [CBC
solver](https://projects.coin-or.org/Cbc) header and library files are
automatically downloaded from [RWinLib](https://github.com/rwinlib/cbc).

### Linux

#### Debian/Ubuntu

The following system command can be used install dependences.

    sudo apt-get install coinor-libcbc-dev coinor-libclp-dev

#### Fedora

The following system command can be used install dependences.

    sudo yum install coin-or-Cbc-devel coin-or-Clp-devel

### Mac OSX

The following system command can be used install dependences using
[Homebrew package manager](https://brew.sh/).

    brew install coin-or-tools/coinor/cbc

## Usage

Here we will provide a brief example showing how the package can be used
to solve an optimization problem (see [package
vignette](https://dirkschumacher.github.io/rcbc/articles/rcbc.html) for
more details).

``` r
# load package
library(rcbc)

# define optimization problem and solve it
## max 1 * x + 2 * y
## s.t.
##   x + y <= 1
##   x, y binary
result <- cbc_solve(
 obj = c(1, 2),
 mat = matrix(c(1, 1), ncol = 2, nrow = 1),
 is_integer = c(TRUE, TRUE),
 row_lb = -Inf, row_ub = 1, max = TRUE,
 col_lb = c(0, 0), col_ub = c(1, 1),
 cbc_args = list("SEC" = "1"))

# extract solution status
solution_status(result)
#> [1] "optimal"

# extract solution values
column_solution(result)
#> [1] 0 1

# extract objective value for solution
objective_value(result)
#> [1] 2
```

## ROI plugin

There is now a work in progress [ROI
plugin](https://github.com/dirkschumacher/ROI.plugin.cbc).

## Contribution

Feel free to open issues and send pull requests.

## Citation

Please cite the *rcbc* R package and the [CBC
solver](https://projects.coin-or.org/Cbc) in publications.

    Warning in citation("rcbc"): no date field in DESCRIPTION file of package 'rcbc'
    
    To cite package 'rcbc' in publications use:
    
      Dirk Schumacher and Jeroen Ooms (2021). rcbc: COIN CBC MILP Solver
      Bindings. R package version 0.1.0.9001.
    
    A BibTeX entry for LaTeX users is
    
      @Manual{,
        title = {rcbc: COIN CBC MILP Solver Bindings},
        author = {Dirk Schumacher and Jeroen Ooms},
        year = {2021},
        note = {R package version 0.1.0.9001},
      }
