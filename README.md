
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CBC bindings for R

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/Lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![check-Ubuntu](https://img.shields.io/github/actions/workflow/status/dirkschumacher/rcbc/check-ubuntu.yaml?branch=master&label=Ubuntu)](https://github.com/dirkschumacher/rcbc/actions)
[![check-Windows](https://img.shields.io/github/actions/workflow/status/dirkschumacher/rcbc/check-windows.yaml?branch=master&label=Windows)](https://github.com/dirkschumacher/rcbc/actions)
[![check-macOS](https://img.shields.io/github/actions/workflow/status/dirkschumacher/rcbc/check-macos.yaml?branch=master&label=macOS)](https://github.com/dirkschumacher/rcbc/actions)
[![Documentation](https://img.shields.io/github/actions/workflow/status/dirkschumacher/rcbc/documentation.yaml?branch=master&label=Documentation)](https://github.com/dirkschumacher/rcbc/actions)
[![Coverage
Status](https://img.shields.io/codecov/c/github/dirkschumacher/rcbc?label=Coverage)](https://app.codecov.io/github/dirkschumacher/rcbc?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/rcbc)](https://CRAN.R-project.org/package=rcbc)
<!-- badges: end -->

The *rcbc* package provides an interface to the [*CBC* (COIN-OR branch
and cut)](https://github.com/coin-or/Cbc) solver. Specifically, *CBC* is
an open-source mixed integer programming solver that is developed as
part of the [Computational Infrastructure for Operations Research
(COIN-OR) project](https://www.coin-or.org/). By interfacing with the
*CBC* solver, the *rcbc* package can be used to generate optimal
solutions to optimization problems. Please note that this package is
under active development and is still a work in progress.

## Installation

The package is not yet available on [The Comprehensive R Archive
Network](https://cran.r-project.org/). To install this package, please
use the following *R* code to install it from the [source code
repository on GitHub](https://github.com/dirkschumacher/rcbc). Please
note that [CBC solver](https://github.com/coin-or/Cbc) header and
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
solver](https://github.com/coin-or/Cbc) header and library files are
automatically downloaded from [RWinLib](https://github.com/rwinlib/cbc).

### Linux

#### Debian/Ubuntu

The following system command can be used to install dependences.

    sudo apt-get install coinor-libcbc-dev coinor-libclp-dev

#### Fedora

The following system command can be used to install dependencies.

    sudo yum install coin-or-Cbc-devel coin-or-Clp-devel

### macOS

The following system command can be used to install dependencies using
[Homebrew package manager](https://brew.sh/). After installing CBC and
its dependencies, they need to linked in order to install the *rcbc*
package. **Please note that if you have previously installed these
software, then they will be overwritten with the newer versions.**

    brew tap coin-or-tools/coinor
    brew install coin-or-tools/coinor/cbc
    brew link cbc --force
    brew link coinutils --force
    brew link osi --force
    brew link clp --force
    brew link cgl --force

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
solver](https://github.com/coin-or/Cbc) in publications.

    To cite the rcbc package in publications, use:

      Schumacher D, Ooms J, Yapparov B, Hanson JO (2024). _rcbc: COIN CBC
      MILP Solver Bindings_. R package version 0.1.0.9002,
      <https://github.com/dirkschumacher/rcbc>.

      Forrest J, Lougee-Heimer R (2005). "CBC User Guide." In _Emerging
      theory, Methods, and Applications_, 257-277.
      <https://doi.org/10.1287/educ.1053.0020>.

    Please cite both COIN-OR CBC and this package.

    To see these entries in BibTeX format, use 'print(<citation>,
    bibtex=TRUE)', 'toBibtex(.)', or set
    'options(citation.bibtex.max=999)'.
