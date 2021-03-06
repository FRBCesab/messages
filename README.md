
<!-- README.md is generated from README.Rmd. Please edit that file -->

# messages

<!-- badges: start -->

[![R CMD
Check](https://github.com/frbcesab/messages/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/frbcesab/messages/actions/workflows/R-CMD-check.yaml)
[![Website
deployment](https://github.com/frbcesab/messages/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/frbcesab/messages/actions/workflows/pkgdown.yaml)
[![License: GPL (\>=
2)](https://img.shields.io/badge/License-GPL%20%28%3E%3D%202%29-blue.svg)](https://choosealicense.com/licenses/gpl-2.0/)
[![LifeCycle](man/figures/lifecycle/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

The R package `messages` is a collection of functions to print messages
in the R console using the packages
[`cli`](https://cran.r-project.org/package=cli) and
[`crayon`](https://cran.r-project.org/package=crayon). This package is
strongly inspired from the functions `ui_*()` of the package
[`usethis`](https://cran.r-project.org/package=usethis).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")

remotes::install_github("frbcesab/messages")
```

## Usage

``` r
## Attach package ----
library("messages")
```

``` r
## Success message ----
msg_done("A success message")
#> ✓ A success message

## Todo message ----
msg_todo("A to do message")
#> • A to do message

## Info message ----
msg_info("An informational message")
#> ℹ An informational message

## Warning message ----
msg_warn("A warning message")
#> ! A warning message

## Error message ----
msg_oops("An error message")
#> x An error message

## Message ----
msg_line("A classic message")
#> A classic message

## Message within a rule ----
msg_rule("Left message")
#> ── Left message ─────────────────────────────────────────────────────────────────────────

## Message within a rule ----
msg_rule(center = "Center message")
#> ───────────────────────────────────── Center message ────────────────────────────────────

## Message within a rule ----
msg_rule(right = "Right message")
#> ──────────────────────────────────────────────────────────────────────── Right message ──


## Messages with inline customization ----
x <- 1

msg_done("The variable", msg_field("x"), "has been set to", msg_value(x))
#> ✓ The variable x has been set to 1

## Messages with inline customization (2) ----
msg_todo("Please use the function:", msg_code("msg_rule()"))
#> • Please use the function: `msg_rule()`

## Messages with inline customization (3) ----
msg_todo("Please use the function:\n", msg_code("msg_rule()"))
#> • Please use the function:
#>   `msg_rule()`
```

## Citation

Please cite this package as:

> Casajus N. (2022) messages: Print messages in the console. R package
> version 1.0. URL: <https://frbcesab.github.io/messages/>.

You can also run:

``` r
citation("messages")

## A BibTeX entry for LaTeX users is:
## 
## @Manual{,
##   title  = {{messages}: {P}rint messages in the console,
##   author = {{Casajus N.}},
##   year   = {2022},
##   note   = {R package version 1.0},
##   url    = {https://frbcesab.github.io/messages/},
## }
```
