Find Mean
================
Kashish Doshi

<!-- README.md is generated from README.Rmd. Please edit that file -->

# findMean

<!-- badges: start -->
<!-- badges: end -->

The goal of findMean is to find the mean of one column grouped by
another column and add it to the original data-set.

## Installation

You can install the released version of findMean from
[Github](https://github.com) with:

``` r
devtools::install_github("stat545ubc-2021/functions-kashishdoshi/findMean", 
                         ref = "0.1.0")
```

## Examples

This is a basic example of find\_mean which adds a column containing
means of the lifeExp column in the gapminder dataset

``` r
suppressPackageStartupMessages(library(findMean))
suppressPackageStartupMessages(library(gapminder))


lifeExp_mean <- if (!requireNamespace("gapminder", quietly = TRUE)) {
  stop("Package \"gapminder\" is needed to run this example. Please install or load it before running", 
       .call = FALSE)
  } else {
  find_mean(gapminder, lifeExp)
  }
```

This is another example of find\_mean that also uses the optional
grouping parameter

``` r
lfExp_mean_country <- if (!requireNamespace("gapminder", quietly = TRUE)) {
  stop("Package \"gapminder\" is needed to run this example. Please install or load it before running", 
       .call = FALSE)
  } else {
  find_mean(gapminder, lifeExp, country)
  }
```
