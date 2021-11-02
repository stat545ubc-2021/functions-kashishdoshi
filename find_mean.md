Function
================
Kashish Doshi
01/11/2021

``` r
suppressPackageStartupMessages(library(gapminder))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(datateachr))
```

## find\_mean

The goal of this function was to build a specific workflow around
dplyr::group\_by() and dplyr::mutate(). I wanted to build this function
because in the milstone asignments in STAT 545A I used this partiular
workflow numerous times and would have helped to have a function that
could return this output with one line instead of 3.

``` r
#' find_mean function
#' 
#' @title Find mean of columns according to groups and add it to original tibble.
#' @name find_mean
#' @usage find_mean(tib, col_name, group_name)
#' @description Add a mean column to data-frame after grouping by another column. The new mean column name will add a "mean." prefix to the
#'original column name ("mean.col_name"). This function uses the mutate [dplyr::mutate()] function and group_by [dplyr::group_by()] from the dplyr package.
#' 
#' @param tib A tibble. Since the input should be a tibble I named it "tib"
#' @param col_name A column name to find mean of. To make it evident I named it col_name. Function will remove NA from this column
#' @param group_name Optional parameter. A column name. If provided, data-set will be grouped by this column before finding mean. Since the data is going to be grouped according to this I named it group_name. Function will not remove any NA from this column.
#' 
#' @return A tibble
#' @export
#' @md

find_mean = function(tib, col_name, group_name) {
  calculations = dplyr::summarise(tib,
                                  is_numeric = is.numeric({{ col_name }}),
                                  class = class({{ col_name }}))
  if (!calculations$is_numeric) {
    stop("Selected column is not numeric. Column is ", calculations$class)
  }
  if (missing(group_name)) {
    mean_tib <- dplyr::mutate(tib, "mean.{{ col_name }}" := mean({{ col_name }}, na.rm = TRUE))
    } else {
      mean_tib <- tib %>% 
        dplyr::group_by({{ group_name }}) %>%
        dplyr::mutate("mean.{{ col_name }}" := mean({{ col_name }}, na.rm = TRUE))
    }
  }
```

### Examples

#### 1. Test without optional parameters

``` r
#Example of finding mean without optional input

gap_lfExp_mean = find_mean(gapminder, lifeExp)
print(gap_lfExp_mean)
```

    ## # A tibble: 1,704 x 7
    ##    country     continent  year lifeExp      pop gdpPercap mean.lifeExp
    ##    <fct>       <fct>     <int>   <dbl>    <int>     <dbl>        <dbl>
    ##  1 Afghanistan Asia       1952    28.8  8425333      779.         59.5
    ##  2 Afghanistan Asia       1957    30.3  9240934      821.         59.5
    ##  3 Afghanistan Asia       1962    32.0 10267083      853.         59.5
    ##  4 Afghanistan Asia       1967    34.0 11537966      836.         59.5
    ##  5 Afghanistan Asia       1972    36.1 13079460      740.         59.5
    ##  6 Afghanistan Asia       1977    38.4 14880372      786.         59.5
    ##  7 Afghanistan Asia       1982    39.9 12881816      978.         59.5
    ##  8 Afghanistan Asia       1987    40.8 13867957      852.         59.5
    ##  9 Afghanistan Asia       1992    41.7 16317921      649.         59.5
    ## 10 Afghanistan Asia       1997    41.8 22227415      635.         59.5
    ## # ... with 1,694 more rows

#### 2. Test with optional parameters

``` r
#Example of finding mean with optional input

lifeExp_coun_mean = find_mean(gapminder, lifeExp, country)
print(lifeExp_coun_mean)
```

    ## # A tibble: 1,704 x 7
    ## # Groups:   country [142]
    ##    country     continent  year lifeExp      pop gdpPercap mean.lifeExp
    ##    <fct>       <fct>     <int>   <dbl>    <int>     <dbl>        <dbl>
    ##  1 Afghanistan Asia       1952    28.8  8425333      779.         37.5
    ##  2 Afghanistan Asia       1957    30.3  9240934      821.         37.5
    ##  3 Afghanistan Asia       1962    32.0 10267083      853.         37.5
    ##  4 Afghanistan Asia       1967    34.0 11537966      836.         37.5
    ##  5 Afghanistan Asia       1972    36.1 13079460      740.         37.5
    ##  6 Afghanistan Asia       1977    38.4 14880372      786.         37.5
    ##  7 Afghanistan Asia       1982    39.9 12881816      978.         37.5
    ##  8 Afghanistan Asia       1987    40.8 13867957      852.         37.5
    ##  9 Afghanistan Asia       1992    41.7 16317921      649.         37.5
    ## 10 Afghanistan Asia       1997    41.8 22227415      635.         37.5
    ## # ... with 1,694 more rows

#### 3. Test with non-expected input parameter

``` r
#Example of error

find_mean(gapminder, country, continent)
```

    ## Error in find_mean(gapminder, country, continent): Selected column is not numeric. Column is factor

### Formally testing the function

Here I have conducted 5 tests that can demonstrate the validity and
usability of the function.

``` r
test1 = mutate(gapminder, mean.lifeExp = mean(lifeExp, na.rm = TRUE))
test_that("Test find_mean with input Vector that has no NAs and no optional argument", 
          expect_equal(find_mean(gapminder, lifeExp), test1))
```

    ## Test passed

``` r
test2 = mutate(vancouver_trees, mean.latitude = mean(latitude, na.rm= TRUE))
test_that("Test find_mean with input Vector that has NAs and no optional argument", 
          expect_equal(find_mean(vancouver_trees, latitude), test2))
```

    ## Test passed

``` r
test3 = vancouver_trees %>%
  group_by(cultivar_name) %>%
  mutate(mean.diameter = mean(diameter, na.rm=TRUE))
test_that("Test find_mean with input Vector that has NAs and an optional argument", 
          expect_equal(find_mean(vancouver_trees, diameter, cultivar_name), test3))
```

    ## Test passed

``` r
test4 = mutate(vancouver_trees, new_col = NA)
test_that("Test find_mean with input Vector that ONLY has NAs and an optional argument", 
          expect_error(find_mean(test4, new_col, cultivar_name)))
```

    ## Test passed

``` r
test_that("Test find_mean with input Vector of non-numeric type and an optional argument", 
          expect_error(find_mean(test4, species_name, cultivar_name)))
```

    ## Test passed
