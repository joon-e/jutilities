
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jutilities

This a collection of convenience functions for data handling, plotting,
and analysis within the `tidyverse` syntax. Most of these functions are
wrappers for tasks I perform regularly (e. g., describe variables, add
mean indices) and wanted to integrate them into `%>%`pipes with just one
line.

## Installation

``` r
install.packages("devtools")
devtools::install_github("joon-e/jutilities")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
