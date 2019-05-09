
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jutilities

This a collection of convenience functions for data handling, plotting,
and analysis within the `tidyverse` syntax. Most of these functions are
wrappers for tasks I perform regularly (e. g., describe variables, add
mean indices) and wanted to integrate them into `%>%` pipes with just
one line.

This is all very much work in progress.

## Installation

Install from GitHub:

``` r
install.packages("devtools")
devtools::install_github("joon-e/jutilities")
```

## Instructions

`jutilities` are to be used with the `tidyverse`. Most functions will
return a tibble and can thus be easily integrated into `%>%` pipes.

``` r
library(tidyverse)
library(jutilities)
```

### Describe continous variables

`describe()` computes several measures of central tendency and
variability for all specified variables:

``` r
diamonds %>% 
  describe(x, y, z)
#> # A tibble: 3 x 11
#>   variable     N Missing     M    SD   Min   Max Range   Mdn   Q25   Q75
#>   <chr>    <int>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 x        53940       0  5.73 1.12      0  10.7  10.7  5.7   4.71  6.54
#> 2 y        53940       0  5.73 1.14      0  58.9  58.9  5.71  4.72  6.54
#> 3 z        53940       0  3.54 0.706     0  31.8  31.8  3.53  2.91  4.04
```

If no variables are specified, all numeric variables are described:

``` r
diamonds %>% 
  describe()
#> # A tibble: 7 x 11
#>   variable     N Missing       M      SD   Min    Max  Range     Mdn    Q25
#>   <chr>    <int>   <int>   <dbl>   <dbl> <dbl>  <dbl>  <dbl>   <dbl>  <dbl>
#> 1 carat    53940       0 7.98e-1 4.74e-1   0.2 5.01e0 4.81e0    0.7    0.4 
#> 2 depth    53940       0 6.17e+1 1.43e+0  43   7.90e1 3.60e1   61.8   61   
#> 3 price    53940       0 3.93e+3 3.99e+3 326   1.88e4 1.85e4 2401    950   
#> 4 table    53940       0 5.75e+1 2.23e+0  43   9.50e1 5.20e1   57     56   
#> 5 x        53940       0 5.73e+0 1.12e+0   0   1.07e1 1.07e1    5.7    4.71
#> 6 y        53940       0 5.73e+0 1.14e+0   0   5.89e1 5.89e1    5.71   4.72
#> 7 z        53940       0 3.54e+0 7.06e-1   0   3.18e1 3.18e1    3.53   2.91
#> # ... with 1 more variable: Q75 <dbl>
```

`describe_groups()` outputs the same statistics for one variable,
grouped by one or more grouping variables.

``` r
diamonds %>% 
  describe_groups(price, cut, color)
#> # A tibble: 35 x 12
#> # Groups:   cut [5]
#>    cut   color     N Missing     M    SD   Min   Max Range   Mdn   Q25
#>    <ord> <ord> <int>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 Fair  D       163       0 4291. 3286.   536 16386 15850 3730  2204.
#>  2 Fair  E       224       0 3682. 2977.   337 15584 15247 2956  1590.
#>  3 Fair  F       312       0 3827. 3223.   496 17995 17499 3035  1642.
#>  4 Fair  G       314       0 4239. 3610.   369 18574 18205 3057  1985 
#>  5 Fair  H       303       0 5136. 3886.   659 18565 17906 3816  2458.
#>  6 Fair  I       175       0 4685. 3730.   735 18242 17507 3246  2324 
#>  7 Fair  J       119       0 4976. 4050.   416 18531 18115 3302  2313 
#>  8 Good  D       662       0 3405. 3175.   361 18468 18107 2728.  957.
#>  9 Good  E       933       0 3424. 3331.   327 18236 17909 2420   969 
#> 10 Good  F       909       0 3496. 3202.   357 18686 18329 2647  1214 
#> # ... with 25 more rows, and 1 more variable: Q75 <dbl>
```

### Describe categorical variables

`cat_ftable()` outputs a frequency table including relative, valid, and
cumulative frequencies for one categorical variable.

``` r
diamonds %>% 
  cat_ftable(cut)
#> # A tibble: 5 x 7
#>   cut           n cum.n percent cum.percent valid.percent cum.valid.percent
#>   <ord>     <int> <int>   <dbl>       <dbl>         <dbl>             <dbl>
#> 1 Fair       1610  1610  0.0298      0.0298        0.0298            0.0298
#> 2 Good       4906  6516  0.0910      0.121         0.0910            0.121 
#> 3 Very Good 12082 18598  0.224       0.345         0.224             0.345 
#> 4 Premium   13791 32389  0.256       0.600         0.256             0.600 
#> 5 Ideal     21551 53940  0.400       1             0.400             1
```

`cat_xtable` outputs contigency tables for one column variable and one
or more row variables:

``` r
diamonds %>% 
  cat_xtable(cut, color, clarity)
#> # A tibble: 56 x 8
#>    color clarity  Fair  Good `Very Good` Premium Ideal total
#>    <ord> <ord>   <int> <int>       <int>   <int> <int> <dbl>
#>  1 D     I1          4     8           5      12    13    42
#>  2 D     SI2        56   223         314     421   356  1370
#>  3 D     SI1        58   237         494     556   738  2083
#>  4 D     VS2        25   104         309     339   920  1697
#>  5 D     VS1         5    43         175     131   351   705
#>  6 D     VVS2        9    25         141      94   284   553
#>  7 D     VVS1        3    13          52      40   144   252
#>  8 D     IF          3     9          23      10    28    73
#>  9 E     I1          9    23          22      30    18   102
#> 10 E     SI2        78   202         445     519   469  1713
#> # ... with 46 more rows
```

Setting the argument `percentages = TRUE` changes the output to relative
frequencies:

``` r
diamonds %>% 
  cat_xtable(cut, color, percentages = TRUE)
#> # A tibble: 7 x 7
#>   color   Fair   Good `Very Good` Premium  Ideal  total
#>   <ord>  <dbl>  <dbl>       <dbl>   <dbl>  <dbl>  <dbl>
#> 1 D     0.101  0.135       0.125   0.116  0.132  0.126 
#> 2 E     0.139  0.190       0.199   0.169  0.181  0.182 
#> 3 F     0.194  0.185       0.179   0.169  0.178  0.177 
#> 4 G     0.195  0.178       0.190   0.212  0.227  0.209 
#> 5 H     0.188  0.143       0.151   0.171  0.145  0.154 
#> 6 I     0.109  0.106       0.0997  0.104  0.0971 0.101 
#> 7 J     0.0739 0.0626      0.0561  0.0586 0.0416 0.0521
```

A Chi² test can be optionally computed by setting the argument `chisq =
TRUE`. Test results will be displayed in a console message:

``` r
diamonds %>% 
  cat_xtable(cut, color, chisq = TRUE)
#> Chi² = 310.317901, df = 24.000000, p = 0.000000
#> # A tibble: 7 x 7
#>   color  Fair  Good `Very Good` Premium Ideal total
#>   <ord> <int> <int>       <int>   <int> <int> <dbl>
#> 1 D       163   662        1513    1603  2834  6775
#> 2 E       224   933        2400    2337  3903  9797
#> 3 F       312   909        2164    2331  3826  9542
#> 4 G       314   871        2299    2924  4884 11292
#> 5 H       303   702        1824    2360  3115  8304
#> 6 I       175   522        1204    1428  2093  5422
#> 7 J       119   307         678     808   896  2808
```

### Modify data

`add_index()` adds a rowwise mean index columns of the specified
variables to the dataset. The second argument (first argument if used in
a pipe) should be the name of the index column:

``` r
diamonds %>%
  add_index(meanxyz, x, y, z)
#> # A tibble: 53,940 x 11
#>    carat cut      color clarity depth table price     x     y     z meanxyz
#>    <dbl> <ord>    <ord> <ord>   <dbl> <dbl> <int> <dbl> <dbl> <dbl>   <dbl>
#>  1 0.23  Ideal    E     SI2      61.5    55   326  3.95  3.98  2.43    3.45
#>  2 0.21  Premium  E     SI1      59.8    61   326  3.89  3.84  2.31    3.35
#>  3 0.23  Good     E     VS1      56.9    65   327  4.05  4.07  2.31    3.48
#>  4 0.290 Premium  I     VS2      62.4    58   334  4.2   4.23  2.63    3.69
#>  5 0.31  Good     J     SI2      63.3    58   335  4.34  4.35  2.75    3.81
#>  6 0.24  Very Go~ J     VVS2     62.8    57   336  3.94  3.96  2.48    3.46
#>  7 0.24  Very Go~ I     VVS1     62.3    57   336  3.95  3.98  2.47    3.47
#>  8 0.26  Very Go~ H     SI1      61.9    55   337  4.07  4.11  2.53    3.57
#>  9 0.22  Fair     E     VS2      65.1    61   337  3.87  3.78  2.49    3.38
#> 10 0.23  Very Go~ H     VS1      59.4    61   338  4     4.05  2.39    3.48
#> # ... with 53,930 more rows
```

Set the argument `type = "sum"` to create a sum index instead:

``` r
diamonds %>% 
  add_index(sumxyz, x, y, z, type = "sum")
#> # A tibble: 53,940 x 11
#>    carat cut       color clarity depth table price     x     y     z sumxyz
#>    <dbl> <ord>     <ord> <ord>   <dbl> <dbl> <int> <dbl> <dbl> <dbl>  <dbl>
#>  1 0.23  Ideal     E     SI2      61.5    55   326  3.95  3.98  2.43   10.4
#>  2 0.21  Premium   E     SI1      59.8    61   326  3.89  3.84  2.31   10.0
#>  3 0.23  Good      E     VS1      56.9    65   327  4.05  4.07  2.31   10.4
#>  4 0.290 Premium   I     VS2      62.4    58   334  4.2   4.23  2.63   11.1
#>  5 0.31  Good      J     SI2      63.3    58   335  4.34  4.35  2.75   11.4
#>  6 0.24  Very Good J     VVS2     62.8    57   336  3.94  3.96  2.48   10.4
#>  7 0.24  Very Good I     VVS1     62.3    57   336  3.95  3.98  2.47   10.4
#>  8 0.26  Very Good H     SI1      61.9    55   337  4.07  4.11  2.53   10.7
#>  9 0.22  Fair      E     VS2      65.1    61   337  3.87  3.78  2.49   10.1
#> 10 0.23  Very Good H     VS1      59.4    61   338  4     4.05  2.39   10.4
#> # ... with 53,930 more rows
```

`add_label` adds a text label column of a numeric variable to the
dataset, for example to be used as labels in plots.

``` r
diamonds %>% 
  add_label(labelz, z)
#> # A tibble: 53,940 x 11
#>    carat cut       color clarity depth table price     x     y     z labelz
#>    <dbl> <ord>     <ord> <ord>   <dbl> <dbl> <int> <dbl> <dbl> <dbl> <chr> 
#>  1 0.23  Ideal     E     SI2      61.5    55   326  3.95  3.98  2.43 2.43  
#>  2 0.21  Premium   E     SI1      59.8    61   326  3.89  3.84  2.31 2.31  
#>  3 0.23  Good      E     VS1      56.9    65   327  4.05  4.07  2.31 2.31  
#>  4 0.290 Premium   I     VS2      62.4    58   334  4.2   4.23  2.63 2.63  
#>  5 0.31  Good      J     SI2      63.3    58   335  4.34  4.35  2.75 2.75  
#>  6 0.24  Very Good J     VVS2     62.8    57   336  3.94  3.96  2.48 2.48  
#>  7 0.24  Very Good I     VVS1     62.3    57   336  3.95  3.98  2.47 2.47  
#>  8 0.26  Very Good H     SI1      61.9    55   337  4.07  4.11  2.53 2.53  
#>  9 0.22  Fair      E     VS2      65.1    61   337  3.87  3.78  2.49 2.49  
#> 10 0.23  Very Good H     VS1      59.4    61   338  4     4.05  2.39 2.39  
#> # ... with 53,930 more rows
```

By default, `add_label` rounds to two decimal places. You can change
this by setting the `decimal.places` argument:

``` r
diamonds %>% 
  add_label(labelz, z, decimal.places = 0)
#> # A tibble: 53,940 x 11
#>    carat cut       color clarity depth table price     x     y     z labelz
#>    <dbl> <ord>     <ord> <ord>   <dbl> <dbl> <int> <dbl> <dbl> <dbl> <chr> 
#>  1 0.23  Ideal     E     SI2      61.5    55   326  3.95  3.98  2.43 2     
#>  2 0.21  Premium   E     SI1      59.8    61   326  3.89  3.84  2.31 2     
#>  3 0.23  Good      E     VS1      56.9    65   327  4.05  4.07  2.31 2     
#>  4 0.290 Premium   I     VS2      62.4    58   334  4.2   4.23  2.63 3     
#>  5 0.31  Good      J     SI2      63.3    58   335  4.34  4.35  2.75 3     
#>  6 0.24  Very Good J     VVS2     62.8    57   336  3.94  3.96  2.48 2     
#>  7 0.24  Very Good I     VVS1     62.3    57   336  3.95  3.98  2.47 2     
#>  8 0.26  Very Good H     SI1      61.9    55   337  4.07  4.11  2.53 3     
#>  9 0.22  Fair      E     VS2      65.1    61   337  3.87  3.78  2.49 2     
#> 10 0.23  Very Good H     VS1      59.4    61   338  4     4.05  2.39 2     
#> # ... with 53,930 more rows
```

### Analyze data

#### Correlations

`correlate()` computes correlations coefficients and p-values for all
combinations of the specified variables. By default, Pearson correlation
coefficients are computed. Set the argument `type = "spearman"` to
compute Spearman ranked correlations coefficicents instead.

``` r
diamonds %>% 
  correlate(x, y, z)
#> # A tibble: 3 x 5
#>   var1  var2  r_pearson     n p.value
#>   <chr> <chr>     <dbl> <int>   <dbl>
#> 1 x     y         0.975 53940       0
#> 2 x     z         0.971 53940       0
#> 3 y     z         0.952 53940       0
```

#### T-tests

`ttest` computes t-Tests for one grouping variable and one or more test
variables. Output statistics include descriptives, t-values, degrees of
freedom, p-values, and Cohen’s d:

``` r
diamonds %>% 
  filter(cut == "Ideal" | cut == "Fair") %>% 
  droplevels() %>% 
  ttest(cut, price, x, y, z)
#> Group 1: Fair, Group 2: Ideal
#> # A tibble: 4 x 10
#>   variable     M1     SD1     M2     SD2  deltaM     t    df         p
#>   <chr>     <dbl>   <dbl>  <dbl>   <dbl>   <dbl> <dbl> <dbl>     <dbl>
#> 1 price    4.36e3 3.56e+3 3.46e3 3.81e+3 901.     9.75 1895. 6.08e- 22
#> 2 x        6.25e0 9.64e-1 5.51e0 1.06e+0   0.739 29.5  1914. 1.34e-157
#> 3 y        6.18e0 9.56e-1 5.52e0 1.07e+0   0.663 26.6  1925. 8.22e-133
#> 4 z        3.98e0 6.52e-1 3.40e0 6.58e-1   0.581 34.5  1862. 3.75e-202
#> # ... with 1 more variable: d <dbl>
```
