#' Provide summary functions
#'
#' Returns summary functions for describe functions
#'
#' @import dplyr
#'
#' @param var Variable that holds values to summarize
#' @param na.rm a logical value indicating whether NA values should be stripped
#'     before the computation proceeds, passed on from the describe functions.
#'
#' @return A list of summary functions to be used in summarise().
summary <- function(var, na.rm) {
  s <- list(
    N = quo(n()),
    Missing = quo(sum(is.na(!! var))),
    M = quo(mean(!! var, na.rm = !! na.rm)),
    SD = quo(sd(!! var, na.rm = !! na.rm)),
    Min = quo(min(!! var, na.rm = !! na.rm)),
    Max = quo(max(!! var, na.rm = !! na.rm)),
    Range = quo(Max - Min),
    Mdn = quo(median(!! var, na.rm = !! na.rm)),
    Q2.5 = quo(quantile(!! var, .025, na.rm = !! na.rm)),
    Q97.5 = quo(quantile(!! var, .975, na.rm = !! na.rm))
  )
  return(s)
}


#' Describe variables
#'
#' Descriptively describe variables by several measures of
#' central tendency and variability. If no variables are specified,
#' all numeric (integer or double) variables are described.
#'
#' @import dplyr
#' @import tidyr
#'
#' @param data A (tidy) dataset.
#' @param ... Variables to describe (column names)
#' @param na.rm a logical value indicating whether NA values should be stripped
#'     before the computation proceeds. Defaults to TRUE.
#'
#' @return A tibble containing descriptive statistics for the selected
#'     variables
#'
#' @examples describe(mtcars, disp, hp, mpg)
#'   describe(iris)
#'
#' @export
describe <- function(data, ..., na.rm = T) {
  vars <- quos(...)

  if (length(vars) == 0) {
    vars <- data %>%
      select_if(~ is.double(.) || is.integer(.)) %>%
      names %>%
      syms
  }

  data %>%
    select(!!! vars) %>%
    gather(variable, val, !!! vars) %>%
    group_by(variable) %>%
    summarise(!!! summary(quo(val), na.rm))

}

#' Describe groups by variable
#'
#' Describe one or more groups by descriptive statistics for one continous
#' variable.
#'
#' @import dplyr
#'
#' @param data A (tidy) dataset
#' @param var Variable to describe by (column name)
#' @param ... Variable(s) to group by (column name)
#' @param na.rm a logical value indicating whether NA values should be stripped
#' before the computation proceeds. Defaults to TRUE.
#'
#' @return A tibble containing descriptive statistics.
#'
#' @examples describe(mtcars, mpg, cyl, am)
#'
#' @export
describe_groups <- function(data, var, ..., na.rm = T) {

  var <- enquo(var)
  groupVars <- quos(...)

  data %>%
    select(!! var, !!! groupVars) %>%
    group_by(!!! groupVars) %>%
    summarise(!!! summary(var, na.rm))

}
