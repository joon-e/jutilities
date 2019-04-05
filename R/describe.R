#' Describe variables
#'
#' Descriptively describe one or more continous variable by several measures of
#' central tendency and variability.
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
#'
#' @export
describe <- function(data, ..., na.rm = T) {

  vars <- quos(...)

  data %>%
    select(!!! vars) %>%
    gather(variable, val, !!! vars) %>%
    group_by(variable) %>%
    summarise(N = n(),
              Missing = sum(is.na(val)),
              M = mean(val, na.rm = na.rm),
              SD = sd(val, na.rm = na.rm),
              Min = min(val, na.rm = na.rm),
              Max = max(val, na.rm = na.rm),
              Range = Max - Min,
              Mdn = median(val, na.rm = na.rm))

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
    summarise(N = n(),
              Missing = sum(is.na(!! var)),
              M = mean(!! var, na.rm = na.rm),
              SD = sd(!! var, na.rm = na.rm),
              Min = min(!! var, na.rm = na.rm),
              Max = max(!! var, na.rm = na.rm),
              Range = Max - Min,
              Mdn = median(!! var, na.rm = na.rm))

}

#' Describe categorial variable
#'
#' Compute frequency table for a categorial variable (includes percentages
#'  and cumulative percentages).
#'
#' @import dplyr
#'
#' @param data A (tidy) dataset
#' @param var Variable to describe
#' @param sort A logical value indicating whether categories should be sorted
#'     descending by highest value. Defaults to FALSE.
#' @return A tibble containing absolute frequencies, percentages and cumulative
#'     percentages.
#' @examples describe_cat(mtcars, cyl)
#'
#' @export
describe_cat <- function(data, var, sort = F) {

  var <- enquo(var)

  data %>%
    group_by(!! var) %>%
    count(!! var, sort = sort) %>%
    ungroup() %>%
    mutate(
      perc = n / sum(n),
      cperc = cumsum(perc)
    )

}
