#' Compute correlation matrix
#'
#' Computes correlations coefficients with p-values for all specified variables.
#'
#' @import dplyr
#'
#' @param data A (tidy) dataset.
#' @param ... Variables to correlate with eachother.
#' @param type Type of correlation coefficient. Either "pearson" or "spearman".
#'
#' @return A tibble containing correlation coefficients.
#'
#' @export
correlate <- function(data, ..., type = "pearson") {

  vars <- quos(...)

  estimate = paste("r", type, sep = "_")

  data %>%
    select(!!! vars) %>%
    as.matrix %>%
    Hmisc::rcorr(type = type) %>%
    broom::tidy() %>%
    rename(var1 = column1,
           var2 = column2,
           !! estimate := estimate)
}
