#' Calculate Cohen's d
#'
#' Calculates Cohen's d for a t-Test
#'
#' @param M1 Mean of Group 1
#' @param M2 Mean of Group 2
#' @param SD1 Standard deviation of Group 1
#' @param SD2 Standard deviation of Group 2
#'
#' @return Absolute value of Cohen's d
cohensD <- function(M1, M2, SD1, SD2) {
  d = (M1 - M2) / sqrt((SD1^2 + SD2^2) / 2)
  return(abs(d))
}

#' Compute t-Test
#'
#' Computes t-Tests for one grouping variable and one or more test variables.
#'
#' @import dplyr
#' @import tidyr
#'
#' @param data A (tidy) dataset.
#' @param groupVar Grouping variable
#' @param ... Test variables.
#'
#' @return A tibble containing test statistics for all test variables.
#'
#' @export
ttest <- function(data, groupVar, ...) {

  groupVar <- enquo(groupVar)
  testVars <- quos(...)

  test.groups <- data %>%
    select(!! groupVar) %>%
    pull %>%
    as.factor %>%
    levels()

  message(paste("Group 1: ", test.groups[1],
                ", Group 2: ", test.groups[2],
                sep = ""))

  data %>%
    select(!! groupVar, !!! testVars) %>%
    gather(variable, value, -!! groupVar) %>%
    group_by(!! groupVar, variable) %>%
    summarise(value = list(value)) %>%
    spread(!! groupVar, value) %>%
    rename("gr1" = 2, "gr2" = 3) %>%
    group_by(variable) %>%
    mutate(
      M1 = mean(unlist(gr1)),
      SD1 = sd(unlist(gr1)),
      M2 = mean(unlist(gr2)),
      SD2 = sd(unlist(gr2)),
      deltaM = M1 - M2,
      t = t.test(unlist(gr1), unlist(gr2))$statistic,
      df = t.test(unlist(gr1), unlist(gr2))$parameter,
      p = t.test(unlist(gr1), unlist(gr2))$p.value,
      d = cohensD(M1, M2, SD1, SD2)
    ) %>%
    select(-2, -3)
}
