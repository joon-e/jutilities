#' Compute frequency table
#'
#' Computes frequency table for one categorical variable, including relative,
#' valid, and cumulative frequencies.
#'
#' @import dplyr
#'
#' @param data A (tidy) dataset
#' @param var Variable to describe
#'
#' @return A tibble of the frequency table
#'
#' @export
cat_ftable <- function(data, var) {

  var <- enquo(var)
  n.na <- data %>%
    pull(!! var) %>%
    is.na %>%
    sum

  d <- data %>%
    group_by(!! var) %>%
    summarise(n = n()) %>%
    arrange(!! var) %>%
    mutate(percent = n / sum(n),
           valid.percent = ifelse(!is.na(!! var),
                                  n / (sum(n) - n.na),
                                  0),
    )

  d %>%
    bind_cols(d %>%
                select(cum.n = n,
                       cum.percent = percent,
                       cum.valid.percent = valid.percent) %>%
                mutate_all(cumsum)
    ) %>%
    mutate(cum.valid.percent = ifelse(is.na(!! var), 0, cum.valid.percent)) %>%
    select(!! var, n, cum.n, percent, cum.percent,
           valid.percent, cum.valid.percent)
}

#' Compute contigency table
#'
#' Computes contingency table for one independent (column) variable and one
#' or more dependent (row) variables. Optionally computes Chi² test.
#'
#' @import dplyr
#'
#' @param data A (tidy) dataset
#' @param col.var Independent (column) variable.
#' @param ... Dependt (row) variable(s).
#' @param percentages Logical indicating whether to output column-wise
#'     percentages instead of absolute values. Defaults to FALSE.
#' @param chisq Logical indicating whether a Chi² test should be computed. Test
#'     will be reported via message(). Defaults to FALSE.
#'
#' @return A tibble with the contingency table.
#'
#' @export
cat_xtable <- function(data, col.var, ..., percentages = FALSE, chisq = FALSE) {

  var <- enquo(col.var)
  vars <- quos(...)

  var.lvls <- data %>%
    pull(!! var) %>%
    as.factor() %>%
    levels()

  xt <- data %>%
    group_by(!! var, !!! vars) %>%
    count() %>%
    spread(!! var, n) %>%
    ungroup()

  group.vars <- xt %>%
    select(!!! vars)

  counts <- xt %>%
    select(var.lvls) %>%
    modify(~ `attr<-`(., "var.type", "count")) %>%
    mutate(total = rowSums(.))

  if (percentages) {
    counts <- counts %>%
      mutate_all(~ . / sum(., na.rm = T))
  }

  if (chisq) {
    chi2 <- counts %>%
      chisq_test(tidy = F)

    s <- "Chi² = %f, df = %f, p = %f"

    message(sprintf(s, chi2$statistic, chi2$parameter, chi2$p.value))
  }

  group.vars %>%
    bind_cols(counts)
}

#' Chi² test
#'
#' Computes Chi² test from contigency table. To be used following cat_xtable.
#'
#' @import dplyr
#'
#' @param data A contigency table.
#' @param tidy Logical indicating whether output should be tidied. If FALSE,
#'     outputs the standard output of chisq.test(). Defaults to TRUE.
#'
#' @return Results of the Chi² test.
#'
#' @export
chisq_test <- function(data, tidy = T) {

  df <- data %>%
    select_if(~ "count" %in% attributes(.))

  if (length(df) == 0) {
    df <- data
  }

  chi2 <- df %>%
    as.matrix() %>%
    chisq.test()

  if (tidy) {
    chi2 <- chi2 %>%
      broom::tidy()
  }

  return(chi2)
}
