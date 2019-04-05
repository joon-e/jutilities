#' Add index
#'
#' Add a rowwise mean or sum index of specific variables to the dataset.
#'
#' @import dplyr
#'
#' @param data A (tidy) dataset.
#' @param name Name of the index column to compute.
#' @param ... Variables used for the index.
#' @param type Type of index to compute. Either "mean" (default) or "sum".
#' @param na.rm a logical value indicating whether NA values should be stripped
#' before the computation proceeds. Defaults to TRUE.
#'
#' @return A tibble of the original datset with the added index column.
#'
#' @export
add_index <- function(data, name, ..., type = "mean", na.rm = T) {

  name <- quo_name(enquo(name))
  indexVars <- quos(...)

  data %>%
    rowwise() %>%
    mutate(
      !! name := ifelse(type == "sum",
                        sum(c(!!! indexVars), na.rm = na.rm),
                        mean(c(!!! indexVars), na.rm = na.rm))
    ) %>%
    ungroup()
}

#' Add text label column
#'
#' Add a text label column of a numeric variable to the dataset (e.g., to use
#' as labels in a plot).
#'
#' @import dplyr
#'
#' @param data A (tidy) dataset.
#' @param name Name of the label column to add.
#' @param labelVar Name of the variable to turn into a text label.
#' @param decimal.places Number of decimal places to round the text label.
#' Defaults to 2.
#'
#' @return A tibble of the original dataset with the label column added.
#'
#' @export
add_label <- function(data, name, labelVar, decimal.places = 2) {

  name <- quo_name(enquo(name))
  labelVar <- enquo(labelVar)

  data %>%
    mutate(
      !! name := trimws(
        format(
          round(!! labelVar, decimal.places), nsmall = decimal.places)
      )
    )

}
