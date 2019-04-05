#' Create codings matrix
#'
#' Creates a coders x codings matrix for a test variable that can be passed to
#' irr functions.
#'
#' @import dplyr
#'
#' @param data A (tidy) dataset
#' @param unitVar A unit (e.g., an article) identifier variable.
#' @param coderVar A coder identifier variable.
#' @param testVar The variable to be tested.
#'
#' @return A coders x codings matrix.

irr.matrix = function(data, unitVar, coderVar, testVar) {

  unitVar = enquo(unitVar)
  coderVar = enquo(coderVar)
  testVar = enquo(testVar)

  var.matrix = data %>%
    select(!! unitVar, !! coderVar, !! testVar) %>%
    spread(!! coderVar, !! testVar) %>%
    select(-1) %>%
    data.matrix()

  return(var.matrix)
}


#' Compute interreliability test
#'
#' Computes an interreliability test for a specific test variable.
#'
#' @import dplyr
#'
#' @inheritParams irr.matrix
#'
#' @param method Data level of the test variable ("nominal", "ordinal",
#' "interval" or "ratio"). Defaults to "nominal".
#'
#' @return A tibble containing various interreliability measures.
#'
irr.test = function(data, unitVar, coderVar, testVar, method = "nominal") {

  unitVar = enquo(unitVar)
  coderVar = enquo(coderVar)
  testVar = enquo(testVar)

  test.matrix <- irr.matrix(data, !! unitVar, !! coderVar, !! testVar)

  test.alpha <- test.matrix %>%
    t %>%
    irr::kripp.alpha(method)

  test.agree <- test.matrix %>%
    irr::agree()

  test.df = tibble(
    var.name = quo_name(testVar),
    alpha = test.alpha$value,
    agree = test.agree$value,
    n.coders = test.alpha$raters,
    n.units = test.alpha$subjects,
    type = method
  )

  return(test.df)
}


#' Compute interreliability tests
#'
#' Computes interreliability tests for specified variables.
#'
#' @import dplyr
#'
#' @param data A (tidy) dataset.
#' @param unitVar A unit (e.g., an article) identifier variable.
#' @param coderVar A coder identifier variable.
#' @param ... The variables to be tested.
#' @param method The data level of the test variables ("nominal", "ordinal",
#' "interval" or "ratio"). Different data levels may be passed as a vector the
#' length of the amount of test variables.
#'
#' @return A tibble containing various interreliability measures and statistics
#' for the test variables.

irr = function(data, unitVar, coderVar, ..., method = "nominal") {

  unitVar <- enquo(unitVar)
  coderVar <- enquo(coderVar)

  df <- NULL
  i <- 1

  for (testVar in quos(...)) {

    if (length(method) == 1) {
      current.method <- method
    } else {
      current.method <- method[i]
      i <- i + 1
    }

    df <- df %>%
      bind_rows(irr.test(data, !! unitVar, !! coderVar, !! testVar, current.method))
  }

  return(df)
}
