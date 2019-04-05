#' Color scales
#'
#' A variety of discreet color scales to be used in ggplot as a value in
#' scale_color_manual().
#' Currently, the following scales are supported:
#' - "cb": Colorblind-friendly scale.
#' - "cb2": Alternative colorblind-friendly scale.
#'
#' @param scale Name of the color scale. Defaults to "cb".
#'
#' @return A vector containing the color scale (hex values).
#'
#' @references \url{http://jfly.iam.u-tokyo.ac.jp/color/}
#'
#' @export
color_scale <- function(scale = "cb") {

  # Color-blind scale
  if (scale %in% c("cb", "colorblind", "colourblind", "color-blind", "colour-blind")) {
    colors <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  }

  # Alternative color-blind scale
  if (scale %in% c("cb2", "cb alt")) {
    colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  }

  return(colors)
}

#' Label text size multiplier
#'
#' Multiplier for text size in geom_text() to correspond with element_text().
#'
#' @param size Text size in default units to be used in geom_text().
#'
#' @return The desired text size in points.
#'
#' @references \url{https://stackoverflow.com/questions/17311917/ggplot2-the-unit-of-size}
#'
#' @export
label_text <- function(size = 12) size * 0.352777778

#' Plot theme
#'
#' ggplot2 theme based on theme_minimal()
#'
#' @import ggplot2
#' @import dplyr
#'
#' @param base_size Base size for text elements. Defaults to 12.
#' @param base_family Base family for text elements. Defaults to "sans".
#'
#' @return Theme to be added to ggplot2 object.
#'
#' @export
theme_ju <- function(base_size = 12, base_family = "sans") {
  theme_minimal(base_family = base_family,
                base_size = base_size) %+replace%
    theme(

      # Text
      axis.text = element_text(size = base_size),
      legend.text = element_text(size = base_size),
      strip.text = element_text(size = base_size),
      plot.title = element_text(hjust = 0.5, size = base_size),

      # Grid
      panel.grid.minor = element_blank(),

      # Legend
      legend.position = "bottom"
    )
}
