% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modify.R
\name{add_label}
\alias{add_label}
\title{Add text label column}
\usage{
add_label(data, name, labelVar, decimal.places = 2)
}
\arguments{
\item{data}{A (tidy) dataset.}

\item{name}{Name of the label column to add.}

\item{labelVar}{Name of the variable to turn into a text label.}

\item{decimal.places}{Number of decimal places to round the text label.
Defaults to 2.}
}
\value{
A tibble of the original dataset with the label column added.
}
\description{
Add a text label column of a numeric variable to the dataset (e.g., to use
as labels in a plot).
}
