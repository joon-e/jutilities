% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/irr.R
\name{irr}
\alias{irr}
\title{Compute interreliability tests}
\usage{
irr(data, unitVar, coderVar, ..., method = "nominal")
}
\arguments{
\item{data}{A (tidy) dataset.}

\item{unitVar}{A unit (e.g., an article) identifier variable.}

\item{coderVar}{A coder identifier variable.}

\item{...}{The variables to be tested.}

\item{method}{The data level of the test variables ("nominal", "ordinal",
"interval" or "ratio"). Different data levels may be passed as a vector the
length of the amount of test variables.}
}
\value{
A tibble containing various interreliability measures and statistics
for the test variables.
}
\description{
Computes interreliability tests for specified variables.
}
