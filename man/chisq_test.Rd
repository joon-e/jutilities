% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cat.R
\name{chisq_test}
\alias{chisq_test}
\title{Chi² test}
\usage{
chisq_test(data, tidy = T)
}
\arguments{
\item{data}{A contigency table.}

\item{tidy}{Logical indicating whether output should be tidied. If FALSE,
outputs the standard output of chisq.test(). Defaults to TRUE.}
}
\value{
Results of the Chi² test.
}
\description{
Computes Chi² test from contigency table. To be used following cat_xtable.
}
