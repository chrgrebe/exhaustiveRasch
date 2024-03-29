% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_combo_rules.R
\name{check_combo_rules}
\alias{check_combo_rules}
\title{selects item combinations based on defined rules}
\usage{
check_combo_rules(full, rules = NULL)
}
\arguments{
\item{full}{a numeric vector containing the the source for the
combinations, typically the indices of the items in the referring dataset}

\item{rules}{a list defining rules for combination selection}
}
\value{
a list of numeric vectors containing the selected item combinations
 that match the defined rules of forced_items and/or rules.
}
\description{
selects item combinations based on defined rules
}
\keyword{internal}
