% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tests_psychotools.R
\name{ppar.psy}
\alias{ppar.psy}
\title{itemfit statistics for psychotools}
\usage{
ppar.psy(model = NULL)
}
\arguments{
\item{model}{a list of type RM, PCM or RSM (a previously fit model)
matching the value of modelType. If model is provided, this model is used.
 If NULL, a model is fit using dset and items.}
}
\value{
an object containing person parameters, residuals and PSI
}
\description{
itemfit statistics for psychotools
}
\examples{
\dontrun{
 model <- psychotools::raschmodel(ADL[c(6,7,12,14,15)])
 ppar.psy(model)
 }
}
