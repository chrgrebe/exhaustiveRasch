% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_respca.R
\name{test_respca}
\alias{test_respca}
\title{runs a principal component analysis (PCA) on the residuals of the
 rasch model.}
\usage{
test_respca(
  items = NULL,
  dset = NULL,
  na.rm = TRUE,
  model = NULL,
  modelType = NULL,
  max_contrast = 1.5,
  estimation_param = NULL
)
}
\arguments{
\item{items}{a numeric vector containing the index numbers of the items
in dset that are used to fit the model}

\item{dset}{a data.frame containing the data}

\item{na.rm}{a boolean value. If TRUE, all cases with any NA are removed
(na.omit). If FALSE, only cases with full NA responses are removed}

\item{modelType}{a character value defining the rasch model to fit.
Possible values: RM, PCM, RSM}

\item{max_contrast}{a numeric value defining the maximum loading of a
factor in the principal components analysis of the standardised residuals.}

\item{estimation_param}{options for parameter estimation using
\link{estimation_control}}
}
\value{
if the maximum eigenvalue of the contrasts of the pca
 is < max_contrast a   #'  list containing two elements is returned:
 the item combination that was tested and a list of type RM, RCM or RSM
 (depending on modelType) with the fit model. Else, NULL is returned.
}
\description{
runs a principal component analysis (PCA) on the residuals of the
 rasch model.
}
\keyword{internal}
