% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_waldtest.R
\name{test_waldtest}
\alias{test_waldtest}
\title{runs a Wald test using the Waldtest() function of eRm.}
\usage{
test_waldtest(
  items = NULL,
  dset = NULL,
  na.rm = TRUE,
  model = NULL,
  modelType = NULL,
  splitcr = "median",
  alpha = 0.1,
  bonf = FALSE,
  estimation_param = NULL
)
}
\arguments{
\item{items}{a numeric vector containing the index numbers of the items in
dset that are used to fit the model}

\item{dset}{a data.frame containing the data}

\item{na.rm}{a boolean value. If TRUE, all cases with any NA are removed
(na.omit). If FALSE, only cases with full NA responses are removed}

\item{model}{a list of type RM, PCM or RSM (a previously fit model)
matching the value of modelType. If model is provided, this model is used.
 If NULL, a model is fit using dset and items.}

\item{modelType}{a character value defining the rasch model to fit.
Possible values: RM, PCM, RSM}

\item{splitcr}{as defined by eRm::Waldtest: Split criterion for subject
raw score splitting. median uses the median as split criterion, mean
 performs a mean-split. Optionally splitcr can also be a dichotomous
  vector which assigns each person to a certain subgroup
   (e.g., following an external criterion). This vector can be numeric,
    character or a factor.}

\item{alpha}{a numeric value for the alpha level. Will be ignored if
use.pval is FALSE}

\item{bonf}{a boolean value wheter to use a Bonferroni correction. Will be
ignored if use.pval is FALSE}

\item{estimation_param}{options for parameter estimation using
\link{estimation_control}}
}
\value{
if none of the p-values is significant (above p=0.05), a list
 containing two elements is returned: the pattern that was tested an a list
  of type RM, RCM or RSM (depending on modelType) with the fit model.
   If there is at least one item with a significant p-value,
    NULL is returned.
}
\description{
runs a Wald test using the Waldtest() function of eRm.
}
\keyword{internal}
