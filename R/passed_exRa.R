#' class passed_exRa, an S4 class representing
#'  an a result of the exhaustive_tests function
#' @slot process a data.frame containg process information
#'  from the call to exhaustive_tests
#' @slot passed_combos a list of vectors containing item combinations
#'  using the indices of the items
#' @slot passed_models a list of objects of the classes RM, PCM or RSM
#'  of the eRm package (fit rasch models)
#' @slot passed_p.par a list of objects containing the person parameters.
#'  Depending on the package used for estimation, also residuals and/or
#'  the PSI value can be part of this list
#' @slot IC a data.frame conatining information criteria for each of the
#'  models in passed_models
#' @slot data a data.frame containing the data used for the analyes.
#' @slot timings a data.frame containing the the timings of the analyses.
#' @export
methods::setClass("passed_exRa", slots=c(process="data.frame",
                                         passed_combos="list",
                                         passed_models="list",
                                         passed_p.par="list",
                                         IC="data.frame",
                                         data="data.frame",
                                         timings="data.frame"))
