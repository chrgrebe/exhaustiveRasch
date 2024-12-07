estimation_control <- function(est="psychotools",
                               resp.cat=NULL,
                               use.thurst=T,
                               splitseed=NULL){
  #' options for item parameter estimation
  #' @param est a character value defining the estimation function to use.
  #'  Possible values: 'psychotools', 'eRm'.
  #' @param resp.cat number of response (answer) categories for all items.
  #' If not given, they will be calculated from the data, assuming that
  #' every response category is at least once present in the data. Currently
  #' only used for pairwise estimations (m parameter in pairwise::pair).
  #' @param use.thurst a boolean value defining whether thurstonian threshold
  #' parameters (TRUE, default) or Rasch-Andrich thresholds (step parameters)
  #' will be computed.
  #' @param splitseed a numeric value, the seed for random splits in test_waldtest,
  #' lest_LR (both only relevant for pairwise or psychotools estimations) and
  #' test_mloef (only psyhotools estimations)
  #' @return a list containing the options
  #' @export

  return(list("est"= est, "resp.cat"= resp.cat, "use.thurst"= use.thurst,
              "splitseed"= splitseed))
}
