estimation_control <- function(est="psychotools",
                               resp.cat=NULL,
                               use.thurst=T){
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
  #' @return a list containing the options
  #' @export

  return(list("est"= est, "resp.cat"= resp.cat, "use.thurst"= use.thurst))
}
