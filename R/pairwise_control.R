pairwise_control <- function(resp.cat=NULL,
                             use.thurst=T
){
  #' options for fitting pairwise models
  #' @param resp.cat number of response (answer) categories for all items. If not
  #' given, they will be calculated from the data, assuming that every response
  #' category is at least once present in the data.
  #' @param use.thurst a boolean value defining whether thurstonian threshold
  #' parameters (TRUE, default) or Rasch-Andrich thresholds (step parameters)
  #' will be computed.
  #' @param use.rel a boolean value defining if unweighted (default, FALSE) or
  #' weighted item fit indices will be used.
  #' @return a list containing the options
  #' @export

  return(list("resp.cat"= resp.cat, "use.thurst"= use.thurst))
}
