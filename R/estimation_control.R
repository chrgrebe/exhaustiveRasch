estimation_control <- function(est="psychotools",
                               se=TRUE,
                               sum0=TRUE){
  #' options for item parameter estimation
  #' @param est a character value defining the estimation function to use.
  #'  Possible values: 'psychotools', 'eRm'.
  #' @param se a boolean value defining if hessian values and standard errors
  #' for the item parameter estimates are to be computed.
  #' @param sum0 a boolean value defining if item parameters are to be
  #' computed in a way that their sum is 0. This parameter is only meaninful, if
  #' the 'est' parameter is 'eRm". Estimates computed using 'psychotools' are
  #' always computed by setting the first parameter 0.
  #' @return a list containing the options
  #' @export

  return(list("est"= est, "se"= se, "sum0"= sum0))
}
