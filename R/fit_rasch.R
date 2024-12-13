fit_rasch <- function(X, modelType, estimation_param){
  # This is an internal function that is not intended to be called by users.
  # It is nevertheless exported so that it can be run in the parallelization
  # workers. However, the function is not documented in the manual.

  #' parameter estimation for for rasch models.
  #' @param X a numeric vector containing the index numbers of the items
  #'  in dset that are used to fit the model
  #' @param modelType a character value defining the rasch model to fit.
  #'  Possible values: RM, PCM, RSM
  #' @param estimation_param options for parameter estimation using
  #' \link{estimation_control}
  #' @return a model of the class depending of modelType and
  #' estimation_param$est.If no model can be fit, NULL is returened.
  #' @export
  #' @keywords internal

  # This function implements the respective estimation of the item parameters,
  # depending on the estimation method and other arguments set in the
  # estimation_param argument. It is an internal function, a call by the user is
  # not indicated. It is nevertheless exported in order to work in
  # parallelization. However, it is not included in the package documentation
  # (roxygen2 keyword 'internal').

  #estimation_param$se=FALSE
  #  if(!estimation_param$est=="eRm" & modelType=="RM"){
  #    estimation_param$est="eRm"
  #    warning("Estimation using functions from package 'psychotools' is
  #      currently only supported for PCM and RSM models. Argument 'est' was set to
  #      'eRm'. Parameter estimations used functions of package eRm.",
  #            "\n")
  #  }

  mod <- NULL
  if (estimation_param$est=="eRm"){
    try(suppressWarnings({
      mod <- get(modelType,envir = loadNamespace("eRm"))(X, se=TRUE,
                            sum0=TRUE)
    }), silent=TRUE)
  } else if (estimation_param$est=="psychotools"){
    if (modelType=="RM"){
      mod <- psychotools::raschmodel(X, hessian=TRUE)
    } else if (modelType=="PCM"){
      mod <- psychotools::pcmodel(X, hessian=TRUE, nullcats="ignore")
    } else if (modelType=="RSM"){
      mod <- psychotools::rsmodel(X, hessian=TRUE)
    }
    mod$thresholds  <- psychotools::threshpar(mod, type="mode")
    names(mod[[length(mod)]]) <- "thresholds"
    #if (is.na(mod$vcov[1])){mod <- NULL}
  } else{ # pairwise model
    try(suppressWarnings({
      mod <- pairwise::pair(daten=X, m=estimation_param$resp.cat)
    }), silent=TRUE)

  }

  return(mod)
}
