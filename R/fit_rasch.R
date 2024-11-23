fit_rasch <- function(X, modelType, estimation_param, pair_param){
  #' parameter estimation for for rasch models.
  #' @param X a numeric vector containing the index numbers of the items
  #'  in dset that are used to fit the model
  #' @param modelType a character value defining the rasch model to fit.
  #'  Possible values: RM, PCM, RSM
  #' @param estimation_param options for parameter estimation using
  #' \link{estimation_control}
  #' @param pair_param options for options for fitting pairwise models using
  #' \link{pairwise_control}
  #' @return if est=eRm was used: an object of the respecting classes RM, PCM
  #' or RSM of the eRm package (fit rasch models);
  #' if est=psychotools was used: a reduced list of model parameters with the
  #' same names as they are used in the respective eRm object (X, betapar,
  #' etapar, loglik, se.beta, X01, model), but estimated using the respective
  #' function of psychotools.
  #' @export
  #' @keywords internal

  # This function implements the respective estimation of the item parameters,
  # depending on the estimation method and other arguments set in the
  # estimation_param argument. It is an internal function, a call by the user is
  # not indicated. It is nevertheless exported in order to work in
  # parallelization. However, it is not included in the package documentation
  # (roxygen2 keyword 'internal').

  #estimation_param$se=F
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
      mod <- get(modelType,envir = loadNamespace("eRm"))(X, se=estimation_param$se,
                            sum0=estimation_param$sum0)
    }), silent=TRUE)
  } else if (estimation_param$est=="psychotools"){
    if (modelType=="RM"){
      mod <- psychotools::raschmodel(X, hessian=T)
    } else if (modelType=="PCM"){
      mod <- psychotools::pcmodel(X, hessian=T, nullcats="ignore")
    } else if (modelType=="RSM"){
      mod <- psychotools::rsmodel(X, hessian=T)
    }
    mod$thresholds  <- psychotools::threshpar(mod, type="mode")
    names(mod[[length(mod)]]) <- "thresholds"
    #if (is.na(mod$vcov[1])){mod <- NULL}
  } else{ # pairwise model
    try(suppressWarnings({
      mod <- pairwise::pair(daten=X, m=pair_param$m)
    }), silent=TRUE)

  }

  return(mod)
}
