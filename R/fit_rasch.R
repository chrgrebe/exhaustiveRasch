fit_rasch <- function(X, modelType, estimation_param){
  #' parameter estimation for for rasch models.
  #' @param X a numeric vector containing the index numbers of the items
  #'  in dset that are used to fit the model
  #' @param modelType a character value defining the rasch model to fit.
  #'  Possible values: RM, PCM, RSM
  #' @param est a character value defining the estimation function to use
  #'  (psychtools or eRm for using the estimation function of the respective
  #'  package).
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
      mod <- get(modelType)(X, se=estimation_param$se,
                            sum0=estimation_param$sum0)
    }), silent=TRUE)
  } else{
    a <- try(suppressWarnings({
      XWcheck <- datcheck(X,NA,1,1,modelType)
      X <- XWcheck$X
    }), silent=TRUE)

    if (!"try-error" %in% class(a)){
      if (modelType=="RM"){
        psytool_mod <- psychotools::raschmodel(XWcheck$X, hessian=estimation_param$se)
        classtype <- c("dRm","Rm","eRm")
      } else if (modelType=="PCM"){
        psytool_mod <- psychotools::pcmodel(XWcheck$X, hessian=estimation_param$se,
                                            nullcats="ignore")
        classtype <- c("Rm","eRm")
      } else if (modelType=="RSM"){
        psytool_mod <- psychotools::rsmodel(XWcheck$X, hessian=estimation_param$se)
        classtype <- c("dRm","Rm","eRm")
        # hessian=F to avoid unnecessary comptations; nullcats="ignore" for
        # eRm-like behaviour in the case of missing categories
      }
      datprep <- dataprep(psytool_mod$data,1,F, modelType=modelType)
      etapar <- psytool_mod$coefficients
      betapar <- c(0,0-etapar)
      npar=psytool_mod$df
      g <-names(etapar)
      names(betapar) <- c(paste(substr(g[1],1, nchar(g[1])-1), as.character(as.numeric(substr(g[1],nchar(g[1]), nchar(g[1])))-1),sep=""),g)
      names(betapar) <- paste0("beta ", chartr("C", "c", chartr("-", ".", names(betapar))))
      names(etapar) <- chartr("C", "c", chartr("-", ".", names(etapar)))
      rownames(datprep$W) <- names(betapar)
      colnames(datprep$W) <- paste0("eta ", 1:npar)
      loglik <- psytool_mod$loglik
      call <- "assembled eRm object from exhaustiveRasch based on parameter
         estimates using package psychotools"
      iter= as.numeric(psytool_mod$iterations)
      if (estimation_param$se==T){
        se.beta <- as.numeric(c(0, sqrt(diag(psytool_mod$vcov))))
        se.eta <- as.numeric(se.beta[-1])
        hessian <- chol2inv(chol(psytool_mod$vcov))
      }else{
        hessian=NULL
        se.beta <- rep(NA,npar) # NA because Hessians are not calculated
        se.eta <- rep(NA,npar+1) # NA because Hessians are not calculated
      }
      X <- as.matrix(datprep$X)
      model <- modelType
      mod <- list(X=X, X01=datprep$X01, model=model, loglik=loglik, npar=npar,
                  iter=iter, convergence=NULL, etapar=etapar, se.eta=se.eta,
                  hessian=hessian, betapar=betapar, se.beta=se.beta,
                  W=datprep$W, call=call)
      class(mod) <- classtype
    }
  }

  return(mod)
}
