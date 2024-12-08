add_ICs <- function(obj, ignoreCores=1){
  #' adds information criteria to the @IC slot of an object of
  #'  class \link{passed_exRa-class}
  #' @param obj an object of class \link{passed_exRa-class}
  #' @param ignoreCores a numeric value for the number of virtual CPU cores
  #' (threads)to hold out in computing the information criteria.
  #' @export
  #' @examples \dontrun{
  #'   passed <- exhaustive_tests(ADL[c(1,4,6,7,10,14,15)],
  #'     modelType = "RM", scale_length = 4:5, upperMSQ=1.5, lowerMSQ=0.5,
  #'     tests=c("test_mloef", "test_itemfit", "test_respca"),
  #'     estimation_param=estimation_control())
  #'   passed <- add_ICs(passed)
  #'  }


  compute_ICs <- function(obj){
    ### internal function
    if ("eRm" %in% class(obj)){
      loglik_value <- obj$loglik
      npar_value <- obj$npar
      N <- dim(obj$X)[1]
    } else if (class(obj)[1] %in% c("raschmodel", "pcmodel", "rsmodel")){
      loglik_value <- obj$loglik
      npar_value <- length(obj$coefficients)
      N <- dim(obj$data)[1]
    } else if ("pair" %in% class(obj)){
      loglik_value <- as.numeric(logLik.pers(pairwise::pers(obj)))
      npar_value <- sum(obj$pair$m-1)*2-1
      N <- dim(obj$resp)[1]
    }

    if (exists("loglik_value") & exists("npar_value") & exists("N")){
      AIC <- round(-2*loglik_value + 2*npar_value, digits=3)
      BIC <- round(-2*loglik_value + log(N)*npar_value, digits=3)
      cAIC <- round(-2*loglik_value + log(N)*npar_value + npar_value, digits=3)
      return(c(loglik_value, AIC, BIC, cAIC))
    }
  }

  if (!inherits(obj,"passed_exRa")){
    stop("add_ICs() requires an object of class passed_exRa")
  }

  if (isTRUE(as.logical(Sys.getenv("_R_CHECK_LIMIT_CORES_")))) {
    # on cran
    ncores <- 2L    # use 2 cores in CRAN/Travis/AppVeyor
    cl <- parallel::makePSOCKcluster(2L)
  } else {
    # use all cores in devtools::test()
    cl <- parallel::makePSOCKcluster(parallel::detectCores()- ignoreCores)
  }

  parallel::setDefaultCluster(cl)

  information_criteria <- parallel::parLapply(cl,
                        seq_len(length(obj@passed_models)),
                        function(x) compute_ICs(
                          obj@passed_models[[x]]))
  obj@IC <- as.data.frame(do.call(rbind, information_criteria))
  colnames(obj@IC) <- c("loglik", "AIC", "BIC", "cAIC")
  return(obj)
}
