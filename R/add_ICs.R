add_ICs <- function(obj, ignoreCores=1){
  #' adds information criteria to the @IC slot of an object of
  #'  class \link{passed_exRa-class}
  #' @param obj an object of class \link{passed_exRa-class}
  #' @param ignoreCores a numeric value for the number of virtial CPU cores
  #' (threads)to hold out in computing the information criteria.

  if (!inherits(obj,"passed_exRa")){
    stop("add_ICs() requires an object of class passed_exRa")
  }

  chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  if (nzchar(chk) && chk == "TRUE") {
    # use 2 cores in CRAN/Travis/AppVeyor
    cl <- parallel::makePSOCKcluster(2L)
  } else {
    # use all cores in devtools::test()
    cl <- parallel::makePSOCKcluster(parallel::detectCores()- ignoreCores)
  }

  parallel::setDefaultCluster(cl)
  parallel::clusterEvalQ(cl, library(eRm))


  information_criteria <- parallel::parLapply(cl,
                                              seq_len(length(obj@passed_models)),
                                              function(x) eRm::IC(
                                                eRm::person.parameter(obj@passed_models[[x]]))$ICtable[3,3:5])
  obj@IC <- as.data.frame(do.call(rbind, information_criteria))
  return(obj)
}
