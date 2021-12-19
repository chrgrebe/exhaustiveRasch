parallized_tests <- function(dset, modelType="RM", combos, testfunction, itemfit_param, ...){
parallized_tests <- function(dset, modelType="RM", combos, testfunction, itemfit_param, splitcr=NULL, ...){
  arguments <- list(...)
  # abfangen, wenn keine Pattern oder eine Warnmeldung als character ?bergeben wurden

  if (length(combos)==0 | is.character(combos)){
    warning(paste("No patterns left to perform ", testfunction, ". Aborted.", sep=""))
  } else{
      chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
      if (nzchar(chk) && chk == "TRUE") {
      # use 2 cores in CRAN/Travis/AppVeyor
      cl <- parallel::makePSOCKcluster(2L)
    } else {
      # use all cores in devtools::test()
      cl <- parallel::makePSOCKcluster(parallel::detectCores())
    }


    cl <- parallel::makePSOCKcluster(parallel::detectCores())
    parallel::setDefaultCluster(cl)
    parallel::clusterExport(cl, testfunction)
    parallel::clusterEvalQ(cl, library(eRm))
    parallel::clusterEvalQ(cl, library(psych))

    if (testfunction=="test_itemfit"){
      param1 <- list(cl=cl, dset=dset, X=combos, fun= testfunction)
      param1$control= itemfit_param
      #if (exists("modelle")){modelle=NULL}
      param1$modelType=modelType
      param1$dset=dset
      tim <- system.time(a <- do.call(parallel::parLapply, param1))
    } else{
      if (!is.null(splitcr) & (testfunction=="test_mloef" | testfunction=="test_LR")){
        tim <- system.time(a <- parallel::parLapply(cl=cl, X=combos, fun=testfunction, dset=dset, modelType=modelType, splitcr=splitcr))
      } else{
        tim <- system.time(a <- parallel::parLapply(cl=cl, X=combos, fun=testfunction, dset=dset, modelType=modelType))
      }
    }

    parallel::stopCluster(cl)
    a[sapply(a, is.null)] <- NULL
    print(paste("Patterns that passed", testfunction, ":",length(a)))
    print(paste("--- Runtime: ", tim[3], " Sekunden", sep=""))
    return(a)
  }
}
