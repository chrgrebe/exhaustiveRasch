parallized_tests <- function(dset, modelType, combos, models, na.rm, testfunction, itemfit_param, splitcr=NULL,
                             alpha, bonf, DIFvars, gap_prop, extremes, ignoreCores, ...){
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
      cl <- parallel::makePSOCKcluster(parallel::detectCores()- ignoreCores)
    }
    parallel::setDefaultCluster(cl)
    parallel::clusterExport(cl, testfunction)
    parallel::clusterEvalQ(cl, library(eRm))
    parallel::clusterEvalQ(cl, library(psych))
    parallel::clusterEvalQ(cl, library(psychotree))

    #if (is.null(models)){
    param1 <- list(cl=cl, X=combos, dset=dset, modelType=modelType, na.rm=na.rm, fun= testfunction)
    #} else{
    #  param1 <- list(cl=cl, X=models, dset=dset, modelType=modelType, na.rm=na.rm, fun= testfunction)
    #}
    if (testfunction=="test_itemfit"){
      param1$control= itemfit_param
    }
    if (!is.null(splitcr) & (testfunction=="test_mloef" | testfunction=="test_LR" | testfunction=="test_waldtest")){
      param1$splitcr=splitcr
    }
    if (testfunction %in% c("test_mloef", "test_LR", "test_waldtest")){
      param1$alpha=alpha
      param1$bonf=bonf
    }
    if (testfunction=="test_DIFtree"){
      param1$DIFvars= DIFvars
    }
    if (testfunction=="test_personsItems"){
      param1$gap_prop= gap_prop
      param1$extremes= extremes
    }
    tim <- system.time(a <- do.call(parallel::parLapply, param1))

    parallel::stopCluster(cl)
    a[sapply(a, is.null)] <- NULL
    print(paste("Patterns that passed", testfunction, ":",length(a)))
    print(paste("--- Runtime: ", tim[3], " Sekunden", sep=""))
    return(a)
  }
}
