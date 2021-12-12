parallized_tests <- function(dset, modelType="RM", combos, testfunction,...){
  arguments <- list(...)
  # abfangen, wenn keine Pattern oder eine Warnmeldung als character uebergeben wurden

  if (length(combos)==0 | is.character(combos)){
    warning(paste("No patterns left to perform ", testfunction, ". Aborted.", sep=""))
  } else{
    cl <- makePSOCKcluster(detectCores())
    setDefaultCluster(cl)
    clusterExport(cl, testfunction)
    clusterEvalQ(cl, library(eRm))
    clusterEvalQ(cl, library(psych))

    if (testfunction=="test_itemfit"){
      param1 <- list(cl=cl, dset=dset, X=combos, fun= testfunction)
      if (!is.null(arguments$lower)){param1$lower= arguments$lower}
      if (!is.null(arguments$upper)){param1$upper= arguments$upper}
      #if (exists("modelle")){modelle=NULL}
      if (!is.null(arguments$p.val)){param1$p.val= arguments$p.val}
      param1$modelType=modelType
      param1$dset=dset
      tim <- system.time(a <- do.call(parLapply, param1))
    } else{
      tim <- system.time(a <- parLapply(cl=cl, X=combos, fun=testfunction,
                                        dset=dset, modelType=modelType))
    }

    stopCluster(cl)
    a[sapply(a, is.null)] <- NULL
    print(paste("Patterns that passed", testfunction, ":",length(a)))
    print(paste("--- Runtime: ", tim[3], " Sekunden", sep=""))
    return(a)
  }
}
