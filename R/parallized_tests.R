all_rawscores <- function(werte=NULL, dset=NULL, model=NULL, modelType=NULL){
  #' @export
  ds_test <- dset[,werte]
  scores <- rowSums(ds_test)
  if (length(unique(scores))==(length(ds_test)+1)){
    return(werte)
  }
}

test_itemfit <- function(werte=NULL, dset=NULL, lower=0.7, upper=1.3, model=NULL, p.val=TRUE, modelType=NULL){
  #' @export
    if (is.null(model)){
    ds_test <- dset[werte]
    try(suppressWarnings({model <- get(modelType)(ds_test, se=TRUE)}))
  }

  if (exists("model")==T){
    try(p.par <- eRm::person.parameter(model))
    if (exists("p.par")){
      ifit <- eRm::itemfit(p.par)
      if (p.val==TRUE){
        if ((min(ifit$i.infitMSQ)>=lower) & (max(ifit$i.infitMSQ)<=upper) & (min(ifit$i.outfitMSQ)>=lower) & (max(ifit$i.outfitMSQ)<=upper) & (min(stats::pchisq(ifit$i.fit, df=ifit$i.df, lower.tail=FALSE))>0.05)){
          return(list(werte, model))
        }
      } else{
        if ((min(ifit$i.infitMSQ)>=lower) & (max(ifit$i.infitMSQ)<=upper) & (min(ifit$i.outfitMSQ)>=lower) & (max(ifit$i.outfitMSQ)<=upper)){
          return(list(werte, model))
        }
      }
    }
  }

}

test_LR <- function(werte=NULL, dset=NULL, model=NULL, modelType=NULL){
  #' @export
  if (is.null(model)){
    ds_test <- dset[, werte]
    try(suppressWarnings({model <- get(modelType)(ds_test, se=TRUE)}))
  }

  try(suppressWarnings({lr <- eRm::LRtest(model, splitcr="median")}))
  if (exists("lr")==T){
    if (lr$pvalue >0.05 & length(lr$betalist$low)==length(ds_test)){
      return(list(werte, model))
    }
  }
}

test_mloef <- function(werte=NULL, dset=NULL, model=NULL, modelType=NULL){
  #' @export
  ds_test <- dset[,werte]
  try(suppressWarnings({model <- get(modelType)(ds_test, se=TRUE)}))

  if (exists("model")==T){
    try(ml <- eRm::MLoef(model, splitcr="median"))
    if (exists("ml")==T){
      if (ml$p.value >0.05){
        return(list(werte, model))
      }
    }
  }
}

test_pca <- function(werte=NULL, dset=NULL, model=NULL, modelType=NULL){
  #' @export
  ds_test <- dset[werte]
  km <- psych::tetrachoric(ds_test,y=NULL,correct=.5,smooth=TRUE,global=TRUE,weight=NULL,na.rm=TRUE, delete=TRUE)$rho
  eig <- as.list(eigen(km)$values)
  nr_fac <- length(eig[which(eig>1)])

  if (nr_fac==1){
    return(werte)
  }
}

test_waldtest <- function(werte=NULL, dset=NULL, model=NULL, modelType=NULL){
  #' @export
  if (is.null(model)){
    ds_test <- dset[werte]
    try(suppressWarnings({model <- get(modelType)(ds_test, se=TRUE)}))
  }

  try(suppressWarnings({wald <- eRm::Waldtest(model)}))
  if (exists("wald")==T){
    if (min(wald$coef.table[,2]) >0.05){
      return(list(werte, model))
    }
  }
}

threshold_order <- function(werte=NULL, dset=NULL, model=NULL, modelType=NULL){
  #' @export
  if (is.null(model)){
    ds_test <- dset[werte]
    try(suppressWarnings({model <- get(modelType)(ds_test, se=TRUE)}))
  }

  bet <- model$betapar
  no_items <- length(ds_test)
  no_thres <- length(model$betapar)/no_items
  sorted <- T
  for (i in 1:no_items){
    first_thres <- 1+(i-1)*no_thres
    if (is.unsorted(bet[(first_thres+no_thres-1):first_thres])==T){sorted <- F}
  }

  if (sorted==T){return(list(werte, model))}
}

parallized_tests <- function(dset, modelType="RM", combos, testfunction,...){
  arguments <- list(...)
  # abfangen, wenn keine Pattern oder eine Warnmeldung als character uebergeben wurden

  if (length(combos)==0 | is.character(combos)){
    warning(paste("No patterns left to perform ", testfunction, ". Aborted.", sep=""))
  } else{
    cl <- parallel::makePSOCKcluster(detectCores())
    parallel::setDefaultCluster(cl)
    parallel::clusterExport(cl, testfunction)
    parallel::clusterEvalQ(cl, library(eRm))
    parallel::clusterEvalQ(cl, library(psych))
    parallel::clusterEvalQ(cl, library(exhaustiveRasch))

    if (testfunction=="test_itemfit"){
      param1 <- list(cl=cl, dset=dset, X=combos, fun= testfunction)
      if (!is.null(arguments$lower)){param1$lower= arguments$lower}
      if (!is.null(arguments$upper)){param1$upper= arguments$upper}
      #if (exists("modelle")){modelle=NULL}
      if (!is.null(arguments$p.val)){param1$p.val= arguments$p.val}
      param1$modelType=modelType
      param1$dset=dset
      tim <- system.time(a <- do.call(parallel::parLapply, param1))
    } else{
      tim <- system.time(a <- parallel::parLapply(cl=cl, X=combos, fun=testfunction,
                                        dset=dset, modelType=modelType))
    }

    parallel::stopCluster(cl)
    a[sapply(a, is.null)] <- NULL
    print(paste("Patterns that passed", testfunction, ":",length(a)))
    print(paste("--- Runtime: ", tim[3], " Sekunden", sep=""))
    return(a)
  }
}
