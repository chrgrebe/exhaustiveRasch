threshold_order <- function(werte=NULL, dset=NULL, model=NULL, modelType=NULL){
  ds_test <- dset[,werte]
  model <- fit_rasch(werte=werte, dset=dset, modelType=modelType)
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
