threshold_order <- function(werte=NULL, dset=NULL, model=NULL, modelType=NULL){
  #' checks for disordered threshoilds in rasch models
  #' @param werte a numeric vector containing the index numbers of the items in dset that are used to fit the model
  #' @param dset a data.frame containing the data
  #' @param model a list of type RM, PCM or RSM (a previously fit model) matching the value of modelType. If model is provided, this model ist used. If NULL, a model is fit using dset and werte.
  #' @param modelType a character value defining the rasch model to fit. Possible values: RM, PCM, RSM
  #' @return if there are no items with disordered thresholds in the model, a list containing two elements is returned: the pattern that was tested an a list of type RM, RCM or RSM (depending on modelType) with the fit model. If there is at least one item with disordered thresholds, NULL is returned.
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
