test_LR <- function(werte=NULL, dset=NULL, model=NULL, modelType=NULL){
  #' runs Anderson's likelihood ration test with median split using the LRtest() function of eRm.
  #' @param werte a numeric vector containing the index numbers of the items in dset that are used to fit the model
  #' @param dset a data.frame containing the data
  #' @param model a list of type RM, PCM or RSM (a previously fit model) matching the value of modelType. If model is provided, this model ist used. If NULL, a model is fit using dset and werte.
  #' @param modelType: a character value defining the rasch model to fit. Possible values: RM, PCM, RSM
  #' @return if the p-value of the test ist not significant (above p=0.05), a list containing two elements is returned: the pattern that was tested an a list of type RM, RCM or RSM (depending on modelType) with the fit model. If the test is significant (p<0.05), NULL is returned.
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
