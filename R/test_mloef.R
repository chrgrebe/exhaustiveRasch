test_mloef <- function(werte=NULL, dset=NULL, model=NULL, modelType=NULL, splitcr="median"){
  #' runs Martin-Loef Test using the MLoef() function of eRm.
  #' @param werte a numeric vector containing the index numbers of the items in dset that are used to fit the model
  #' @param dset a data.frame containing the data
  #' @param model a list of type RM, PCM or RSM (a previously fit model) matching the value of modelType. If model is provided, this model ist used. If NULL, a model is fit using dset and werte.
  #' @param modelType a character value defining the rasch model to fit. Possible values: RM, PCM, RSM
  #' @param splitcr as defined by eRm::mloef: Split criterion to define the item groups. "median" and "mean" split items in two groups based on their items' raw scores. splitcr can also be a vector of length k (where k denotes the number of items) that takes two or more distinct values to define groups used for the Martin-L?f Test.
  #' @return if the p-value of the test is not significant (above p=0.05), a list containing two elements is returned: the pattern that was tested an a list of type RM, RCM or RSM (depending on modelType) with the fit model. If the test is significant (p<0.05), NULL is returned.
  #' @export
  ds_test <- dset[,werte]
  try(suppressWarnings({model <- get(modelType)(ds_test, se=TRUE)}))

  if (exists("model")==T){
    try(ml <- eRm::MLoef(model, splitcr=splitcr))
    if (exists("ml")==T){
      if (ml$p.value >0.05){
        return(list(werte, model))
      }
    }
  }
}
