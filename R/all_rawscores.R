all_rawscores <- function(werte=NULL, dset=NULL, na.rm=T, model=NULL, modelType=NULL){
  #' checks if all possible raw scores occur in the data.
  #' @param werte a numeric vector containing the index numbers of the items in dset that are used to fit the model
  #' @param dset a data.frame containing the data
  #' @param na.rm a boolean value. If TRUE, all cases with any NA are removed (na.omit). If FALSE, only cases with full NA responses are removed
  #' @param model a list of type RM, PCM or RSM (a previously fit model) matching the value of modelType. If model is provided, this model ist used. If NULL, a model is fit using dset and werte.
  #' @param modelType a character value defining the rasch model to fit. Possible values: RM, PCM, RSM
  #' @return if all possible raw scores occur in dset, a list containing two elements is returned: the pattern that was tested an a list of type RM, RCM or RSM (depending on modelType) with the fit model. If at least one raw score does not occur in dset, NULL is returned.
  #' @export
  ds_test <- dset[,werte]
  if (na.rm==T){ds_test<- stats::na.omit(ds_test)} else{ds_test <- ds_test <- ds_test[rowSums(is.na(ds_test)) < ncol(ds_test)-1, ]}
  scores <- rowSums(ds_test)
  if (length(unique(scores))==(length(ds_test)+1)){
    return(werte)
  }
}
