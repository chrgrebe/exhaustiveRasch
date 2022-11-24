all_rawscores <- function(items=NULL,
                          dset=NULL,
                          na.rm=TRUE,
                          model=NULL,
                          modelType=NULL){
  #' checks if all possible raw scores occur in the data.
  #' @param items a numeric vector containing the index numbers of the items in
  #'  dset that are used to fit the model
  #' @param dset a data.frame containing the data
  #' @param na.rm a boolean value. If TRUE, all cases with any NA are removed
  #'  (na.omit). If FALSE, only cases with full NA responses are removed
  #' @param model a list of type RM, PCM or RSM (a previously fit model)
  #'  matching the value of modelType. If model is provided, this model is used.
  #'   If NULL, a model is fit using dset and items.
  #' @param modelType a character value defining the rasch model to fit.
  #'  Possible values: RM, PCM, RSM
  #' @return if all possible raw scores occur in dset, a list containing two
  #'  elements is returned: the pattern that was tested an a list of type
  #'   RM, RCM or RSM (depending on modelType) with the fit model. If at least
  #'    one raw score does not occur in dset, NULL is returned.
  #' @export
  #' @keywords internal

  if (inherits(items, "list")){
    model <- items[[2]]
    items <- items[[1]]
  }

  ds_test <- dset[,items]
  if (na.rm==TRUE){ds_test<- stats::na.omit(ds_test)}

  min_cat <- min(apply(ds_test,2, function(x) min(x, na.rm=T)))
  max_cat <- max(apply(ds_test,2, function(x) max(x, na.rm=T)))
  no_cats <- (max_cat-min_cat)*length(ds_test)
  poss_rawscores <- seq(min_cat*length(ds_test), max_cat*length(ds_test))
  emp_rawscores <- as.numeric(names((table(rowSums(ds_test)))))
  if (length(which(!poss_rawscores %in% emp_rawscores))==0){
    return(items)
  }

}
