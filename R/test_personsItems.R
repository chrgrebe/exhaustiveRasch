test_personsItems <- function(items=NULL, dset=NULL, na.rm=T, model=NULL, modelType=NULL, gap_prop=0, extremes=T){
  #' checks the relationship between the person parameter distribution and the item (or: threshold) locations for defined criteria
  #' @param items a numeric vector containing the index numbers of the items in dset that are used to fit the model
  #' @param dset a data.frame containing the data
  #' @param na.rm a boolean value. If TRUE, all cases with any NA are removed (na.omit). If FALSE, only cases with full NA responses are removed
  #' @param model a list of type RM, PCM or RSM (a previously fit model) matching the value of modelType. If model is provided, this model ist used. If NULL, a model is fit using dset and items.
  #' @param modelType a character value defining the rasch model to fit. Possible values: RM, PCM, RSM
  #' @param gap_prop a numeric value between 0 and 1 that sets the criterion for the minimum proportion of neighboring person parameters with an item/threshold location in between. If set to 0, this criterion will not be checked.
  #' @param extremes a boolean value indicating if a check for the item/threshold locations left of the 2nd lowest and right of the 2nd highest person parameter
  #' @return if the criteria are met, a list containing two elements is returned: the pattern that was tested an a list of type RM, RCM or RSM (depending on modelType) with the fit model. If the criteria are not met, NULL is returned.
  #' @export

  if (inherits(items, "list")){
    model <- items[[2]]
    items <- items[[1]]
  }

  if (is.null(model)){
    ds_test <- dset[items]
    if (na.rm==T){ds_test<- stats::na.omit(ds_test)} else{ds_test <- ds_test <- ds_test[rowSums(is.na(ds_test)) < ncol(ds_test)-1, ]}
    try(suppressWarnings({
      model <- get(modelType)(ds_test, se=TRUE)
    }), silent=T)
  } else{
    items <- which(colnames(dset) %in% colnames(model$X))
  }
  PImapExtremes <- T
  try(p.par <- eRm::person.parameter(model), silent=T)
  if (exists("p.par")){
    perspars <- sort(unique(unlist(p.par$thetapar)))
    if (modelType=="RM"){
      threshs <- sort(as.vector(0-model$betapar))
    } else{
      threshs <- sort(as.vector(eRm::thresholds(model)$threshtable$'1'[,2:length(eRm::thresholds(model)$threshtable$'1'[1,])]))
    }
    if (extremes==T){  # checks if there are items (thresholds) left of the 2nd lowest and right of the 2nd highest person parameter
      no_items <- length(items)
      no_thres <- length(model$betapar)/no_items
      PImapExtremes <-F
      low_pers <- perspars[2]
      high_pers <- perspars[length(perspars)-1]
      if (length(which(threshs<low_pers))>0 & length(which(threshs>high_pers)) >0){
        PImapExtremes <-T
      }
    }
    gap_crit=T
    if (gap_prop>0){#calculates the proportions of neighboring person parameters with an items/threshold location in between
      total_gaps <- length(perspars) -1
      count_gaps <- 0
      for (i in 1: length(perspars)-1){
        if (length(intersect(which(threshs>perspars[i]),which(threshs<perspars[i+1])))>0){count_gaps <- count_gaps+1}
      }
      prop_gaps <- count_gaps/total_gaps
      if (prop_gaps < gap_prop){gap_crit <- F}
    }
    if (PImapExtremes==T & gap_crit==T){
      return(list(items, model))
    }
  }
}
