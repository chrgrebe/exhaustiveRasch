test_DIFtree <- function(items=NULL, DIFvars=NULL, dset=NULL, na.rm=T, model=NULL, modelType=NULL, alpha=0.1, bonf=F){

  #' builds a raschtree using the raschtree or rstree function of the psychotree Package.
  #' @param items a numeric vector containing the index numbers of the items in dset that are used to fit the model
  #' @param DIFvars a vector or a data.frame containing the external variable(s) to test for differential item functioning
  #' @param dset a data.frame containing the data
  #' @param na.rm a boolean value. If TRUE, all cases with any NA are removed (na.omit). If FALSE, only cases with full NA responses are removed
  #' @param model a list of type RM, PCM or RSM (a previously fit model) matching the value of modelType. If model is provided, this model ist used. If NULL, a model is fit using dset and items.
  #' @param modelType a character value defining the rasch model to fit. Possible values: RM, PCM, RSM
  #' @param alpha a numeric value for the alpha level. Will be ignored if use.pval is FALSE
  #' @param bonf a boolean value wheter to use a Bonferroni correction. Will be ignored if use.pval is FALSE
  #' @return if none of the p-values is significant (above p=0.05), a list containing two elements is returned: the pattern that was tested an a list of type RM, RCM or RSM (depending on modelType) with the fit model. If there is at least one item with a significant p-value, NULL is returned.
  #' @export

  if (bonf==T){local_alpha <- alpha/length(items)} else{local_alpha <- alpha}

  if (is.null(model)){
    ds_test <- dset[, items]
    if (na.rm==T){ds_test<- stats::na.omit(ds_test)} else{ds_test <- ds_test <- ds_test[rowSums(is.na(ds_test)) < ncol(ds_test)-1, ]}
    try(suppressWarnings({
      model <- get(modelType)(ds_test, se=TRUE)
      items <- which(colnames(dset) %in% colnames(model$X))}))
    ds_DIF <-  ds_DIF <- merge(ds_test, DIFvars, by=0)
    ds_DIF$Row.names <- NULL
    ds_DIF$rasch <- as.matrix(ds_DIF[ , 1:length(items)])
    ds_DIF <- ds_DIF[ , -(1:length(items))]
    if (modelType=="RM"){
      try(suppressWarnings({DIF_tree <- psychotree::raschtree(rasch ~., data=ds_DIF)}))
    }
    if (modelType=="RSM"){
      try(suppressWarnings({DIF_tree <- psychotree::rstree(rasch ~., data=ds_DIF)}))
    }
    if (modelType=="PCM"){
      try(suppressWarnings({DIF_tree <- psychotree::pctree(rasch ~., data=ds_DIF)}))
    }
  }

  if (exists("DIF_tree")==T){
    if (length(DIF_tree)==1){
      return(list(items, model))
    }
  }
}
