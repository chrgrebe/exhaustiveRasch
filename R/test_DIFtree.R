test_DIFtree <- function(items=NULL,
                         DIFvars=NULL,
                         dset=NULL,
                         na.rm=TRUE,
                         model=NULL,
                         p.par=NULL,
                         modelType=NULL,
                         alpha=0.1,
                         estimation_param=NULL){

  #' builds a raschtree using the raschtree or rstree function of the
  #'  psychotree Package.
  #' @param items a numeric vector containing the index numbers of the items in
  #'  dset that are used to fit the model
  #' @param DIFvars a vector or a data.frame containing the external variable(s)
  #'  to test for differential item functioning
  #' @param dset a data.frame containing the data
  #' @param na.rm a boolean value. If TRUE, all cases with any NA are removed
  #'  (na.omit). If FALSE, only cases with full NA responses are removed
  #' @param model a list of type RM, PCM or RSM (a previously fit model)
  #'  matching the value of modelType. If model is provided, this model is used.
  #'   If NULL, a model is fit using dset and items.
  #' @param modelType a character value defining the rasch model to fit.
  #'  Possible values: RM, PCM, RSM
  #' @param alpha a numeric value for the alpha level. Will be ignored if
  #'  use.pval is FALSE
  #' @param estimation_param options for parameter estimation using
  #' \link{estimation_control}
  #' @return if none of the p-values is significant (above p=0.05), a list
  #'  containing two elements is returned: the pattern that was tested an a list
  #'   of type RM, RCM or RSM (depending on modelType) with the fit model.
  #'    If there is at least one item with a significant p-value,
  #'     NULL is returned.
  #' @export
  #' @keywords internal

  # This function implements one of the tests that are executed via the 'tests'
  # argument of the exhaustive_tests() function. It is an internal function, a
  # call by the user is not indicated. It is nevertheless exported in order to
  # work in parallelization. However, it is not included in the package
  # documentation (roxygen2 keyword 'internal').

  if (inherits(items, "list")){
    model <- items[[2]]
    p.par <- items[[3]]
    items <- items[[1]]
  }

  ds_test <- dset[, items]

  if (is.null(model)){
    if (na.rm==TRUE){ds_test<- stats::na.omit(ds_test)
    } else{ds_test <- ds_test[rowSums(is.na(ds_test)) < ncol(ds_test)-1, ]}
    #try(suppressWarnings({
    #  model <- get(modelType)(ds_test, se=TRUE)
    #}), silent=TRUE)
    model <- fit_rasch(X=ds_test, modelType=modelType,
                       estimation_param=estimation_param)
  }

  if (!is.null(model)){
    ds_DIF <- merge(ds_test, DIFvars, by=0)
    ds_DIF$Row.names <- NULL
    ds_DIF$rasch <- as.matrix(ds_DIF[ , seq_len(length(items))])
    ds_DIF <- ds_DIF[ , -(seq_len(length(items)))]
    if (modelType=="RM"){
      try(suppressWarnings({
        DIF_tree <- psychotree::raschtree(rasch ~., data=ds_DIF)}), silent=TRUE)
      }
    if (modelType=="RSM"){
      try(suppressWarnings({
        DIF_tree <- psychotree::rstree(rasch ~., data=ds_DIF)}), silent=TRUE)
      }
    if (modelType=="PCM"){
      try(suppressWarnings({
        DIF_tree <- psychotree::pctree(rasch ~., data=ds_DIF)}), silent=TRUE)
      }
  }
  if (exists("DIF_tree")==TRUE & !is.null(model)){
    if (length(DIF_tree)==1){
      return(list(items, model, p.par))
    }
  }
}
