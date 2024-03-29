threshold_order <- function(items=NULL,
                            dset=NULL,
                            na.rm=TRUE,
                            model=NULL,
                            modelType=NULL,
                            estimation_param=NULL){
  #' checks for disordered thresholds in rasch models
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
  #' @param estimation_param options for parameter estimation using
  #' \link{estimation_control}
  #' @return if there are no items with disordered thresholds in the model,
  #'  a list containing two elements is returned: the pattern that was tested an
  #'   a list of type RM, RCM or RSM (depending on modelType) with the fit
  #'    model. If there is at least one item with disordered thresholds,
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
    items <- items[[1]]
  }

  if (modelType %in% c("PCM", "RSM")){
    if (is.null(model)){
      ds_test <- dset[items]
      if (na.rm==TRUE){ds_test<- stats::na.omit(ds_test)
      } else{ds_test <- ds_test[rowSums(is.na(ds_test)) < ncol(ds_test)-1, ]}
      try(suppressWarnings({
        model <- get(modelType)(ds_test, se=TRUE)
      }), silent=TRUE)
    } else{
      items <- which(colnames(dset) %in% colnames(model$X))
    }

    no_items <- length(items)
    no_thres <- length(model$betapar)/no_items

    sorted <- TRUE
    for (i in 1:no_items){
      if (is.unsorted(eRm::thresholds(model)$threshtable$'1'[i,
                                                             2:(no_thres+1)]==TRUE)){sorted <- FALSE}
    }
    if (sorted==TRUE){return(list(items, model))}
  }
}
