test_PSI <- function(items=NULL,
                     dset=NULL,
                     na.rm=TRUE,
                     model=NULL,
                     modelType=NULL,
                     PSI=NULL,
                     estimation_param=NULL){
  #' runs a principal component analysis (PCA) on the residuals of the
  #'  rasch model.
  #' @param items a numeric vector containing the index numbers of the items
  #'  in dset that are used to fit the model
  #' @param modelType a character value defining the rasch model to fit.
  #'  Possible values: RM, PCM, RSM
  #' @param dset a data.frame containing the data
  #' @param na.rm a boolean value. If TRUE, all cases with any NA are removed
  #'  (na.omit). If FALSE, only cases with full NA responses are removed
  #' @param PSI a numeric value defining the minimum value for the person-
  #' separation-index (separation reliablility).
  #' @param estimation_param options for parameter estimation using
  #' \link{estimation_control}
  #' @return if the maximum eigenvalue of the contrasts of the pca
  #'  is < max_contrast a   #'  list containing two elements is returned:
  #'  the item combination that was tested and a list of type RM, RCM or RSM
  #'  (depending on modelType) with the fit model. Else, NULL is returned.
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

  ds_test <- dset[items]

  if (is.null(model)){
    if (na.rm==TRUE){ds_test<- stats::na.omit(ds_test)
    } else{ds_test <- ds_test[rowSums(is.na(ds_test)) < ncol(ds_test)-1, ]}
    #try(suppressWarnings({
    #  model <- get(modelType)(ds_test, se=TRUE)
    #}), silent=TRUE)
    model <- fit_rasch(X=ds_test, modelType=modelType,
                       estimation_param=estimation_param)
  } else{
    items <- which(colnames(dset) %in% colnames(model$X))
  }

  if (exists("model")==TRUE){
    try(p.par <- eRm::person.parameter(model), silent=TRUE)
    if (exists("p.par")){
      res <- eRm::SepRel(p.par)$sep.rel
      if (!res<PSI){
        return(list(items, model))
      }
    }
  }
}
