threshold_order <- function(items=NULL,
                            dset=NULL,
                            na.rm=TRUE,
                            model=NULL,
                            p.par=NULL,
                            modelType=NULL,
                            estimation_param=NULL,
                            pair_param=NULL){
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
  #' @param pair_param options for options for fitting pairwise models using
  #' \link{pairwise_control}
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
    p.par <- items[[3]]
    items <- items[[1]]
  }

  if (modelType %in% c("PCM", "RSM")){
    if (is.null(model)){
      ds_test <- dset[items]
      if (na.rm==TRUE){ds_test<- stats::na.omit(ds_test)
      } else{ds_test <- ds_test[rowSums(is.na(ds_test)) < ncol(ds_test)-1, ]}

      model <- fit_rasch(X=ds_test, modelType=modelType,
                         estimation_param=estimation_param,
                         pair_param = pair_param)
    }

    sorted <- TRUE

    if (estimation_param$est=="pairwise"){
      if(pair_param$use.thurst==T){
        thrstable <- model$threshold
      } else{
        thrstable <- as.matrix(pairwise::deltapar(model))[,-1]
      }
    } else if (estimation_param$est=="eRm"){
      thrstable <- eRm::thresholds(model)$threshtable[[1]][,-1]
      if(any(apply(thrstable, 1, is.unsorted, na.rm=T))){sorted <- FALSE}
    } else{ #psychotools
      thrstable <- model$thresholds
      if(any(lapply(thrstable, is.unsorted, na.rm=T))){sorted <- FALSE}
    }
    if (sorted==TRUE){return(list(items, model, p.par))}
  }
}

