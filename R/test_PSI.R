test_PSI <- function(items=NULL,
                     dset=NULL,
                     na.rm=TRUE,
                     model=NULL,
                     p.par=NULL,
                     modelType=NULL,
                     PSI=NULL,
                     estimation_param=NULL){
  # This is an internal function that is not intended to be called by users.
  # It is nevertheless exported so that it can be run in the parallelization
  # workers. However, the function is not documented in the manual.

  #' runs a principal component analysis (PCA) on the residuals of the
  #'  rasch model.
  #' @param items a numeric vector containing the index numbers of the items
  #'  in dset that are used to fit the model
  #' @param dset a data.frame containing the data
  #' @param na.rm a boolean value. If TRUE, all cases with any NA are removed
  #'  (na.omit). If FALSE, only cases with full NA responses are removed
  #' @param model on object of a fit Rasch model, estimated with the packages
  #' 'eRm' (classes 'RM', 'PCM' or 'RSM'), 'psychotools' (classes raschmodel,
  #' 'pcmodel' or 'rsmodel') or 'pairwise' (class 'pers'), matching the value of
  #'  modelType. If 'model' is provided, this model is used. If NULL, a model is
  #'  fit using 'dset' and 'items'.
  #' @param p.par a person parameter object matching the class of 'model'. If
  #'  NULL, the person parameters will be estimated.
  #' @param modelType a character value defining the rasch model to fit.
  #'  Possible values: "RM", "PCM", "RSM".
  #' @param PSI a numeric value defining the minimum value for the person-
  #' separation-index (separation reliablility).
  #' @param estimation_param options for parameter estimation using
  #' \link{estimation_control}
  #' @return if the value of the PSI for the model is >= the PSI argument,
  #'  a list containing 3 elements is returned: the item combination that was
  #'  tested, a list of the class the model was estimated with (depending on
  #'  modelType and estimation_param$est) with the fit model and a list with a
  #'  person parameter object (depending on estimation_param$est). Else, NULL
  #'  is returned.
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

  ds_test <- dset[items]

  if (is.null(model)){
    if (na.rm==TRUE){ds_test<- stats::na.omit(ds_test)
    } else{ds_test <- ds_test[rowSums(is.na(ds_test)) < ncol(ds_test)-1, ]}
    model <- fit_rasch(X=ds_test, modelType=modelType,
                       estimation_param=estimation_param)
  }

  ### get person parameter object if not already existing
  if (!is.null(model) & is.null(p.par)){
    if (estimation_param$est=="pairwise"){
      p.par <- pairwise::pers(model)
    } else if (estimation_param$est=="eRm"){
      try(suppressWarnings({
        p.par <- eRm::person.parameter(model)
      }), silent=TRUE)
    } else{ # psychotools
      p.par <- ppar.psy(model)
    }
  }

  ### get person person separation reliability

  if (!is.null(p.par)){
    if (estimation_param$est=="pairwise"){
      if (exists("p.par")){
        res <- pairwise::pairwise.SepRel(p.par)$sep.rel
      }
    } else if (estimation_param$est=="eRm"){
      res <- eRm::SepRel(p.par)$sep.rel
    } else if (estimation_param$est=="psychotools"){
      res <- p.par$PSI
    }
  }
  if(!is.null(p.par)){
    if (!res<PSI){
      return(list(items, model, p.par))
    }
  }
}
