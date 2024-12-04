test_mloef <- function(items=NULL,
                       dset=NULL,
                       na.rm=TRUE,
                       model=NULL,
                       p.par=NULL,
                       modelType=NULL,
                       splitcr="median",
                       alpha=0.1,
                       estimation_param=NULL){
  #' runs Martin-Loef Test using the MLoef() function of eRm.
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
  #' @param splitcr as defined by eRm::MLoef: Split criterion to define the
  #'  item groups. "median" and "mean" split items in two groups based on their
  #'   items' raw scores. splitcr can also be a vector of length k (where k
  #'   denotes the number of items) that takes two or more distinct values to
  #'    define groups used for the Martin-Löf Test.
  #' @param alpha a numeric value for the alpha level. Will be ignored if
  #'  use.pval is FALSE
  #' @param estimation_param options for parameter estimation using
  #' \link{estimation_control}
  #' @return if the p-value of the test is not significant, a list containing
  #'  3 elements is returned: the item combination that was tested, a list of
  #'  the class the model was estimated with (depending on modelType and
  #'  estimation_param$est) with the fit model and a list with a person
  #'  parameter object (depending on estimation_param$est). If the test is
  #'  significant, NULL is returned.
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

  if (is.null(model)){
    ds_test <- dset[items]
    if (na.rm==TRUE){ds_test<- stats::na.omit(ds_test)
    } else{ds_test <- ds_test[rowSums(is.na(ds_test)) < ncol(ds_test)-1, ]}
    #try(suppressWarnings({
    #  model <- get(modelType)(ds_test, se=TRUE)
    #}), silent=TRUE)
    model <- fit_rasch(X=ds_test, modelType=modelType,
                       estimation_param=estimation_param)
  }

  if (!is.null(model)){
    if (estimation_param$est=="psychotools"){
      try(suppressWarnings({
        ml <- mloef.psy(model, modelType, splitcr)
      }), silent=TRUE)
    } else if (estimation_param$est=="eRm"){
      try(suppressWarnings({ml <- eRm::MLoef(model, splitcr=splitcr)
      }), silent=TRUE)
    }
  }

  if (exists("ml")==TRUE){
    if (!is.null(ml)){
      if (ml$p.value >=alpha){
        return(list(items, model, p.par))
      }
    }
  }

}
