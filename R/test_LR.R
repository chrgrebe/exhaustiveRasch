test_LR <- function(items=NULL,
                    dset=NULL,
                    na.rm=TRUE,
                    model=NULL,
                    p.par=NULL,
                    modelType=NULL,
                    splitcr="median",
                    alpha=0.1,
                    estimation_param=NULL,
                    pair_param=NULL){
  #' runs Anderson's likelihood ration test using the LRtest() function of eRm.
  #' @param items a numeric vector containing the index numbers of the items
  #'  in dset that are used to fit the model
  #' @param dset a data.frame containing the data
  #' @param na.rm a boolean value. If TRUE, all cases with any NA are removed
  #'  (na.omit). If FALSE, only cases with full NA responses are removed
  #' @param model a list of type RM, PCM or RSM (a previously fit model)
  #'  matching the value of modelType. If model is provided, this model is used.
  #'   If NULL, a model is fit using dset and items.
  #' @param modelType a character value defining the rasch model to fit.
  #'  Possible values: RM, PCM, RSM
  #' @param splitcr as defined by eRm::LRtest. Split criterion for subject
  #'  raw score splitting. "all.r" corresponds to a full raw score split,
  #'   "median" uses the median as split criterion, "mean" performs a
  #'    mean split. Optionally splitcr can also be a vector which assigns each
  #'     person to a certain subgroup (e.g., following an external criterion).
  #'      This vector can be numeric, character or a factor.
  #' @param alpha a numeric value for the alpha level. Will be ignored if
  #'  use.pval is FALSE
  #' @param estimation_param options for parameter estimation using
  #' \link{estimation_control}
  #' @param pair_param options for options for fitting pairwise models using
  #' \link{pairwise_control}
  #' @return if the p-value of the test is not significant (above p=0.05) AND
  #'  if no items were excluded in the test due to missing patterns
  #'   (length of betalist == number of items), a list containing two elements
  #'    is returned: the pattern that was tested an a list of type RM, RCM or
  #'     RSM (depending on modelType) with the fit model. If the test is
  #'      significant, NULL is returned.
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
    #try(suppressWarnings({
    #  model <- get(modelType)(ds_test, se=TRUE)
    #}), silent=TRUE)
    model <- fit_rasch(X=ds_test, modelType=modelType,
                       estimation_param=estimation_param,
                       pair_param = pair_param)
  }

  if (estimation_param$est=="pairwise"){
    try(p.par <- pairwise::pers(model), silent=TRUE)
    if (exists("p.par")){
      try(suppressWarnings({lr <- pairwise::andersentest.pers(
        p.par, split=splitcr)$p}),
        silent=TRUE)
    }
    if (exists("lr")==TRUE){
      if (lr >=alpha){
        return(list(items, model, p.par))
      }
    }
  } else if (estimation_param$est=="psychotools"){
    try(suppressWarnings({lr <- LRtest.psy(
      model=model, modelType=modelType, splitcr=splitcr)}),
      silent=TRUE)
    if (exists("lr")==TRUE){
      if (lr >=alpha ){
        return(list(items, model, p.par))
      }
    }
  } else if (estimation_param$est=="eRm"){
    try(suppressWarnings({lr <- eRm::LRtest(
      model, splitcr=splitcr)}),
      silent=TRUE)
    if (exists("lr")==TRUE){
      if (lr$pvalue >=alpha & length(lr$X[1,])==length(
        lr$X.list[[1]][1,])){
        return(list(items, model, p.par))
      }
    }
  }
}
