test_itemfit<- function(items=NULL,
                        dset=NULL,
                        na.rm=TRUE,
                        control,
                        modelType=NULL,
                        model=NULL,
                        alpha=0.1,
                        bonf=FALSE,
                        estimation_param=NULL){
  #' checks the itemfit indices of a rasch model using the itemfit() function
  #'  of eRm.
  #' @param items a numeric vector containing the index numbers of the items
  #'  in dset that are used to fit the model
  #' @param dset a data.frame containing the data
  #' @param na.rm a boolean value. If TRUE, all cases with any NA are removed
  #'  (na.omit). If FALSE, only cases with full NA responses are removed
  #' @param control list object with options from \link{itemfit_control}
  #' @param model a list of type RM, PCM or RSM (a previously fit model)
  #'  matching the value of modelType. If model is provided, this model is used.
  #'   If NULL, a model is fit using dset and items.
  #' @param modelType a character value defining the rasch model to fit.
  #'  Possible values: RM, PCM, RSM
  #' @param alpha a numeric value for the alpha level. Will be ignored if
  #'  use.pval is FALSE
  #' @param bonf a boolean value whether to use a Bonferroni correction. Will
  #'  be ignored if use.pval is FALSE
  #' @param estimation_param options for parameter estimation using
  #' \link{estimation_control}
  #' @return if all fit indices meet the given criteria, a list containing
  #'  two elements is returned: the pattern that was tested an a list of type
  #'   RM, RCM or RSM (depending on modelType) with the fit model. If at least
  #'    one item's fit indices do not meet the given criteria, NULL is returned.
  #' @export
  #' @keywords internal

  if (inherits(items, "list")){
    model <- items[[2]]
    items <- items[[1]]
  }

  if (bonf==TRUE){local_alpha <- alpha/length(items)} else{local_alpha <- alpha}

  if (is.null(model)){
    ds_test <- dset[items]
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
      ifit <- eRm::itemfit(p.par)
      check <- TRUE
      if (control$use.pval==TRUE & min(stats::pchisq(ifit$i.fit, df=ifit$i.df,
                                                     lower.tail=FALSE))<local_alpha){
        check <- FALSE # check for p.value
      }
      if (control$msq==TRUE & (min(ifit$i.infitMSQ)<control$lowerMSQ | max(
        ifit$i.infitMSQ)>control$upperMSQ)){
        check <- FALSE # check for MSQ infit
      }
      if (control$zstd==TRUE & (min(ifit$i.infitZ)<control$lowerZ | max(
        ifit$i.infitZ)>control$upperZ)){
        check <- FALSE # check for standardised infit
      }
      if (control$outfits==TRUE){
        if (control$msq==TRUE & (min(ifit$i.outfitMSQ)<control$lowerMSQ | max(
          ifit$i.outfitMSQ)>control$upperMSQ)){
          check <- FALSE # check for MSQ outfit
        }
        if (control$zstd==TRUE & (min(ifit$i.outfitZ)<control$lowerZ | max(
          ifit$i.outfitZ)>control$upperZ)){
          check <- FALSE # check for standardised outfit
        }
      }
      if (check==TRUE){return(list(items, model))}
    }
  }

}
