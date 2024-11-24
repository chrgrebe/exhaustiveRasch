test_waldtest <- function(items=NULL,
                          dset=NULL,
                          na.rm=TRUE,
                          model=NULL,
                          p.par=NULL,
                          modelType=NULL,
                          splitcr="median",
                          icat=F,
                          alpha=0.1,
                          bonf=FALSE,
                          estimation_param=NULL){

  #' runs a Wald test using the Waldtest() function of eRm.
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
  #' @param splitcr as defined by eRm::Waldtest: Split criterion for subject
  #'  raw score splitting. median uses the median as split criterion, mean
  #'   performs a mean-split. Optionally splitcr can also be a dichotomous
  #'    vector which assigns each person to a certain subgroup
  #'     (e.g., following an external criterion). This vector can be numeric,
  #'      character or a factor.
  #' @param icat a boolean value indicating if the waldtest will be
  #' conducted on item level (TRUE, default value) or on item category level.
  #' This parameter only effects estimations using psychotools or pairwise and
  #' will be ignored for eRm estimations.
  #' @param alpha a numeric value for the alpha level. Will be ignored if
  #'  use.pval is FALSE
  #' @param bonf a boolean value wheter to use a Bonferroni correction. Will be
  #'  ignored if use.pval is FALSE
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


  ds_test <- dset[items]

  if (is.null(model)){
    if (na.rm==TRUE){ds_test<- stats::na.omit(ds_test)
    } else{ds_test <- ds_test[rowSums(is.na(ds_test)) < ncol(ds_test)-1, ]}
    model <- fit_rasch(X=ds_test, modelType=modelType,
                       estimation_param=estimation_param)
  }

  if (!is.null(model)){
    if (estimation_param$est=="pairwise"){
      try(suppressWarnings({wald <- pairwise::pairwise.S(
        daten=ds_test, split=splitcr)}),
        silent=TRUE)
      if (exists("wald")){
        if (icat){ # use item category parameters
          if (bonf==TRUE){local_alpha <- alpha/length(
            wald$S[[1]]$threshold$p)}
          else{local_alpha <- alpha}
          if (min(wald$S[[1]]$threshold$p) >= local_alpha){
            return(list(items, model, p.par))
          }
        } else{ # use item parameters
          if (bonf==TRUE){local_alpha <- alpha/length(
            wald$S[[1]]$sigma$p)}
          else{local_alpha <- alpha}
          if (min(wald$S[[1]]$sigma$p) >= local_alpha){
            return(list(items, model, p.par))
          }
        }
      }
    } else if (estimation_param$est=="eRm"){
      try(suppressWarnings({wald <- eRm::Waldtest(
        model,splitcr=splitcr)}),
        silent=TRUE)
      minNaN <- 1
      if (exists("wald")){
        if (bonf==TRUE){local_alpha <- alpha/length(wald$coef.table[,2])
        }else{local_alpha <- alpha}

        if (min(wald$coef.table[minNaN:length(
          wald$coef.table[,2]),2])!="NaN"){
          if (min(wald$coef.table[minNaN:length(wald$coef.table[,2]),2]) >=
              local_alpha & model$npar+1==
              length(wald$coef.table[,2])){
            return(list(items, model, p.par))
          }
        }
      }
    } else if (estimation_param$est=="psychotools"){
      try(suppressWarnings({wald <- waldtest.psy(model, modelType, splitcr,
                                                 icat=icat)}),
          silent=TRUE)
      if (exists("wald")){
        if (bonf==TRUE){local_alpha <- alpha/length(wald)}
        else{local_alpha <- alpha}

        if (min(wald) >= local_alpha){
          return(list(items, model, p.par))
        }
      }
    }
  }

}
