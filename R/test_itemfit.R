test_itemfit<- function(items=NULL,
                        dset=NULL,
                        na.rm=TRUE,
                        control,
                        modelType=NULL,
                        model=NULL,
                        p.par=NULL,
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
  #'  3 elements is returned: the item combination that was tested, a list of
  #'  the class the model was estimated with (depending on modelType and
  #'  estimation_param$est) with the fit model and a list with a person
  #'  parameter object (depending on estimation_param$est). If at least one
  #'  item's fit indices do not meet the given criteria, NULL is returned.
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

  if (bonf==TRUE){local_alpha <- alpha/length(items)} else{local_alpha <- alpha}

  if (is.null(model)){
    ds_test <- dset[items]
    if (na.rm==TRUE){ds_test<- stats::na.omit(ds_test)
    } else{ds_test <- ds_test[rowSums(is.na(ds_test)) < ncol(ds_test)-1, ]}

    model <- fit_rasch(X=ds_test, modelType=modelType,
                       estimation_param=estimation_param)
  }

  check <- FALSE

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

  ### get fit indices
  if (!is.null(p.par)){
    if (estimation_param$est=="pairwise"){
      ifit <- pairwise::pairwise.item.fit(p.par)
    } else if (estimation_param$est=="eRm"){
      ifit <- eRm::itemfit(p.par)
    } else{ # psychotools
      ifit <- p.par$itemfit
    }
  }

  ### compare fit indices with restrictions
  if (exists("ifit")){
    if (estimation_param$est=="pairwise" | estimation_param$est=="psychotools"){
      if (!control$use.rel){
        in.msq <- ifit$INFIT.MSQ
        out.msq <- ifit$OUTFIT.MSQ
        in.z <- ifit$INFIT.ZSTD
        out.z <- ifit$OUTFIT.ZSTD
        p <- ifit$p
      } else{
        in.msq <- ifit$INFIT.MSQ.REL
        out.msq <- ifit$OUTFIT.MSQ.REL
        in.z <- ifit$INFIT.ZSTD.REL
        out.z <- ifit$i.outfitZ
      }
    } else{ # eRm
      in.msq <- ifit$i.infitMSQ
      out.msq <- ifit$i.outfitMSQ
      in.z <- ifit$i.infitZ
      out.z <- ifit$i.outfitZ
      p <- stats::pchisq(ifit$i.fit, df=ifit$i.df-1,
                         lower.tail=FALSE)
      # -1 is a correction for an error in eRm
    }

    check <- TRUE

    if (control$use.pval==TRUE & min(p)<local_alpha){
      check <- FALSE # check for p.value
    }
    if (control$msq==TRUE & (min(in.msq)<control$lowerMSQ | max(
      in.msq)>control$upperMSQ)){
      check <- FALSE # check for MSQ infit
    }
    if (control$zstd==TRUE & (min(in.z)<control$lowerZ | max(
      in.z)>control$upperZ)){
      check <- FALSE # check for standardised infit
    }
    if (control$outfits==TRUE){
      if (control$msq==TRUE & (min(out.msq)<control$lowerMSQ | max(
        out.msq)>control$upperMSQ)){
        check <- FALSE # check for MSQ outfit
      }
      if (control$zstd==TRUE & (min(out.z)<control$lowerZ | max(
        out.z)>control$upperZ)){
        check <- FALSE # check for standardised outfit
      }
    }
  }

  if (check==TRUE){return(list(items, model, p.par))}

}
