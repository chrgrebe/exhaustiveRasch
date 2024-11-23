test_respca <- function(items=NULL,
                        dset=NULL,
                        na.rm=TRUE,
                        model=NULL,
                        p.par=NULL,
                        modelType=NULL,
                        max_contrast=1.5,
                        estimation_param=NULL,
                        pair_param=NULL){
  #' runs a principal component analysis (PCA) on the residuals of the
  #'  rasch model.
  #' @param items a numeric vector containing the index numbers of the items
  #'  in dset that are used to fit the model
  #' @param modelType a character value defining the rasch model to fit.
  #'  Possible values: RM, PCM, RSM
  #' @param dset a data.frame containing the data
  #' @param na.rm a boolean value. If TRUE, all cases with any NA are removed
  #'  (na.omit). If FALSE, only cases with full NA responses are removed
  #' @param max_contrast a numeric value defining the maximum loading of a
  #'  factor in the principal components analysis of the standardised residuals.
  #' @param estimation_param options for parameter estimation using
  #' \link{estimation_control}
  #' @param pair_param options for options for fitting pairwise models using
  #' \link{pairwise_control}
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
    p.par <- items[[3]]
    items <- items[[1]]
  }

  ds_test <- dset[items]

  if (is.null(model)){
    if (na.rm==TRUE){ds_test<- stats::na.omit(ds_test)
    } else{ds_test <- ds_test[rowSums(is.na(ds_test)) < ncol(ds_test)-1, ]}
    #}), silent=TRUE)
    model <- fit_rasch(X=ds_test, modelType=modelType,
                       estimation_param=estimation_param,
                       pair_param = pair_param)
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

  ### get std. residuals

  if (!is.null(p.par)){
    if (estimation_param$est=="pairwise"){
      res <- residuals.pers(p.par, res="stdr") # change to pairwise:: as soon as
                                               # the package exports that
                                               # function
    } else if (estimation_param$est=="eRm"){
      res <- eRm::itemfit(p.par)$st.res
    } else{ # psychotools
      res <- ppar.psy(model)$residuals$res_std
    }
  }

  if(!is.null(p.par)){
    pca <- psych::pca(res, nfactors = length(items), rotate = "none")
    if (max(pca$values<max_contrast)){
      return(list(items, model, p.par))
    }
  }
}
