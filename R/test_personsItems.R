test_personsItems <- function(items=NULL,
                              dset=NULL,
                              na.rm=TRUE,
                              model=NULL,
                              p.par=NULL,
                              modelType=NULL,
                              gap_prop=0,
                              extremes=TRUE,
                              estimation_param=NULL){
  #' checks the relationship between the person parameter distribution and
  #'  the item (or: threshold) locations for defined criteria
  #' @param items a numeric vector containing the index numbers of the items
  #'  in dset that are used to fit the model
  #' @param dset a data.frame containing the data
  #' @param na.rm a boolean value. If TRUE, all cases with any NA are removed
  #'  (na.omit). If FALSE, only cases with full NA responses are removed
  #' @param model a list of type RM, PCM or RSM (a previously fit model)
  #'  matching the value of modelType. If model is provided, this model is
  #'   used. If NULL, a model is fit using dset and items.
  #' @param modelType a character value defining the rasch model to fit.
  #'  Possible values: RM, PCM, RSM
  #' @param gap_prop a numeric value between 0 and 1 that sets the criterion
  #'  for the minimum proportion of neighboring person parameters with an
  #'   item/threshold location in between. If set to 0, this criterion will
  #'    not be checked.
  #' @param extremes a boolean value indicating if a check for the
  #'  item/threshold locations left of the 2nd lowest and right of the 2nd
  #'   highest person parameter.
  #' @param estimation_param options for parameter estimation using
  #' \link{estimation_control}
  #' @return if the criteria are met, a list containing two elements is
  #'  returned: the pattern that was tested an a list of type RM, RCM or RSM
  #'   (depending on modelType) with the fit model. If the criteria are not met,
  #'    NULL is returned.
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

  ### get ordered unique person parameters and item thresholds
  if (!is.null(p.par)){
    if (estimation_param$est=="pairwise"){
      if (exists("p.par")){
        perspars <- sort(unique(p.par$pers$WLE))
        threshs <- as.vector(sort(model$threshold))
      }
    } else if (estimation_param$est=="eRm"){
       perspars <- sort(unique(unlist(p.par$thetapar)))
       if (modelType=="RM"){
         threshs <- as.vector(sort(model$betapar))
         } else{
           threshs <- as.vector(sort(eRm::thresholds(model)$threshpar))
         }
    } else if (estimation_param$est=="psychotools"){
       perspars <- sort(unique(unlist(p.par$theta)))
       threshs <- as.vector(sort(unlist(model$thresholds)))
    }
  }

  PImapExtremes <- TRUE
  gap_crit <- TRUE

  if (exists("perspars")){
    if (extremes==TRUE){
      # checks if there are items (thresholds) left of the 2nd lowest and
      # right of the 2nd highest person parameter
      PImapExtremes <-FALSE
      low_pers <- perspars[2]
      high_pers <- perspars[length(perspars)-1]
      if (length(which(threshs<low_pers))>0 & length(
        which(threshs>high_pers)) >0){
        PImapExtremes <-TRUE
      }
    }

    if (gap_prop>0){
      # calculates the proportions of neighboring person parameters with an
      # items/threshold location in between
      total_gaps <- length(perspars) -1
      count_gaps <- 0
      for (i in seq_len(length(perspars))-1){
        if (length(intersect(which(threshs>perspars[i]),which(
          threshs<perspars[i+1])))>0){count_gaps <- count_gaps+1}
      }
      prop_gaps <- count_gaps/total_gaps
      if (prop_gaps < gap_prop){gap_crit <- FALSE}
    }
    if (PImapExtremes==TRUE & gap_crit==TRUE){
      return(list(items, model, p.par))
    }
  }
}
