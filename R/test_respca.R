test_respca <- function(items=NULL,
                        dset=NULL,
                        na.rm=TRUE,
                        model=NULL,
                        modelType=NULL,
                        max_contrast=1.5){
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
  #'  (na.omit). If FALSE, only cases with full NA responses are removed
  #' @return if the maximum eigenvalue of the contrasts of the pca
  #'  is < max_contrast a   #'  list containing two elements is returned:
  #'  the item combination that was tested and a list of type RM, RCM or RSM
  #'  (depending on modelType) with the fit model. Else, NULL is returned.
  #' @export
  #' @keywords internal

  if (inherits(items, "list")){
    model <- items[[2]]
    items <- items[[1]]
  }

  ds_test <- dset[items]

  if (is.null(model)){
    if (na.rm==TRUE){ds_test<- stats::na.omit(ds_test)
    } else{ds_test <- ds_test[rowSums(is.na(ds_test)) < ncol(ds_test)-1, ]}
    try(suppressWarnings({
      model <- get(modelType)(ds_test, se=TRUE)
    }), silent=TRUE)
  } else{
    items <- which(colnames(dset) %in% colnames(model$X))
  }

  if (exists("model")==TRUE){
    try(p.par <- eRm::person.parameter(model), silent=TRUE)
    if (exists("p.par")){
      res <- eRm::itemfit(p.par)$st.res
      pca <- psych::pca(res, nfactors = length(items), rotate = "none")
      if (max(pca$values<max_contrast)){
        return(list(items, model))
      }
    }
  }
}
