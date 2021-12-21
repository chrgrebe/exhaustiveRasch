test_mloef <- function(werte=NULL, dset=NULL, na.rm=T, model=NULL, modelType=NULL, splitcr="median", alpha=0.1, bonf=F){
  #' runs Martin-Loef Test using the MLoef() function of eRm.
  #' @param werte a numeric vector containing the index numbers of the items in dset that are used to fit the model
  #' @param dset a data.frame containing the data
  #' @param na.rm a boolean value. If TRUE, all cases with any NA are removed (na.omit). If FALSE, only cases with full NA responses are removed
  #' @param model a list of type RM, PCM or RSM (a previously fit model) matching the value of modelType. If model is provided, this model ist used. If NULL, a model is fit using dset and werte.
  #' @param modelType a character value defining the rasch model to fit. Possible values: RM, PCM, RSM
  #' @param splitcr as defined by eRm::mloef: Split criterion to define the item groups. "median" and "mean" split items in two groups based on their items' raw scores. splitcr can also be a vector of length k (where k denotes the number of items) that takes two or more distinct values to define groups used for the Martin-L?f Test.
  #' @param alpha a numeric value for the alpha level
  #' @param alpha a numeric value for the alpha level. Will be ignored if use.pval is FALSE
  #' @param bonf a boolean value wheter to use a Bonferroni correction. Will be ignored if use.pval is FALSE
  #' @return if the p-value of the test is not significant (above p=0.05), a list containing two elements is returned: the pattern that was tested an a list of type RM, RCM or RSM (depending on modelType) with the fit model. If the test is significant (p<0.05), NULL is returned.
  #' @export

  if (bonf==T){local_alpha <- alpha/length(werte)} else{local_alpha <- alpha}

  ds_test <- dset[,werte]
  if (na.rm==T){ds_test<- stats::na.omit(ds_test)} else{ds_test <- ds_test <- ds_test[rowSums(is.na(ds_test)) < ncol(ds_test)-1, ]}
  try(suppressWarnings({model <- get(modelType)(ds_test, se=TRUE)}))

  if (exists("model")==T){
    try(ml <- eRm::MLoef(model, splitcr=splitcr))
    if (exists("ml")==T){
      if (ml$p.value >=local_alpha){
        return(list(werte, model))
      }
    }
  }
}

