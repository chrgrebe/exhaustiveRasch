test_itemfit<- function(werte=NULL, dset=NULL, control, modelType=NULL, model=NULL){
  #' checks the itemfit indices of a rasch model using the itemfit() function of eRm.
  #' @param werte a numeric vector containing the index numbers of the items in dset that are used to fit the model
  #' @param dset a data.frame containing the data
  #' @param control list object with options from \link{itemfit_control}
  #' @param model a list of type RM, PCM or RSM (a previously fit model) matching the value of modelType. If model is provided, this model ist used. If NULL, a model is fit using dset and werte.
  #' @param modelType a character value defining the rasch model to fit. Possible values: RM, PCM, RSM
  #' @return if all fit indices meet the given criteria, a list containing two elements is returned: the pattern that was tested an a list of type RM, RCM or RSM (depending on modelType) with the fit model. If at least one item's fit indices do not meet the given criteria, NULL is returned.
  #' @export
  if (is.null(model)){
    ds_test <- dset[werte]
    try(suppressWarnings({model <- get(modelType)(ds_test, se=TRUE)}))
  }


  if (exists("model")==T){
    try(p.par <- eRm::person.parameter(model))
    if (exists("p.par")){
      ifit <- eRm::itemfit(p.par)
      check=T
      if (control$p.val==TRUE & min(stats::pchisq(ifit$i.fit, df=ifit$i.df, lower.tail=FALSE))<0.05){
        check <- F # check for p.value
      }
      if (control$msq==T & (min(ifit$i.infitMSQ)<control$lowerMSQ | max(ifit$i.infitMSQ)>control$upperMSQ)){
        check <- F # check for MSQ infit
      }
      if (control$zstd==T & (min(ifit$i.infitZ)<control$lowerZ | max(ifit$i.infitZ)>control$upperZ)){
        check <- F # check for z-standardised infit
      }
      if (control$outfits==T){
        if (control$msq==T & (min(ifit$i.outfitMSQ)<control$lowerMSQ | max(ifit$i.outfitMSQ)>control$upperMSQ)){
          check <- F # check for MSQ outfit
        }
        if (control$zstd==T & (min(ifit$i.outfitZ)<control$lowerZ | max(ifit$i.outfitZ)>control$upperZ)){
          check <- F # check for z-standardised outfit
        }
      }
      if (check==T){return(list(werte, model))}
    }
  }

}

