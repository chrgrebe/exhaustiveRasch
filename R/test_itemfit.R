test_itemfit <- function(werte=NULL, dset=NULL, lower=0.7, upper=1.3, model=NULL, p.val=TRUE, modelType=NULL){
  #' checks the itemfit indices of a rasch model using the itemfit() function of eRm.
  #' @param werte a numeric vector containing the index numbers of the items in dset that are used to fit the model
  #' @param dset a data.frame containing the data
  #' @lower lower bound for acceptable item fit (MSQ fit indices are used).
  #' @upper upper bound for acceptable item fit (MSQ fit indices are used).
  #' @param model a list of type RM, PCM or RSM (a previously fit model) matching the value of modelType. If model is provided, this model ist used. If NULL, a model is fit using dset and werte.
  #' @param modelType: a character value defining the rasch model to fit. Possible values: RM, PCM, RSM
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
      if (p.val==TRUE){
        if ((min(ifit$i.infitMSQ)>=lower) & (max(ifit$i.infitMSQ)<=upper) & (min(ifit$i.outfitMSQ)>=lower) & (max(ifit$i.outfitMSQ)<=upper) & (min(stats::pchisq(ifit$i.fit, df=ifit$i.df, lower.tail=FALSE))>0.05)){
          return(list(werte, model))
        }
      } else{
        if ((min(ifit$i.infitMSQ)>=lower) & (max(ifit$i.infitMSQ)<=upper) & (min(ifit$i.outfitMSQ)>=lower) & (max(ifit$i.outfitMSQ)<=upper)){
          return(list(werte, model))
        }
      }
    }
  }

}
