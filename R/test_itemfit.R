test_itemfit <- function(werte=NULL, dset=NULL, lower=0.7, upper=1.3, model=NULL, p.val=TRUE, modelType=NULL){
  if (is.null(model)){
    ds_test <- dset[werte]
    try(suppressWarnings({model <- get(modelType)(ds_test, se=TRUE)}))
  }

  if (exists("model")==T){
    try(p.par <- person.parameter(model))
    if (exists("p.par")){
      ifit <- itemfit(p.par)
      if (p.val==TRUE){
        if ((min(ifit$i.infitMSQ)>=lower) & (max(ifit$i.infitMSQ)<=upper) & (min(ifit$i.outfitMSQ)>=lower) & (max(ifit$i.outfitMSQ)<=upper) & (min(pchisq(ifit$i.fit, df=ifit$i.df, lower.tail=FALSE))>0.05)){
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
