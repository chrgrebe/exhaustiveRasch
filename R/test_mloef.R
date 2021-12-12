test_mloef <- function(werte=NULL, dset=NULL, model=NULL, modelType=NULL){

  ds_test <- dset[,werte]
  try(suppressWarnings({model <- get(modelType)(ds_test, se=TRUE)}))

  if (exists("model")==T){
    try(ml <- MLoef(model, splitcr="median"))
    if (exists("ml")==T){
      if (ml$p.value >0.05){
        return(list(werte, model))
      }
    }
  }
}
