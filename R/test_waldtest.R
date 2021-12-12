test_waldtest <- function(werte=NULL, dset=NULL, model=NULL, modelType=NULL){
  if (is.null(model)){
    ds_test <- dset[werte]
    try(suppressWarnings({model <- get(modelType)(ds_test, se=TRUE)}))
  }

  try(suppressWarnings({wald <- Waldtest(model)}))
  if (exists("wald")==T){
    if (min(wald$coef.table[,2]) >0.05){
      return(list(werte, model))
    }
  }
}
