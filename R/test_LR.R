test_LR <- function(wwerte=NULL, dset=NULL, model=NULL, modelType=NULL){
  if (is.null(model)){
    ds_test <- dset[werte]
    try(suppressWarnings({model <- get(modelType)(ds_test, se=TRUE)}))
  }

  try(suppressWarnings({lr <- LRtest(model, splitcr="median")}))
  if (exists("lr")==T){
    print("ex")
    if (lr$pvalue >0.05 & length(lr$betalist$low)==length(ds_test)){
      return(list(werte, model))
    }
  }
}
