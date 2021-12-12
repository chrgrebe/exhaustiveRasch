all_rawscores <- function(werte=NULL, dset=NULL, model=NULL, modelType=NULL){
  ds_test <- dset[,werte]
  scores <- rowSums(ds_test)
  if (length(unique(scores))==(length(ds_test)+1)){
    return(werte)
  }
}
