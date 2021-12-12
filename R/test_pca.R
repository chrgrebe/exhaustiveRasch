test_pca <- function(werte=NULL, dset=NULL, model=NULL, modelType=NULL){
  ds_test <- dset[werte]
  km <- tetrachoric(ds_test,y=NULL,correct=.5,smooth=TRUE,global=TRUE,weight=NULL,na.rm=TRUE, delete=TRUE)$rho
  eig <- as.list(eigen(km)$values)
  nr_fac <- length(eig[which(eig>1)])

  if (nr_fac==1){
    return(werte)
  }
}
