test_pca <- function(werte=NULL, dset=NULL){
  arguments <- list(...)
  #' runs a principal component analysis (PCA) for binary data based on a tetrachoric correlation matrix
  #' @param werte a numeric vector containing the index numbers of the items in dset that are used to fit the model
  #' @param dset a data.frame containing the data
  #' @return if all items load on one
  #' @export
  ds_test <- dset[werte]
  km <- psych::tetrachoric(ds_test,y=NULL,correct=.5,smooth=TRUE,global=TRUE,weight=NULL,na.rm=TRUE, delete=TRUE)$rho
  eig <- as.list(eigen(km)$values)
  nr_fac <- length(eig[which(eig>1)])

  if (nr_fac==1){
    return(werte)
  }
}
