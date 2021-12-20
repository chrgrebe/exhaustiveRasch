test_pca <- function(werte=NULL, dset=NULL, na.rm=T){

  #' runs a principal component analysis (PCA) for binary data based on a tetrachoric correlation matrix
  #' @param werte a numeric vector containing the index numbers of the items in dset that are used to fit the model
  #' @param dset a data.frame containing the data
  #' @param na.rm a boolean value. If TRUE, all cases with any NA are removed (na.omit). If FALSE, only cases with full NA responses are removed
  #' @return if all items load on one
  #' @export
  ds_test <- dset[werte]
  if (na.rm==T){ds_test<- stats::na.omit(ds_test)} else{ds_test <- ds_test <- ds_test[rowSums(is.na(ds_test)) < ncol(ds_test)-1, ]}
  km <- psych::tetrachoric(ds_test,y=NULL,correct=.5,smooth=TRUE,global=TRUE,weight=NULL,na.rm=na.rm, delete=TRUE)$rho
  eig <- as.list(eigen(km)$values)
  nr_fac <- length(eig[which(eig>1)])

  if (nr_fac==1){
    return(werte)
  }
}

