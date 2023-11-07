itemfit_control <- function(zstd=FALSE,
                            msq=TRUE,
                            outfits=TRUE,
                            use.pval=TRUE,
                            lowerMSQ=0.7,
                            upperMSQ=1.3,
                            lowerZ=-1.96,
                            upperZ=1.96){
  #' options for test_itemfit()
  #' @param zstd a boolean value whether to check the standardised fit indices
  #' @param msq aboolean value whether to check the mean-squared fit indices
  #' @param use.pval a boolean value whether to exclude combinations with at
  #'  least one item with significant p-value
  #' @param outfits a boolean value whether to check outfit indices
  #'  (if FALSE, only infits are checked)
  #' @param lowerMSQ a numeric value for the lower bound for acceptable fit
  #'  (mean-squared fit indices)
  #' @param upperMSQ a numeric value for the upper bound for acceptable fit
  #'  (mean-squared fit indices)
  #' @param lowerZ a numeric value for the lower bound for acceptable fit
  #'  (standardised fit indices)
  #' @param upperZ a numeric value for the upper bound for acceptable fit
  #'  (standardised fit indices)
  #' @return a list containing the options
  #' @export

  return(list("zstd"= zstd, "msq"= msq, "outfits"= outfits,
              "use.pval"= use.pval, "lowerMSQ"= lowerMSQ,
              "upperMSQ"= upperMSQ, "lowerZ"= lowerZ, "upperZ"= upperZ))
}
