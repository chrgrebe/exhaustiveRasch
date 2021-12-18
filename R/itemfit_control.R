itemfit_control <- function(zstd=F, msq=T, outfits=F, p.val=F, lowerMSQ=0.7, upperMSQ=1.3, lowerZ=-2, upperZ=2){
  #' options for test_itemfit()
  #' @param zstd a boolean value whether to check the z-standardised fit indices
  #' @param msq aboolean value whether to check the mean-squared fit indices
  #' @param p.val a boolean value whether to exclude patterns with at least one item with significant p-value (p<0.05)
  #' @param outfits a boolean value whether to check outfit indices (in FALSE, only infits are checked)
  #' @param lowerMSQ a numeric vaue for the lower bound for acceptable Infit (mean-squared fit indices)
  #' @param upperMSQ a numeric vaue for the upper bound for acceptable Infit (mean-squared fit indices)
  #' @param lowerZ a numeric vaue for the lower bound for acceptable Infit (z-standardised fit indices)
  #' @param upperZ a numeric vaue for the upper bound for acceptable Infit (z-standardised fit indices)
  #' @return a list containing the options
  #' @export

  return(list("zstd"= zstd, "msq"= msq, "outfits"= outfits, "p.val"= p.val, "lowerMSQ"= lowerMSQ,
              "upperMSQ"= upperMSQ, "lowerZ"= lowerZ, "upperZ"= upperZ))
}
