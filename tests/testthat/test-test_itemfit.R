library(eRm)
#Itemfit Statistics:
#  Chisq  df p-value Outfit MSQ Infit MSQ Outfit t Infit t Discrim
#V12 182.432 258   1.000      0.704     0.783   -1.835  -3.064   0.294
#V22 138.778 258   1.000      0.536     0.802   -2.181  -2.441   0.007
#V27 225.681 258   0.927      0.871     0.910   -0.541  -1.146  -0.062
#V36 137.030 258   1.000      0.529     0.707   -3.602  -4.180   0.436

# only MSQ infits
# list of 3 is returned (item combinations, fit rasch model and ppar)
data(ADL)
testthat::test_that("test_itemfit: MSQ infits 0.7-1.3",{
  testthat::expect_equal(length(exhaustiveRasch::test_itemfit(
    items=c(6,7,12,14), dset=ADL, na.rm=TRUE, modelType="RM",
                                   exhaustiveRasch::itemfit_control(
                                     outfits = FALSE, use.pval = FALSE),
    estimation_param= estimation_control(est="eRm"))),
               expected=3)})

# only MSQ infits
# list of 3 is returned (item combinations, fit rasch model and ppar)
data(ADL)
testthat::test_that("test_itemfit: MSQ infits 0.7-1.3; na.rm=FALSE",{
  testthat::expect_equal(length(exhaustiveRasch::test_itemfit(
    items=c(6,7,12,14), dset=ADL, na.rm=FALSE, modelType="RM",
    exhaustiveRasch::itemfit_control(
      outfits = FALSE, use.pval = FALSE),
    estimation_param= estimation_control(est="psychotools"))),
    expected=3)})


# MSQ infits and outfits: empty list is returned
data(ADL)
testthat::test_that("test_itemfit: MSQ in- and outfits 0.7-1.3",{
  testthat::expect_equal(length(exhaustiveRasch::test_itemfit(
    items=c(6,7,12,14), dset=ADL, na.rm=TRUE, modelType="RM",
                                   exhaustiveRasch::itemfit_control(
                                     outfits = TRUE, use.pval = FALSE),
    estimation_param= estimation_control(est="psychotools"))),
               expected=0)})

# MSQ infits and p-values
# list of 3 is returned (item combinations, fit rasch model and ppar)
data(ADL)
testthat::test_that("test_itemfit: MSQ infits and p-values",{
  testthat::expect_equal(length(exhaustiveRasch::test_itemfit(
    items=c(6,7,12,14), dset=ADL, na.rm=TRUE, modelType="RM",
                                   exhaustiveRasch::itemfit_control(
                                     outfits = FALSE, use.pval = TRUE),
    estimation_param= estimation_control(est="psychotools"))),
               expected=3)})

# standardized infits and outfits: empty list is returned
data(ADL)
testthat::test_that("test_itemfit: std in- and outfits",{
  testthat::expect_equal(length(exhaustiveRasch::test_itemfit(
    items=c(6,7,12,14), dset=ADL, na.rm=TRUE, modelType="RM",
                                   exhaustiveRasch::itemfit_control(
                                     outfits = TRUE, use.pval = FALSE,
                                     zstd=TRUE),
    estimation_param= estimation_control(est="psychotools"))),
               expected=0)})

# only MSQ infits with pre-fit model in the 'itemss' parameter:
# list of 3 is returned (item combinations, fit rasch model and ppar)
data(ADL)
firstrun <- exhaustiveRasch::test_itemfit(
  items=c(6,7,12,14), dset=ADL, na.rm=TRUE, modelType="RM",
  exhaustiveRasch::itemfit_control(outfits = FALSE,  use.pval = FALSE),
  estimation_param= estimation_control(est="psychotools"))
testthat::test_that("test_itemfit: pre-fit model",{
  testthat::expect_equal(length(exhaustiveRasch::test_itemfit(
    items=firstrun, dset=ADL, na.rm=TRUE, modelType="RM",
    exhaustiveRasch::itemfit_control(outfits = FALSE,  use.pval = FALSE),
    estimation_param= estimation_control(est="psychotools"))),
                         expected=3)})
