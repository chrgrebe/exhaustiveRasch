library(eRm)
# with pre-defined item-combinations
combos <- list(
  c(1,2,3,4,5),
  c(1,2,3,4,6),
  c(1,2,7,10,15),
  c(2,3,7,12,13),
  c(2,4,7,10,15)
)
data(ADL)
passed_ADL <- exhaustiveRasch::exhaustive_tests(
  dset=ADL[1:15], combos=combos, modelType= "RM",
  upperMSQ=1.5, lowerMSQ=0.5, use.pval=F, bonf=T,
  na.rm=T, tests=c("test_itemfit"))
testthat::test_that("summary error: object of classpassed_exRa",{
  testthat::expect_no_error(
    exhaustiveRasch::summary(passed_ADL), message = NULL, class = NULL)
})
testthat::test_that("summary warning: no object of classpassed_exRa",{
    testthat::expect_no_warning(
      exhaustiveRasch::summary(passed_ADL), message = NULL, class = NULL)
  })


# with scale length
data(ADL)
passed_ADL <- exhaustiveRasch::exhaustive_tests(
  dset=ADL[1:6], scale_length = 4:4, modelType= "RM",
  upperMSQ=1.5, lowerMSQ=0.5, use.pval=F, bonf=T,
  na.rm=T, tests=c("test_itemfit"))
testthat::test_that("summary error: object of classpassed_exRa",{
  testthat::expect_no_error(
    exhaustiveRasch::summary(passed_ADL), message = NULL, class = NULL)
})
