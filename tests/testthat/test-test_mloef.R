library(eRm)
# list of 2 is returned (item combinations and fit rasch model)
data(ADL)
testthat::test_that("test_mloef: p-value 0.088, alpha: 0.05",{
  testthat::expect_equal(length(
    exhaustiveRasch::test_mloef(items=1:5, dset=ADL, na.rm=TRUE, modelType="RM",
                                 alpha=0.05)),
               expected=2)})

# list of 2 is returned (item combinations and fit rasch model)
data(ADL)
testthat::test_that("test_mloef: p-value 0.088, alpha: 0.05; na.rm=FALSE",{
  testthat::expect_equal(length(
    exhaustiveRasch::test_mloef(items=1:5, dset=ADL, na.rm=FALSE, modelType="RM",
                                alpha=0.05)),
    expected=2)})

# list of 2 is returned (item combinations and fit rasch model)
data(ADL)
firstrun <- exhaustiveRasch::test_mloef(
  items=1:5, dset=ADL, na.rm=T, modelType="RM", alpha=0.05)
testthat::test_that("test_mloef: p-value 0.088, alpha: 0.05;
                    with pre-fitted model in 'items' parameter",{
  testthat::expect_equal(length(
    exhaustiveRasch::test_mloef(items=firstrun, dset=ADL, na.rm=TRUE,
                                modelType="RM", alpha=0.05)),
                         expected=2)})


# empty list is returned
data(ADL)
testthat::test_that("test_mloef: p-value 0.088, alpha: 0.1",{
  testthat::expect_equal(length(
    exhaustiveRasch::test_mloef(items=1:5, dset=ADL, na.rm=TRUE, modelType="RM",
  )),
  expected=0)})

