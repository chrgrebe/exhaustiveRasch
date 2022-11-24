library(eRm)

# list of 2 is returned (item combinations and fit rasch model)
data(ADL)
testthat::test_that("test_respca",{
  testthat::expect_equal(length(exhaustiveRasch::test_respca(
    items=1:4, dset=ADL, na.rm=TRUE, modelType="RM")),
    expected=2)})

# list of 2 is returned (item combinations and fit rasch model)
data(ADL)
firstrun <- exhaustiveRasch::test_respca(
  items=1:4, dset=ADL, na.rm=TRUE, modelType="RM")
testthat::test_that("test_respca with pre-fit model in the 'items' parameter",{
  testthat::expect_equal(length(exhaustiveRasch::test_respca(
    items=firstrun, dset=ADL, na.rm=TRUE, modelType="RM")),
    expected=2)})


# empty list is returned, as no RM model can be fit
data(ADL)
testthat::test_that("test_respca",{
  testthat::expect_equal(length(exhaustiveRasch::test_respca(
    items=c(1,2,3,8), dset=ADL, na.rm=TRUE, modelType="RM")),
    expected=0)})

