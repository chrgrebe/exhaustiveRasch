library(eRm)
# list of 2 is returned (item combinations and fit rasch model)
data(ADL)
testthat::test_that("test_personsItems",{
  testthat::expect_equal(length(exhaustiveRasch::test_personsItems(
    items=1:5, dset=ADL, na.rm=TRUE, modelType="RM")),
    expected=2)})

# list of 2 is returned (item combinations and fit rasch model)
data(ADL)
firstrun <- exhaustiveRasch::test_personsItems(
  items=1:5, dset=ADL, na.rm=TRUE, modelType="RM")
testthat::test_that("test_personsItems with pre-fit model
                    in the 'items' parameter",{
  testthat::expect_equal(length(exhaustiveRasch::test_personsItems(
    items=firstrun, dset=ADL, na.rm=TRUE, modelType="RM")),
    expected=2)})


# empty list is returned
data(ADL)
testthat::test_that("test_personsItems",{
  testthat::expect_equal(length(exhaustiveRasch::test_personsItems(
    items=c(1,2,3,4,8), dset=ADL, na.rm=TRUE, modelType="RM")),
    expected=0)})

# list of 2 is returned (item combinations and fit rasch model)
data(ADL)
testthat::test_that("test_personsItems with gap_prop",{
  testthat::expect_equal(length(exhaustiveRasch::test_personsItems(
    items=1:5, dset=ADL, na.rm=TRUE, modelType="RM", gap_prop = 0.6)),
    expected=2)})

