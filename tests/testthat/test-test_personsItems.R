library(eRm)
# list of 3 is returned (item combinations, fit rasch model and ppar)
data(ADL)
testthat::test_that("test_personsItems",{
  testthat::expect_equal(length(exhaustiveRasch::test_personsItems(
    items=1:5, dset=ADL, na.rm=TRUE, modelType="RM",
    estimation_param= estimation_control(est="psychotools"))),
    expected=3)})

# list of 3 is returned (item combinations, fit rasch model and ppar)
data(ADL)
firstrun <- exhaustiveRasch::test_personsItems(
  items=1:5, dset=ADL, na.rm=TRUE, modelType="RM",
  estimation_param= estimation_control(est="eRm"))
testthat::test_that("test_personsItems with pre-fit model
                    in the 'items' parameter",{
  testthat::expect_equal(length(exhaustiveRasch::test_personsItems(
    items=firstrun, dset=ADL, na.rm=TRUE, modelType="RM",
    estimation_param= estimation_control(est="eRm"))),
    expected=3)})


# empty list is returned
data(ADL)
testthat::test_that("test_personsItems",{
  testthat::expect_equal(length(exhaustiveRasch::test_personsItems(
    items=c(1,2,3,4,8), dset=ADL, na.rm=TRUE, modelType="RM",
    estimation_param= estimation_control(est="pairwise"))),
    expected=0)})

# list of 3 is returned (item combinations, fit rasch model and ppar)
data(ADL)
testthat::test_that("test_personsItems with gap_prop",{
  testthat::expect_equal(length(exhaustiveRasch::test_personsItems(
    items=1:5, dset=ADL, na.rm=TRUE, modelType="RM", gap_prop = 0.6,
    estimation_param= estimation_control(est="eRm"))),
    expected=3)})

