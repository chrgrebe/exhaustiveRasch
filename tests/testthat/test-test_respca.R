library(eRm)

# list of 3 is returned (item combinations, fit rasch model and ppar)
data(ADL)
testthat::test_that("test_respca",{
  testthat::expect_equal(length(exhaustiveRasch::test_respca(
    items=1:4, dset=ADL, na.rm=TRUE, modelType="RM",
    estimation_param= estimation_control(est="eRm"))),
    expected=3)})

# list of 3 is returned (item combinations, fit rasch model and ppar)
data(ADL)
firstrun <- exhaustiveRasch::test_respca(
  items=1:4, dset=ADL, na.rm=TRUE, modelType="RM",
  estimation_param= estimation_control(est="eRm"))
testthat::test_that("test_respca eRm with pre-fit model in the
                    'items' parameter",{
  testthat::expect_equal(length(exhaustiveRasch::test_respca(
    items=firstrun, dset=ADL, na.rm=TRUE, modelType="RM",
    estimation_param= estimation_control(est="eRm"))),
    expected=3)})

# list of 3 is returned (item combinations, fit rasch model and ppar)
data(ADL)
firstrun <- exhaustiveRasch::test_respca(
  items=1:4, dset=ADL, na.rm=TRUE, modelType="RM",
  estimation_param= estimation_control(est="psychotools"))
testthat::test_that("test_respca psychotools with pre-fit model in the
                    'items' parameter",{
    testthat::expect_equal(length(exhaustiveRasch::test_respca(
      items=firstrun, dset=ADL, na.rm=TRUE, modelType="RM",
      estimation_param= estimation_control(est="psychotools"))),
      expected=3)})


# empty list is returned, as no RM model can be fit
data(ADL)
testthat::test_that("test_respca, no model, eRm",{
  testthat::expect_equal(length(exhaustiveRasch::test_respca(
    items=c(1,2,3,8), dset=ADL, na.rm=TRUE, modelType="RM",
    estimation_param= estimation_control(est="eRm"))),
    expected=0)})

# same case, but psychotools: list of 3 is returned (item combinations,
# fit rasch model and ppar) is returned, because the model can be fit (no
# datachecks for ill conditioned data matrix)
data(ADL)
testthat::test_that("test_respca, no model, psychotools",{
  testthat::expect_equal(length(exhaustiveRasch::test_respca(
    items=c(1,2,3,8), dset=ADL, na.rm=TRUE, modelType="RM",
    estimation_param= estimation_control(est="psychotools"))),
    expected=3)})

# same case, but pairwise: list of 3 is returned (item combinations,
# fit rasch model and ppar)
data(ADL)
testthat::test_that("test_respca, no model, psychotools",{
  testthat::expect_equal(length(exhaustiveRasch::test_respca(
    items=c(1,2,3,8), dset=ADL, na.rm=TRUE, modelType="RM",
    estimation_param= estimation_control(est="pairwise"))),
    expected=3)})
