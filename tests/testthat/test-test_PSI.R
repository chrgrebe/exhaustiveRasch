# list of 3 is returned (item combinations, fit rasch model and ppar)
data(cognition)
testthat::test_that("test_PSI",{
  testthat::expect_equal(length(
    exhaustiveRasch::test_PSI(items=c(1,2,5,6,9,10,14,15), dset=cognition, na.rm=TRUE,
                                   modelType="PCM", PSI=0.8,
                                   estimation_param=
                                     estimation_control(est="psychotools"))),
    expected=3)})

data(cognition)
testthat::test_that("test_PSI",{
  testthat::expect_equal(length(
    exhaustiveRasch::test_PSI(items=c(1,2,5,6,9,10,14,15), dset=cognition, na.rm=TRUE,
                              modelType="PCM", PSI=0.8,
                              estimation_param=
                                estimation_control(est="pairwise"))),
    expected=3)})

data(cognition)
testthat::test_that("test_PSI",{
  testthat::expect_equal(length(
    exhaustiveRasch::test_PSI(items=c(1,2,5,6,9,10,14,15), dset=cognition, na.rm=TRUE,
                              modelType="PCM", PSI=0.8,
                              estimation_param=
                                estimation_control(est="eRm"))),
    expected=3)})

data(cognition)
testthat::test_that("test_PSI",{
  testthat::expect_equal(length(
    exhaustiveRasch::test_PSI(items=c(1,2,3,4,5,6,7), dset=cognition, na.rm=TRUE,
                              modelType="PCM", PSI=0.8,
                              estimation_param=
                                estimation_control(est="pairwise"))),
    expected=0)})
