library(eRm)
# list of 2 is returned (item combinations and fit rasch model)
data(ADL)
testthat::test_that("no-test: length of return",{
  testthat::expect_equal(length(
    exhaustiveRasch::no_test(items=1:5, dset=ADL, na.rm=TRUE,
                             modelType="RM", estimation_param=
                               estimation_control(est="eRm"))),
               expected=2)})

# list of 2 is returned (item combinations and fit rasch model)
data(ADL)
testthat::test_that("no-test: length of return; na.rm=FALSE",{
  testthat::expect_equal(length(
    exhaustiveRasch::no_test(items=1:5, dset=ADL, na.rm=FALSE,
                             modelType="RM", estimation_param=
                               estimation_control(est="psychotools"))),
    expected=2)})


#2nd list entry is an eRm rasch model (list of length 14)
data(ADL)
testthat::test_that("no-test: eRm rasch object returned",{
  testthat::expect_equal(length(exhaustiveRasch::no_test(items=1:5,
                                                         dset=ADL, na.rm=TRUE,
                              modelType="RM", estimation_param=
                                estimation_control(est="psychotools"))[[2]]),
                         expected=14)})

# list of 2 is returned (item combinations and fit rasch model)
data(ADL)
firstrun <- exhaustiveRasch::no_test(
  items=1:5, dset=ADL, na.rm=T, modelType="RM", estimation_param=
    estimation_control(est="psychotools"))
testthat::test_that("no-test: pre-fit model in the 'items' parameter",{
  testthat::expect_equal(length(
    exhaustiveRasch::no_test(items=firstrun, dset=ADL, na.rm=TRUE,
                             modelType="RM", estimation_param=
                               estimation_control(est="psychotools"))),
    expected=2)})

