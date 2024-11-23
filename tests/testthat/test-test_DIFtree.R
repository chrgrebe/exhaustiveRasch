library(psychotree)
# list of 2 is returned (item combinations and fit rasch model)
data(ADL)
testthat::test_that("DIFtree: without DIF present",{
  testthat::expect_equal(length(
    exhaustiveRasch::test_DIFtree(items=1:5, dset=ADL, na.rm=TRUE,
                                  modelType="RM", DIFvars = ADL[16:17],
                                  estimation_param=
                                    estimation_control(est="eRm"))),
    expected=3)})

# list of 2 is returned (item combinations and fit rasch model)
data(ADL)
testthat::test_that("DIFtree: without DIF present; na.rm=FALSE",{
  testthat::expect_equal(length(
    exhaustiveRasch::test_DIFtree(items=1:5, dset=ADL, na.rm=FALSE,
                                  modelType="RM", DIFvars = ADL[16:17],
                                  estimation_param=
                                    estimation_control(est="psychotools"))),
    expected=3)})


# list of 2 is returned (item combinations and fit rasch model)
data(ADL)
firstrun <- exhaustiveRasch::test_DIFtree(items=1:5,
                                          dset=ADL, na.rm=TRUE, modelType="RM",
                                          DIFvars = ADL[16:17],
                                          estimation_param=
                                            estimation_control(
                                              est="psychotools"))
testthat::test_that("DIFtree: pre-fit model in the 'items' parameter",{
  testthat::expect_equal(length(
    exhaustiveRasch::test_DIFtree(items=firstrun, dset=ADL, na.rm=TRUE,
                                  modelType="RM", DIFvars = ADL[16:17],
                                  estimation_param=
                                    estimation_control(est="psychotools"))),
    expected=3)})


# empty list is returned because of DIF in this model
data(ADL)
testthat::test_that("DIFtree: with DIF present:",{
  testthat::expect_equal(length(
    exhaustiveRasch::test_DIFtree(items=c(1,2,3,4,8), dset=ADL, na.rm=TRUE,
                                  modelType="RM", DIFvars = ADL[16:17],
                                  estimation_param=
                                    estimation_control(est="psychotools"))),
    expected=0)})


