library(eRm)
# p-value 0.078, but eRm excludes items due to inappropriate response
# patterns within subgroups. So, an empty list is returned
data(ADL)
testthat::test_that("test_LR: eRm excludes items due to inappropriate response
          patterns within subgroups  ",{
            testthat::expect_equal(length(
              exhaustiveRasch::test_LR(items=1:5, dset=ADL, na.rm=TRUE,
                                       modelType="RM", alpha=0.1,
                                       estimation_param=
                                         estimation_control(est="eRm"))),
                         expected=0)})


# list of 3 is returned (item combinations, fit rasch model and ppar)
data(ADL)
testthat::test_that("test_LR: p-value 0.929, alpha: 0.1,
                    Bonferroni correction",{
  testthat::expect_equal(length(
    exhaustiveRasch::test_LR(items=c(6,7,12,14,15), dset=ADL,
                              na.rm=TRUE, modelType="RM", alpha=0.1,
                             estimation_param=
                               estimation_control(est="psychotools"))),
               expected=3)})

# list of 3 is returned (item combinations, fit rasch model and ppar)
data(ADL)
testthat::test_that("test_LR: p-value 0.929, alpha: 0.1,
                    Bonferroni correction; na.rm=FALSE",{
  testthat::expect_equal(length(
    exhaustiveRasch::test_LR(items=c(6,7,12,14,15), dset=ADL,
                             na.rm=FALSE, modelType="RM", alpha=0.1,
                             estimation_param=
                               estimation_control(est="psychotools"))),
    expected=3)})


# list of 3 is returned (item combinations, fit rasch model and ppar)
data(ADL)
firstrun <- exhaustiveRasch::test_LR(items=c(6,7,12,14,15), dset=ADL,
                                     na.rm=FALSE, modelType="RM", alpha=0.1,
                                      estimation_param=
                                       estimation_control(est="eRm"))
testthat::test_that("test_LR: p-value 0.088, alpha: 0.05;
                    with pre-fitted model in 'items' parameter",{
                      testthat::expect_equal(length(
                        exhaustiveRasch::test_LR(
                          items=firstrun, dset=ADL,
                          na.rm=TRUE, modelType="RM", alpha=0.1,
                          estimation_param=
                            estimation_control(est="eRm"))),
                        expected=3)})
