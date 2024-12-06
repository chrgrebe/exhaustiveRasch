library(eRm)
# list of 3 is returned (item combinations, fit rasch model and ppar)
data("InterprofessionalCollaboration")
testthat::test_that("threshold_order 1",{
  testthat::expect_equal(length(
    exhaustiveRasch::threshold_order(items=1:5,
                                     dset=InterprofessionalCollaboration,
                                     na.rm=T, modelType="PCM",
                                     estimation_param=
                                       estimation_control(est="eRm"))),
    expected=3)})


# list of 3 is returned (item combinations, fit rasch model and ppar)
data("InterprofessionalCollaboration")
testthat::test_that("threshold_order 1; na.rm=FALSE",{
  testthat::expect_equal(length(
    exhaustiveRasch::threshold_order(items=1:5,
                                     dset=InterprofessionalCollaboration,
                                     na.rm=FALSE, modelType="PCM",
                                     estimation_param=
                                       estimation_control(est="psychotools"))),
    expected=3)})


# list of 3 is returned (item combinations, fit rasch model and ppar)
data("InterprofessionalCollaboration")
firstrun <- exhaustiveRasch::threshold_order(items=c(1,2,3,5,7),
                                            dset=InterprofessionalCollaboration,
                                            na.rm=T, modelType="PCM",
                                            estimation_param=
                                              estimation_control(est="pairwise"))
testthat::test_that("threshold_order: pre-fit model in the 'items' parameter",{
  testthat::expect_equal(length(
    exhaustiveRasch::threshold_order(items=firstrun,
                                     dset=InterprofessionalCollaboration,
                                     na.rm=T, modelType="PCM",
                                     estimation_param=
                                       estimation_control(est="pairwise"))),
    expected=3)})




