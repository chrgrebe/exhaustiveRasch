library(eRm)
# list of 2 is returned (item combinations and fit rasch model)
data("InterprofessionalCollaboration")
testthat::test_that("threshold_order 1",{
  testthat::expect_equal(length(
    exhaustiveRasch::threshold_order(items=1:5,
                                     dset=InterprofessionalCollaboration,
                                     na.rm=T, modelType="PCM")),
    expected=2)})

data("InterprofessionalCollaboration")
testthat::test_that("threshold_order 1; na.rm=FALSE",{
  testthat::expect_equal(length(
    exhaustiveRasch::threshold_order(items=1:5,
                                     dset=InterprofessionalCollaboration,
                                     na.rm=FALSE, modelType="PCM")),
    expected=2)})


# list of 2 is returned (item combinations and fit rasch model)
data("InterprofessionalCollaboration")
firstrun <- exhaustiveRasch::threshold_order(items=1:5,
                                            dset=InterprofessionalCollaboration,
                                            na.rm=T, modelType="PCM")
testthat::test_that("threshold_order: pre-fit model in the 'items' parameter",{
  testthat::expect_equal(length(
    exhaustiveRasch::threshold_order(items=firstrun,
                                     dset=InterprofessionalCollaboration,
                                     na.rm=T, modelType="PCM")),
    expected=2)})


