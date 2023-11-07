library(eRm)
# numeric vector with the length of the items parameter (5) is returned
#table(rowSums(ADL[1:5]))
#0   1   2   3   4   5
#110 130  36  21  39 252
data("ADL")
testthat::test_that("all_rawscores: RM",{
  testthat::expect_equal(length(
    exhaustiveRasch::all_rawscores(items=1:5,
                                     dset=ADL,
                                     na.rm=T, modelType="RM")[[1]]),
    expected=5)})

data("ADL")
testthat::test_that("all_rawscores: RM; na.rm=FALSE",{
  testthat::expect_equal(length(
    exhaustiveRasch::all_rawscores(items=1:5,
                                   dset=ADL,
                                   na.rm=F, modelType="RM")[[1]]),
    expected=5)})


# nothing is returend, as not all rawscores are represented in the data
data("InterprofessionalCollaboration")
testthat::test_that("all_rawscores: PCM",{
  testthat::expect_equal(length(
    exhaustiveRasch::all_rawscores(items=1:15,
                                     dset=InterprofessionalCollaboration,
                                     na.rm=T, modelType="PCM")),
    expected=0)})

