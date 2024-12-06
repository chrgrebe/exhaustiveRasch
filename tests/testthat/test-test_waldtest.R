library(eRm)
# Wald test on item level (z-values):
# z-statistic p-value
# beta V06       2.115   0.034
# beta V07      -0.934   0.350
# beta V08      -0.660   0.509
# test fits criteria, but eRm excludes items due to inappropriate response
# patterns within subgroups
# empty list is returned
data(ADL)
testthat::test_that("test_waldtest: inapprpriate response patterns",{
  testthat::expect_equal(length(exhaustiveRasch::test_waldtest(items=1:5,
                                    dset=ADL, na.rm=TRUE,
                                    modelType="RM",
                                    bonf=TRUE,
                                    estimation_param=
                                      estimation_control(est="eRm"))),
               expected=0)})


#Wald test on item level (z-values):
# z-statistic p-value
# beta V12      -0.169   0.866
# beta V22       0.391   0.696
# beta V27       0.488   0.626
# beta V36      -0.645   0.519
# beta V39      -0.128   0.899

# list of 3 is returned (item combinations, fit rasch model and ppar)
data(ADL)
testthat::test_that("test_waldtest: lowest p-value=0.519",{
  testthat::expect_equal(length(
    exhaustiveRasch::test_waldtest(items=c(6,7,12,14,15), dset=ADL, na.rm=TRUE,
                                    modelType="RM", bonf=FALSE,
                                   estimation_param=
                                     estimation_control(est="psychotools"))),
               expected=3)})

# list of 3 is returned (item combinations, fit rasch model and ppar)
data(ADL)
testthat::test_that("test_waldtest: lowest p-value=0.519; na.rm=FALSE",{
  testthat::expect_equal(length(
    exhaustiveRasch::test_waldtest(items=c(6,7,12,14,15), dset=ADL, na.rm=FALSE,
                                   modelType="RM", bonf=FALSE,
                                   estimation_param=
                                     estimation_control(est="psychotools"))),
    expected=3)})

# list of 3 is returned (item combinations, fit rasch model and ppar)
data(ADL)
firstrun <- exhaustiveRasch::test_waldtest(
  items=c(6,7,12,14,15), dset=ADL, na.rm=TRUE,  modelType="RM", bonf=FALSE,
  estimation_param=
    estimation_control(est="psychotools"))
testthat::test_that("test_waldtest: lowest p-value=0.519; pre-fit model
                    in the 'items' parameter",{
  testthat::expect_equal(length(
    exhaustiveRasch::test_waldtest(items=firstrun, dset=ADL, na.rm=TRUE,
                                              modelType="RM", bonf=F,
                                   estimation_param=
                                     estimation_control(est="psychotools"))),
                         expected=3)})




# list of 3 is returned (item combinations, fit rasch model and ppar)
data(cognition)
testthat::test_that("test_waldtest: ",{
  testthat::expect_equal(length(
    exhaustiveRasch::test_waldtest(items=c(1:5,7), dset=cognition, na.rm=T,
                                   modelType="PCM", bonf=FALSE, alpha=0.05,
                                   icat=F, splitcr="random",
                                   estimation_param=
                                     estimation_control(est="pairwise",
                                                        splitseed=332))),
    expected=3)})

