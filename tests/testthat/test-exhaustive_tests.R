library(eRm)
combos <- list(
  c(1,2,3,4,5),
  c(1,2,3,4,6),
  c(1,2,7,10,15),
  c(2,3,7,12,13),
  c(2,4,7,10,15)
)

# test 1
data(ADL)
testthat::test_that("exhaustive_tests",{
  testthat::expect_s4_class(
    exhaustiveRasch::exhaustive_tests(dset=ADL[1:15],
                            combos=combos, modelType= "RM",
                            upperMSQ=1.5, lowerMSQ=0.5, use.pval=FALSE,
                            bonf=TRUE, na.rm=TRUE, tests=c("test_itemfit")),
    class="passed_exRa")})

# test 2
data(ADL)
testthat::test_that("exhaustive_tests",{
  testthat::expect_s4_class(
    exhaustiveRasch::exhaustive_tests(dset=ADL[1:15],
                                      combos=combos, modelType= "RM",
                                      upperMSQ=1.5, lowerMSQ=0.5,
                                      use.pval=FALSE, bonf=TRUE,
                                      na.rm=FALSE, tests=c(
                                        "test_itemfit",
                                        "all_rawscores")),
    class="passed_exRa")})

# test 3
data(ADL)
testthat::test_that("exhaustive_tests",{
  testthat::expect_s4_class(
    exhaustiveRasch::exhaustive_tests(dset=ADL[1:6],
                                      scale_length=4:4, modelType= "RM",
                                      upperMSQ=1.5, lowerMSQ=0.5,
                                      use.pval=FALSE, bonf=TRUE,
                                      na.rm=FALSE, DIFvars=ADL[16:17], tests=c(
                                        "test_LR")),
    class="passed_exRa")})
