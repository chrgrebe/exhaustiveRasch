combo_list <- list(c(1,2,3,4), c(1,2,3,4,6), c(1,3,4,7))

# keeping superset
testthat::test_that("remove_subset: removes subset of longer superset",{
  testthat::expect_equal(remove_subsets(combo_list, keep_longest = TRUE),
               expected=list(c(1,2,3,4,6), c(1,3,4,7)))})
# keeping subset
testthat::test_that("remove_subset: removes superset of shorter subset",{
  testthat::expect_equal(remove_subsets(combo_list, keep_longest = FALSE),
               expected=list(c(1,2,3,4), c(1,3,4,7)))})
