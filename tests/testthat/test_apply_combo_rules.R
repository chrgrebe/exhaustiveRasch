# apply_combo_rules(1:6, 4) should return 15 combinations, because the binomial
# coefficient for k=4 out of n=6 items is 15.
testthat::test_that("apply_combo_rules: return without any rules",{
  testthat::expect_equal(length(apply_combo_rules(1:6, 4)),
               expected=15)})

# added parameter forced_items=c(2,3) should return 6 combinations that contain
# the value 2 and 3
testthat::test_that("apply_combo_rules: forced",{
  testthat::expect_equal(length(apply_combo_rules(1:6, 4, forced_items=c(2,3))),
               expected=6)})

# this "min" rule should return 14 combination, as 3-4-5-6 is the only
# combination without 1 or 2
rules <- list()
rules[[1]] <- list("min", 1, 1:2)
testthat::test_that("apply_combo_rules: min",{
  testthat::expect_equal(length(apply_combo_rules(1:6, 4, rules=rules)),
               expected=14)})

# this "max" rule should return 3 combinations, as 1-4-5-6, 2-4-5-6 and
# 3-4-5-6 are the only combination without only one value out of 1,2,3
rules <- list()
rules[[1]] <- list("max", 1, 1:3)
testthat::test_that("apply_combo_rules: max",{
  testthat::expect_equal(length(apply_combo_rules(1:6, 4, rules=rules)),
               expected=3)})

# these rules should return 4 combinations, as 1-3-5-6, 1-4-5-6, 2-3-5-6 and
# 2-4-5-6 are the combinations that pass this rule.
rules <- list()
rules[[1]] <- list("max", 1, 1:2)
rules[[2]] <- list("min", 1, 5:6)
rules[[3]] <- list("forbidden", c(3:4))
testthat::test_that("apply_combo_rules: min, max, forbidden",{
  testthat::expect_equal(length(apply_combo_rules(1:6, 4, rules=rules)),
               expected=4)})

# these rules should return an empty list. There is no combination that
# passes these rules, because the rule allow only combinations with a length
# of 3.
rules <- list()
rules[[1]] <- list("max", 1, 1:2)
rules[[2]] <- list("max", 1, 3:4)
rules[[3]] <- list("max", 1, 5:6)
testthat::test_that("apply_combo_rules: impossible min, max, forbidden",{
  testthat::expect_equal(length(apply_combo_rules(1:6, 4, rules=rules)),
               expected=0)})
