test_that('extract vars returns the correct variables with one formula', {
  formula <- brms::bf(y ~ A + B + C + (1|D) + (F*G|E))
  vars <- extract_vars(formula)
  vars <- sort(vars)
  expect_equal(vars, LETTERS[1:7])

  vars <- extract_vars(formula, include_resp = T)
  vars <- sort(vars)
  expect_equal(vars, c(LETTERS[1:7],'y'))
})

test_that('extract vars returns the correct variables with more than one formula', {
  formula <- brms::bf(y ~ A,
                      sigma ~ B + C + (1|D) + (F*G|E))
  vars <- extract_vars(formula)
  vars <- sort(vars)
  expect_equal(vars, LETTERS[1:7])

  vars <- extract_vars(formula, include_resp = T)
  vars <- sort(vars)
  expect_equal(vars, c(LETTERS[1:7],'y'))
})

test_that('extract vars returns the correct variables with different family', {
  formula <- brms::bf(y ~ A,
                      kappa ~ B + C + (1|D) + (F*G|E))
  vars <- extract_vars(formula, brms::von_mises())
  vars <- sort(vars)
  expect_equal(vars, LETTERS[1:7])

  vars <- extract_vars(formula, brms::von_mises(), include_resp = T)
  vars <- sort(vars)
  expect_equal(vars, c(LETTERS[1:7],'y'))
})


# first draft of tests was written by ChatGPT4
test_that('has_nonconsecutive_duplicates works', {
  # Test with a vector that has only consecutive duplicates
  expect_false(has_nonconsecutive_duplicates(c("a", "a", "b", "b", "c", "c")))

  # Test with a vector that has non-consecutive duplicates
  expect_true(has_nonconsecutive_duplicates(c("a", "b", "a", "c", "c", "b")))

  # Test with a single unique value repeated
  expect_false(has_nonconsecutive_duplicates(rep("a", 5)))

  # Test with a vector of all unique values
  expect_false(has_nonconsecutive_duplicates(letters[1:5]))

  # Test with a vector that has a mix of consecutive and non-consecutive duplicates
  expect_true(has_nonconsecutive_duplicates(c("a", "a", "b", "a", "b", "b")))

  # Test with a numeric vector with mixed values
  expect_true(has_nonconsecutive_duplicates(c(1, 2, 3, 1, 4, 2)))

  # Test with an empty vector
  expect_false(has_nonconsecutive_duplicates(numeric(0)))

  # Test with a vector that has only one element
  expect_false(has_nonconsecutive_duplicates(c("a")))

  # Test with a vector that has non-consecutive duplicates at the beginning and end
  expect_true(has_nonconsecutive_duplicates(c("a", "b", "b", "a")))

  # Test with a vector that includes NA values
  expect_false(has_nonconsecutive_duplicates(c(NA, NA, NA)))

  # Test with a vector that includes NA values among other values
  expect_false(has_nonconsecutive_duplicates(c(NA, 1, NA)))

  # Test with a complex vector including numbers, NA, and characters
  expect_true(has_nonconsecutive_duplicates(c(1, "a", 2, "b", 1, NA, "a")))

  # Test with a vector that changes type (numeric and character mixed)
  expect_true(has_nonconsecutive_duplicates(c("1", 2, "2", 1)))
})

test_that('is_data_ordered works', {
  # Test with a data frame that is ordered
  data1 <- expand.grid(y = 1:3, B = 1:3, C = 1:3)
  formula1 <- brms::bf(y ~ B + C)
  expect_true(is_data_ordered(data1, formula1))

  # Test with a data frame that is not ordered
  data2 <- rbind(data1, data1[1, ])
  expect_false(is_data_ordered(data2, formula1))

  # Test when irrelevant variables are not ordered but predictors are
  data3 <- data1
  data3$A <- c(3, 2, 1)
  data3$A <- sample(data3$A)
  formula2 <- brms::bf(y ~ A + B + C)
  expect_true(is_data_ordered(data3, formula1))
  expect_false(is_data_ordered(data3, formula2))
})
