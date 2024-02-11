test_that("check_data() produces expected errors and warnings", {
  expect_error(check_data(.model_mixture2p()),
               "Data must be specified using the 'data' argument.")
  expect_error(check_data(.model_mixture2p(), data.frame(), brms::bf(y ~ 1)),
               "Argument 'data' does not contain observations.")
  expect_error(check_data(.model_mixture2p(), data.frame(x = 1), brms::bf(y ~ 1)),
               "The response variable 'y' is not present in the data.")

  mls <- lapply(c('mixture2p','mixture3p','IMMabc','IMMbsc','IMMfull'), get_model2)
  for (ml in mls) {
    expect_warning(check_data(ml(non_targets = 'x', setsize=2, spaPos = 'z'),
                              data.frame(y = 12, x = 1, z = 2),
                              brms::bf(y ~ 1)),
                   "It appears your response variable is in degrees.\n")
    expect_silent(check_data(ml(non_targets = 'x', setsize=2, spaPos = 'z'),
                             data.frame(y = 1, x = 1, z = 2), brms::bf(y ~ 1)))
  }

  mls <- lapply(c('mixture3p','IMMabc','IMMbsc','IMMfull'), get_model2)
  for (ml in mls) {
    expect_error(check_data(ml(non_targets = 'x', spaPos = 'z'), data.frame(y = 1, x = 1, z = 2), brms::bf(y ~ 1)),
                 'argument "setsize" is missing, with no default')
    expect_error(check_data(ml(setsize = 'x', spaPos = 'z'), data.frame(y = 1, x = 1, z = 2), brms::bf(y ~ 1)),
                 'argument "non_targets" is missing, with no default')
    expect_error(check_data(ml(non_targets='x', setsize = TRUE, spaPos = 'z'), data.frame(y = 1, x = 1, z = 2), brms::bf(y ~ 1)),
                 "Argument 'setsize' must be either a single numeric value or a character string.")
    expect_error(check_data(ml(non_targets='x', setsize = c(1,2,3), spaPos = 'z'), data.frame(y = 1, x = 1, z = 2), brms::bf(y ~ 1)),
                 "Argument 'setsize' must be either a single numeric value or a character string.")
    expect_error(check_data(ml(non_targets='x', setsize = 5, spaPos = 'z'), data.frame(y = 1, x = 1, z = 2), brms::bf(y ~ 1)),
                 "'non_targets' is less than max\\(setsize\\)-1")
    }

  mls <- lapply(c('IMMbsc','IMMfull'), get_model2)
  for (ml in mls) {
    expect_error(check_data(ml(non_targets=paste0('x',1:4), setsize = 5, spaPos = 'z'), data.frame(y = 1, x1 = 1, x2=2,x3=3,x4=4, z = 2), brms::bf(y ~ 1)),
                 "'spaPos' is less than max\\(setsize\\)-1")
  }
})

test_that("check_data() returns a data.frame()", {
  mls <- lapply(supported_models(print_call=FALSE), get_model)
  for (ml in mls) {
    expect_s3_class(check_data(ml(non_targets = 'x', setsize=2, spaPos = 'z'), data.frame(y = 1, x = 1, z = 2), brms::bf(y ~ 1)), "data.frame")
  }
})



test_that("wrap(x) returns the same for values between -pi and pi", {
  x <- runif(100, -pi, pi)
  expect_equal(wrap(x), x)
})

test_that("wrap(x) returns the correct value for values between (pi, 2*pi)", {
  x <- pi+1
  expect_equal(wrap(x), -(pi-1))
})


test_that("wrap(x) returns the correct value for values between (-2*pi, -pi)", {
  x <- -pi-1
  expect_equal(wrap(x), pi-1)
})

test_that("wrap(x) returns the correct value for values over 2*pi", {
  x <- 2*pi+1
  expect_equal(wrap(x), 1)
})

test_that("wrap(x) returns the correct value for values between (3*pi,4*pi)", {
  x <- 3*pi+1
  expect_equal(wrap(x), -(pi-1))
})


test_that("deg2rad returns the correct values for 0, 180, 360", {
  x <- c(0,90,180)
  expect_equal(round(deg2rad(x),2),c(0.00,1.57,3.14))
})

test_that("rad2deg returns the correct values for 0, pi/2, 2*pi", {
  x <- c(0, pi/2, 2*pi)
  expect_equal(round(rad2deg(x),2),c(0,90,360))
})


test_that("get_standata() returns a string", {
  # define formula
  ff <- brms::bf(y ~ 1,
                 kappa ~ 1,
                 thetat ~ 1,
                 thetant ~ 1)

  # simulate data
  dat <- data.frame(y = rmixture3p(n = 3),
                    nt1_loc = 2,
                    nt2_loc = -1.5)

  # fit the model
  standata <- get_standata(formula = ff,
                           data = dat,
                           model = mixture3p(non_targets = paste0('nt',1,'_loc'), setsize = 2))
  expect_equal(class(standata)[1], "standata")
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
