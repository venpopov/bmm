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

    expect_error(check_data(ml(non_targets='x', setsize = 5, spaPos = 'z'), data.frame(y = 1, x = 1, z = 2), brms::bf(y ~ 1)),
                 "'non_targets' is less than max\\(setsize\\)-1")
    }

  mls <- lapply(c('IMMbsc','IMMfull'), get_model2)
  for (ml in mls) {
    expect_error(check_data(ml(non_targets=paste0('x',1:4), setsize = 5, spaPos = 'z'), data.frame(y = 1, x1 = 1, x2=2,x3=3,x4=4, z = 2), brms::bf(y ~ 1)),
                 "'spaPos' is less than max\\(setsize\\)-1")
  }
})



test_that("check_var_setsize accepts valid input", {
  # Simple numeric vector is valid
  dat <- data.frame(y = rep(c(1,2,3), each=3))
  expect_silent(check_var_setsize('y', dat))
  expect_equal(names(check_var_setsize('y', dat)), c("max_setsize","ss_numeric"))
  expect_equal(check_var_setsize('y', dat)$max_setsize, 3)
  all(is.numeric(check_var_setsize('y', dat)$ss_numeric), na.rm = T)

  # Factor with numeric levels is valid
  dat <- data.frame(y = factor(rep(c(1,2,3), each=3)))
  expect_silent(check_var_setsize('y', dat))
  expect_equal(check_var_setsize('y', dat)$max_setsize, 3)
  all(is.numeric(check_var_setsize('y', dat)$ss_numeric), na.rm = T)


  # Character vector representing numbers is valid
  dat <- data.frame(y = rep(c('1','2','3'), each=3))
  expect_silent(check_var_setsize('y', dat))
  expect_equal(check_var_setsize('y', dat)$max_setsize, 3)
  all(is.numeric(check_var_setsize('y', dat)$ss_numeric), na.rm = T)


  # Numeric vector with NA values is valid (assuming NA is treated correctly)
  dat <- data.frame(y = rep(c(1,5,NA), each=3))
  expect_silent(check_var_setsize('y', dat))
  expect_equal(check_var_setsize('y', dat)$max_setsize, 5)
  all(is.numeric(check_var_setsize('y', dat)$ss_numeric), na.rm = T)


  # Factor with NA and numeric levels is valid
  dat <- data.frame(y = factor(rep(c(1,5,NA), each=3)))
  expect_silent(check_var_setsize('y', dat))
  expect_equal(check_var_setsize('y', dat)$max_setsize, 5)
  all(is.numeric(check_var_setsize('y', dat)$ss_numeric), na.rm = T)

})

test_that("check_var_setsize rejects invalid input", {
  # Factor with non-numeric levels is invalid
  dat <- data.frame(y = factor(rep(c('A','B','C'), each=3)))
  expect_error(check_var_setsize('y', dat), "must be coercible to a numeric vector")

  # Character vector with non-numeric values is invalid
  dat <- data.frame(y = rep(c('A','B','C'), each=3))
  expect_error(check_var_setsize('y', dat), "must be coercible to a numeric vector")

  # Character vector with NA and non-numeric values is invalid
  dat <- data.frame(y = rep(c('A',NA,'C'), each=3))
  expect_error(check_var_setsize('y', dat), "must be coercible to a numeric vector")

  # Factor with NA and non-numeric levels is invalid
  dat <- data.frame(y = factor(rep(c('A',NA,'C'), each=3)))
  expect_error(check_var_setsize('y', dat), "must be coercible to a numeric vector")

  # Character vector with numeric and non-numeric values is invalid
  dat <- data.frame(y = rep(c('A',5,'C'), each=3))
  expect_error(check_var_setsize('y', dat), "must be coercible to a numeric vector")

  # Factor with numeric and non-numeric levels is invalid
  dat <- data.frame(y = factor(rep(c('A',5,'C'), each=3)))
  expect_error(check_var_setsize('y', dat), "must be coercible to a numeric vector")

  # Numeric vector with invalid set sizes (less than 1) is invalid
  dat <- data.frame(y = rep(c(0,1,5), each=3))
  expect_error(check_var_setsize('y', dat), "must be greater than 0")

  # Factor with levels less than 1 are invalid
  dat <- data.frame(y = factor(rep(c(0,4,5), each=3)))
  expect_error(check_var_setsize('y', dat), "must be greater than 0")

  # Character vector representing set sizes with text is invalid
  dat <- data.frame(y = rep(paste0('setsize ', c(2,3,8)), each=3))
  expect_error(check_var_setsize('y', dat), "must be coercible to a numeric vector")

  # Factor representing set sizes with text is invalid
  dat <- data.frame(y = factor(rep(paste0('setsize ', c(2,3,8)), each=3)))
  expect_error(check_var_setsize('y', dat), "must be coercible to a numeric vector")

  # Numeric vector with decimals is invalid
  dat <- data.frame(y = c(1:8,1.3))
  expect_error(check_var_setsize('y', dat), "must be whole numbers")

  # Setsize must be of length 1
  dat <- data.frame(y = c(1,2,3), z = c(1,2,3))
  expect_error(check_var_setsize(c('z','y'), dat), "You provided a vector")
  expect_error(check_var_setsize(list('z','y'), dat), "You provided a vector")
  expect_error(check_var_setsize(setsize=TRUE, dat), "You provided a vector")
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
