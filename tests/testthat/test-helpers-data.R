test_that("check_data() produces expected errors and warnings", {
  expect_error(check_data(.model_mixture2p(resp_err = "y")),
               "Data must be specified using the 'data' argument.")
  expect_error(check_data(.model_mixture2p(resp_err = "y"),
                          data.frame(),
                          bmmformula(kappa ~ 1)),
               "Argument 'data' does not contain observations.")
  expect_error(check_data(.model_mixture2p(resp_err = "y"),
                          data.frame(x = 1),
                          bmmformula(kappa ~ 1)),
               "The response variable 'y' is not present in the data.")
  expect_error(check_data(.model_mixture2p(resp_err = "y"),
                          y~1),
               "Argument 'data' must be coercible to a data.frame.")

  mls <- lapply(c('mixture2p','mixture3p','IMMabc','IMMbsc','IMMfull'), get_model2)
  for (ml in mls) {
    expect_warning(check_data(ml(resp_err = "y", non_targets = 'x', setsize=2, spaPos = 'z'),
                              data.frame(y = 12, x = 1, z = 2),
                              bmmformula(kappa ~ 1)),
                   "It appears your response variable is in degrees.\n")
    expect_silent(check_data(ml(resp_err = "y", non_targets = 'x', setsize=2, spaPos = 'z'),
                             data.frame(y = 1, x = 1, z = 2), brms::bf(y ~ 1)))
  }

  mls <- lapply(c('mixture3p','IMMabc','IMMbsc','IMMfull'), get_model2)
  for (ml in mls) {
    expect_error(check_data(ml(resp_err = "y", non_targets = 'x', spaPos = 'z'),
                            data.frame(y = 1, x = 1, z = 2),
                            bmmformula(kappa ~ 1)),
                 'argument "setsize" is missing, with no default')
    expect_error(check_data(ml(resp_err = "y",setsize = 'x', spaPos = 'z'),
                            data.frame(y = 1, x = 1, z = 2),
                            bmmformula(kappa ~ 1)),
                 'argument "non_targets" is missing, with no default')
    expect_error(check_data(ml(resp_err = "y",non_targets='x', setsize = TRUE, spaPos = 'z'),
                            data.frame(y = 1, x = 1, z = 2),
                            bmmformula(kappa ~ 1)),
                 "Argument 'setsize' must be either a single numeric value or a character string.")
    expect_error(check_data(ml(resp_err = "y",non_targets='x', setsize = c(1,2,3), spaPos = 'z'),
                            data.frame(y = 1, x = 1, z = 2),
                            bmmformula(kappa ~ 1)),
                 "Argument 'setsize' must be either a single numeric value or a character string.")
    expect_error(check_data(ml(resp_err = "y",non_targets='x', setsize = 5, spaPos = 'z'),
                            data.frame(y = 1, x = 1, z = 2),
                            bmmformula(kappa ~ 1)),
                 "'non_targets' is less than max\\(setsize\\)-1")
    expect_warning(check_data(ml(resp_err = "y", non_targets = 'x', setsize=2, spaPos = 'z'),
                              data.frame(y = 1, x = 2*pi+1, z = 2),
                              bmmformula(kappa ~ 1)),
                   "at least one of your non_target variables are in degrees")
  }

  mls <- lapply(c('IMMbsc','IMMfull'), get_model2)
  for (ml in mls) {
    expect_error(check_data(ml(resp_err = "y",non_targets=paste0('x',1:4), setsize = 5, spaPos = 'z'),
                            data.frame(y = 1, x1 = 1, x2=2,x3=3,x4=4, z = 2),
                            bmmformula(kappa ~ 1)),
                 "'spaPos' is less than max\\(setsize\\)-1")
  }
})

test_that("check_data() returns a data.frame()", {
  mls <- lapply(supported_models(print_call=FALSE), get_model)
  for (ml in mls) {
    expect_s3_class(check_data(ml(resp_err = "y",non_targets = 'x', setsize=2, spaPos = 'z'),
                               data.frame(y = 1, x = 1, z = 2),
                               bmmformula(kappa ~ 1)), "data.frame")
  }
})



test_that("wrap(x) returns the same for values between -pi and pi", {
  x <- runif(100, -pi, pi)
  expect_equal(wrap(x), x)
  expect_equal(wrap(rad2deg(x), radians = F), rad2deg(wrap(x)))
})

test_that("wrap(x) returns the correct value for values between (pi, 2*pi)", {
  x <- pi+1
  expect_equal(wrap(x), -(pi-1))
  expect_equal(wrap(rad2deg(x), radians = F), rad2deg(wrap(x)))
})


test_that("wrap(x) returns the correct value for values between (-2*pi, -pi)", {
  x <- -pi-1
  expect_equal(wrap(x), pi-1)
  expect_equal(wrap(rad2deg(x), radians = F), rad2deg(wrap(x)))
})

test_that("wrap(x) returns the correct value for values over 2*pi", {
  x <- 2*pi+1
  expect_equal(wrap(x), 1)
  expect_equal(wrap(rad2deg(x), radians = F), rad2deg(wrap(x)))
})

test_that("wrap(x) returns the correct value for values between (3*pi,4*pi)", {
  x <- 3*pi+1
  expect_equal(wrap(x), -(pi-1))
  expect_equal(wrap(rad2deg(x), radians = F), rad2deg(wrap(x)))
})


test_that("deg2rad returns the correct values for 0, 180, 360", {
  x <- c(0,90,180)
  expect_equal(round(deg2rad(x),2),c(0.00,1.57,3.14))
  expect_equal(wrap(rad2deg(x), radians = F), rad2deg(wrap(x)))
})

test_that("rad2deg returns the correct values for 0, pi/2, 2*pi", {
  x <- c(0, pi/2, 2*pi)
  expect_equal(round(rad2deg(x),2),c(0,90,360))
  expect_equal(wrap(rad2deg(x), radians = F), rad2deg(wrap(x)))
})


test_that("get_standata() returns a standata class", {
  ff <- bmmformula(kappa ~ 1,
                   thetat ~ 1,
                   thetant ~ 1)

  dat <- data.frame(y = rmixture3p(n = 3),
                    nt1_loc = 2,
                    nt2_loc = -1.5)

  standata <- get_standata(formula = ff,
                           data = dat,
                           model = mixture3p(resp_err = "y" ,
                                             non_targets = paste0('nt',1,'_loc'),
                                             setsize = 2))
  expect_equal(class(standata)[1], "standata")
})

