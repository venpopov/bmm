test_that('Available mock models run without errors',{
  skip_on_cran()
  dat <- gen_imm_data(parms = data.frame(c=2,a=0.5,n=0,s=2,kappa=5),
                          ntrial = 100, setsize = 5)

  # two-parameter model mock fit
  f <- brms::bf(respErr ~ 1, kappa ~ 1, thetat ~ 1)
  mock_fit <- fit_model(f, dat, "2p", backend="mock", mock_fit=1, rename=FALSE)
  expect_equal(mock_fit$fit, 1)
  expect_type(mock_fit$fit_args, "list")
  expect_equal(names(mock_fit$fit_args[1:4]), c("formula", "data", "family", "prior"))


  # three-parameter model mock fit
  f <- brms::bf(respErr ~ 1, kappa ~ 1, thetat ~ 1, thetant ~ 1)
  mock_fit <- fit_model(f, dat, "3p", backend="mock", mock_fit=1, rename=FALSE,
                        setsize=5, non_targets = paste0('Item',2:5,'_rel'))
  expect_equal(mock_fit$fit, 1)
  expect_type(mock_fit$fit_args, "list")
  expect_equal(names(mock_fit$fit_args[1:4]), c("formula", "data", "family", "prior"))

  # IMMabc model mock fit
  f <- brms::bf(respErr ~ 1, kappa ~ 1, c ~ 1, a ~ 1)
  mock_fit <- fit_model(f, dat, "IMMabc", backend="mock", mock_fit=1, rename=FALSE,
                        setsize=5, non_targets = paste0('Item',2:5,'_rel'))
  expect_equal(mock_fit$fit, 1)
  expect_type(mock_fit$fit_args, "list")
  expect_equal(names(mock_fit$fit_args[1:4]), c("formula", "data", "family", "prior"))

  # IMMbsc model mock fit
  f <- brms::bf(respErr ~ 1, kappa ~ 1, c ~ 1, s ~ 1)
  mock_fit <- fit_model(f, dat, "IMMbsc", backend="mock", mock_fit=1, rename=FALSE,
                        setsize=5, non_targets = paste0('Item',2:5,'_rel'), spaPos=paste0('spaD',2:5))
  expect_equal(mock_fit$fit, 1)
  expect_type(mock_fit$fit_args, "list")
  expect_equal(names(mock_fit$fit_args[1:4]), c("formula", "data", "family", "prior"))

  # IMMbsc model mock fit
  f <- brms::bf(respErr ~ 1, kappa ~ 1, c ~ 1, a ~ 1, s ~ 1)
  mock_fit <- fit_model(f, dat, "IMMfull", backend="mock", mock_fit=1, rename=FALSE,
                        setsize=5, non_targets = paste0('Item',2:5,'_rel'), spaPos=paste0('spaD',2:5))
  expect_equal(mock_fit$fit, 1)
  expect_type(mock_fit$fit_args, "list")
  expect_equal(names(mock_fit$fit_args[1:4]), c("formula", "data", "family", "prior"))
})

test_that('Available models produce expected errors', {
  skip_on_cran()
  dat <- gen_imm_data(parms = data.frame(c=2,a=0.5,n=0,s=2,kappa=5),
                      ntrial = 100, setsize = 5)

  # Missing data
  okmodels <- supported_models()
  for (model in okmodels) {
    expect_error(fit_model(brms::bf(y~1), model=model, backend="mock", mock_fit=1, rename=FALSE),
                 "Data must be specified using the 'data' argument.")
  }


  okmodels <- c('3p','IMMabc','IMMbsc','IMMfull')
  for (model in okmodels) {
    expect_error(fit_model(brms::bf(respErr~1), data=dat, model=model, backend="mock",
                           mock_fit=1, rename=FALSE),
                 "Argument 'non_targets' must be specified.")
    expect_error(fit_model(brms::bf(respErr~1), data=dat, model=model, backend="mock",
                           mock_fit=1, rename=FALSE, non_targets='Item2_rel'),
                 "Argument 'setsize' is not specified.")
    expect_error(fit_model(brms::bf(respErr~1), data=dat, model=model, backend="mock",
                           mock_fit=1, rename=FALSE, non_targets='Item2_rel', setsize=5),
                 "'non_targets' is less than max\\(setsize\\)-1")
    expect_error(fit_model(brms::bf(respErr~1), data=dat, model=model, backend="mock",
                           mock_fit=1, rename=FALSE, non_targets='Item2_rel', setsize=TRUE),
                 "'setsize' must be either a single numeric value or a character string")
  }
})


