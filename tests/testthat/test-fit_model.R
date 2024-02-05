test_that('Available mock models run without errors',{
  skip_on_cran()
  dat <- gen_imm_data(parms = data.frame(c=2,a=0.5,n=0,s=2,kappa=5),
                          ntrial = 2, setsize = 5)

  # two-parameter model mock fit
  f <- brms::bf(respErr ~ 1, kappa ~ 1, thetat ~ 1)
  mock_fit <- fit_model(f, dat, mixture2p(), backend="mock", mock_fit=1, rename=FALSE)
  expect_equal(mock_fit$fit, 1)
  expect_type(mock_fit$fit_args, "list")
  expect_equal(names(mock_fit$fit_args[1:4]), c("formula", "data", "family", "prior"))


  # three-parameter model mock fit
  f <- brms::bf(respErr ~ 1, kappa ~ 1, thetat ~ 1, thetant ~ 1)
  mock_fit <- fit_model(f, dat, mixture3p(setsize=5, non_targets = paste0('Item',2:5,'_rel')),
                        backend="mock", mock_fit=1, rename=FALSE)
  expect_equal(mock_fit$fit, 1)
  expect_type(mock_fit$fit_args, "list")
  expect_equal(names(mock_fit$fit_args[1:4]), c("formula", "data", "family", "prior"))

  # IMMabc model mock fit
  f <- brms::bf(respErr ~ 1, kappa ~ 1, c ~ 1, a ~ 1)
  mock_fit <- fit_model(f, dat, IMMabc(setsize=5, non_targets = paste0('Item',2:5,'_rel')),
                        backend="mock", mock_fit=1, rename=FALSE)
  expect_equal(mock_fit$fit, 1)
  expect_type(mock_fit$fit_args, "list")
  expect_equal(names(mock_fit$fit_args[1:4]), c("formula", "data", "family", "prior"))

  # IMMbsc model mock fit
  f <- brms::bf(respErr ~ 1, kappa ~ 1, c ~ 1, s ~ 1)
  mock_fit <- fit_model(f, dat, IMMbsc(setsize=5, non_targets = paste0('Item',2:5,'_rel'), spaPos=paste0('spaD',2:5)),
                        backend="mock", mock_fit=1, rename=FALSE)
  expect_equal(mock_fit$fit, 1)
  expect_type(mock_fit$fit_args, "list")
  expect_equal(names(mock_fit$fit_args[1:4]), c("formula", "data", "family", "prior"))

  # IMMbsc model mock fit
  f <- brms::bf(respErr ~ 1, kappa ~ 1, c ~ 1, a ~ 1, s ~ 1)
  mock_fit <- fit_model(f, dat, IMMfull(setsize=5, non_targets = paste0('Item',2:5,'_rel'), spaPos=paste0('spaD',2:5)), backend="mock", mock_fit=1, rename=FALSE)
  expect_equal(mock_fit$fit, 1)
  expect_type(mock_fit$fit_args, "list")
  expect_equal(names(mock_fit$fit_args[1:4]), c("formula", "data", "family", "prior"))
})

test_that('Available models produce expected errors', {
  skip_on_cran()
  dat <- gen_imm_data(parms = data.frame(c=2,a=0.5,n=0,s=2,kappa=5),
                      ntrial = 100, setsize = 5)

  # Missing data
  okmodels <- supported_models(print_call=FALSE)
  for (model in okmodels) {
    model <- get_model2(model)
    args_list <- formals(model)
    test_args <- lapply(args_list, function(x) {NULL})
    model <- brms::do_call(model, test_args)
    expect_error(fit_model(brms::bf(y~1), model=model, backend="mock", mock_fit=1, rename=FALSE),
                 "Data must be specified using the 'data' argument.")
  }


  okmodels <- c('mixture3p','IMMabc','IMMbsc','IMMfull')
  for (model in okmodels) {
    model1 <- get_model2(model)(non_targets='Item2_rel', setsize=5, spaPos='spaD2')
    expect_error(fit_model(brms::bf(respErr~1), data=dat, model=model1, backend="mock",
                           mock_fit=1, rename=FALSE),
                 "'non_targets' is less than max\\(setsize\\)-1")
    model2 <- get_model2(model)(non_targets='Item2_rel', setsize=TRUE, spaPos='spaD2')
    expect_error(fit_model(brms::bf(respErr~1), data=dat, model=model2, backend="mock",
                           mock_fit=1, rename=FALSE),
                 "'setsize' must be either a single numeric value or a character string")
  }
})


