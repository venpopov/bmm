test_that('Available mock models run without errors',{
  withr::local_options('bmm.silent'=2)
  skip_on_cran()
  dat <- data.frame(
    resp_err = rIMM(n = 5),
    Item2_rel = 2,
    Item3_rel = -1.5,
    spaD2 = 0.5,
    spaD3 = 2
  )

  # two-parameter model mock fit
  f <- bmmformula(kappa ~ 1, thetat ~ 1)
  mock_fit <- fit_model(f, dat, mixture2p(resp_err =  "resp_err"), backend="mock", mock_fit=1, rename=FALSE)
  expect_equal(mock_fit$fit, 1)
  expect_type(mock_fit$fit_args, "list")
  expect_equal(names(mock_fit$fit_args[1:4]), c("formula", "data", "family", "prior"))

  # three-parameter model mock fit
  f <- bmmformula(kappa ~ 1, thetat ~ 1, thetant ~ 1)
  mock_fit <- fit_model(f, dat, mixture3p(resp_err = "resp_err", setsize = 3,
                                          non_targets = paste0('Item',2:3,'_rel')),
                        backend="mock", mock_fit=1, rename=FALSE)
  expect_equal(mock_fit$fit, 1)
  expect_type(mock_fit$fit_args, "list")
  expect_equal(names(mock_fit$fit_args[1:4]), c("formula", "data", "family", "prior"))

  # IMMabc model mock fit
  f <- bmmformula(kappa ~ 1, c ~ 1, a ~ 1)
  mock_fit <- fit_model(f, dat, IMMabc(resp_err = "resp_err", setsize =3,
                                       non_targets = paste0('Item',2:3,'_rel')),
                        backend="mock", mock_fit=1, rename=FALSE)
  expect_equal(mock_fit$fit, 1)
  expect_type(mock_fit$fit_args, "list")
  expect_equal(names(mock_fit$fit_args[1:4]), c("formula", "data", "family", "prior"))

  # IMMbsc model mock fit
  f <- bmmformula(kappa ~ 1, c ~ 1, s ~ 1)
  mock_fit <- fit_model(f, dat, IMMbsc(resp_err = "resp_err", setsize=3, non_targets = paste0('Item',2:3,'_rel'), spaPos=paste0('spaD',2:3)),
                        backend="mock", mock_fit=1, rename=FALSE)
  expect_equal(mock_fit$fit, 1)
  expect_type(mock_fit$fit_args, "list")
  expect_equal(names(mock_fit$fit_args[1:4]), c("formula", "data", "family", "prior"))

  # IMMbsc model mock fit
  f <- bmmformula(kappa ~ 1, c ~ 1, a ~ 1, s ~ 1)
  mock_fit <- fit_model(f, dat, IMMfull(resp_err = "resp_err", setsize=3, non_targets = paste0('Item',2:3,'_rel'), spaPos=paste0('spaD',2:3)), backend="mock", mock_fit=1, rename=FALSE)
  expect_equal(mock_fit$fit, 1)
  expect_type(mock_fit$fit_args, "list")
  expect_equal(names(mock_fit$fit_args[1:4]), c("formula", "data", "family", "prior"))
})

test_that('Available models produce expected errors', {
  withr::local_options('bmm.silent'=2)
  skip_on_cran()
  dat <- data.frame(
    resp_err = rIMM(n = 5),
    Item2_rel = 2,
    Item3_rel = -1.5,
    spaD2 = -0.5,
    spaD3 = 2
  )

  # Missing data
  okmodels <- supported_models(print_call=FALSE)
  for (model in okmodels) {
    model <- get_model2(model)
    args_list <- formals(model)
    test_args <- lapply(args_list, function(x) {NULL})
    model <- brms::do_call(model, test_args)
    expect_error(fit_model(bmmformula(kappa~1), model=model, backend="mock", mock_fit=1, rename=FALSE),
                 "Data must be specified using the 'data' argument.")
  }


  okmodels <- c('mixture3p','IMMabc','IMMbsc','IMMfull')
  for (model in okmodels) {
    model1 <- get_model2(model)(resp_err = "resp_err", non_targets='Item2_rel', setsize=5, spaPos='spaD2')
    expect_error(fit_model(bmmformula(kappa~1), data=dat, model=model1, backend="mock",
                           mock_fit=1, rename=FALSE),
                 "'non_targets' should equal max\\(setsize\\)-1")

    model2 <- get_model2(model)(resp_err = "resp_err", non_targets='Item2_rel', setsize=TRUE, spaPos='spaD2')
    expect_error(fit_model(bmmformula(kappa~1), data=dat, model=model2, backend="mock",
                           mock_fit=1, rename=FALSE),
                 "must be either a variable in your data or ")
  }

  spamodels <- c('IMMbsc','IMMfull')
  for(model in spamodels){
    model1 <- get_model2(model)(resp_err = "resp_err", non_targets= paste0("Item",2:3,"_rel"), setsize=3, spaPos=paste0("spaD",2:3))
    expect_error(fit_model(bmmformula(kappa~1), data=dat, model=model1, backend="mock",
                           mock_fit=1, rename=FALSE),
                 "All spatial distances to the target need to be postive.")
  }
})

