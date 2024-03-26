test_that("Available mock models run without errors", {
  withr::local_options("bmm.silent" = 2)
  skip_on_cran()
  dat <- data.frame(
    resp_error = rimm(n = 5),
    Item2_rel = 2,
    Item3_rel = -1.5,
    spaD2 = 0.5,
    spaD3 = 2
  )

  # two-parameter model mock fit
  f <- bmf(kappa ~ 1, thetat ~ 1)
  mock_fit <- bmm(f, dat, mixture2p(resp_error = "resp_error"),
                  backend = "mock", mock_fit = 1, rename = FALSE)
  expect_equal(mock_fit$fit, 1)
  expect_type(mock_fit$bmm, "list")

  # three-parameter model mock fit
  f <- bmf(kappa ~ 1, thetat ~ 1, thetant ~ 1)
  model <- mixture3p(resp_error = "resp_error", set_size = 3,
                     nt_features = paste0("Item", 2:3, "_rel"))
  mock_fit <- bmm(f, dat, model, backend = "mock", mock_fit = 1, rename = FALSE)
  expect_equal(mock_fit$fit, 1)
  expect_type(mock_fit$bmm, "list")

  # imm_abc model mock fit
  f <- bmf(kappa ~ 1, c ~ 1, a ~ 1)
  model <- imm(resp_error = "resp_error", set_size = 3,
               nt_features = paste0("Item", 2:3, "_rel"),
               version = "abc")
  mock_fit <- bmm(f, dat, model, backend = "mock", mock_fit = 1, rename = FALSE)
  expect_equal(mock_fit$fit, 1)
  expect_type(mock_fit$bmm, "list")

  # imm_bsc model mock fit
  f <- bmf(kappa ~ 1, c ~ 1, s ~ 1)
  model <- imm(resp_error = "resp_error", set_size = 3,
               nt_features = paste0("Item", 2:3, "_rel"),
               nt_distances = paste0("spaD", 2:3),
               version = "bsc")
  mock_fit <- bmm(f, dat, model, backend = "mock", mock_fit = 1, rename = FALSE)
  expect_equal(mock_fit$fit, 1)
  expect_type(mock_fit$bmm, "list")

  # imm_full model mock fit
  f <- bmf(kappa ~ 1, c ~ 1, a ~ 1, s ~ 1)
  model <- imm(resp_error = "resp_error", set_size = 3,
               nt_features = paste0("Item", 2:3, "_rel"),
               nt_distances = paste0("spaD", 2:3))
  mock_fit <- bmm(f, dat, model, backend = "mock", mock_fit = 1, rename = FALSE)
  expect_equal(mock_fit$fit, 1)
  expect_type(mock_fit$bmm, "list")
})

test_that("Available models produce expected errors", {
  withr::local_options("bmm.silent" = 2)
  skip_on_cran()
  dat <- data.frame(
    resp_error = rimm(n = 5),
    Item2_rel = 2,
    Item3_rel = -1.5,
    spaD2 = -0.5,
    spaD3 = 2
  )

  # Missing data
  okmodels <- supported_models(print_call = FALSE)
  for (model in okmodels) {
    model <- get_model(model)
    expect_error(
      bmm(bmf(kappa ~ 1), model = model(), backend = "mock",
          mock_fit = 1, rename = FALSE),
      "argument \"data\" is missing, with no default"
    )
  }


  okmodels <- c("mixture3p", "imm")
  for (model in okmodels) {
    model1 <- get_model(model)(resp_error = "resp_error",
                               nt_features = "Item2_rel",
                               set_size = 5,
                               nt_distances = "spaD2")
    expect_error(
      bmm(bmf(kappa ~ 1), dat, model1, backend = "mock",
          mock_fit = 1, rename = FALSE),
      "'nt_features' should equal max\\(set_size\\)-1"
    )

    model2 <- get_model(model)(resp_error = "resp_error",
                               nt_features = "Item2_rel",
                               set_size = TRUE,
                               nt_distances = "spaD2")
    expect_error(
      bmm(bmf(kappa ~ 1), dat, model2, backend = "mock",
          mock_fit = 1, rename = FALSE),
      "must be either a variable in your data or "
    )
  }

  for (version in c('bsc', 'full')) {
    model1 <- imm(resp_error = "resp_error",
                  nt_features = paste0("Item", 2:3, "_rel"),
                  set_size = 3,
                  nt_distances = paste0("spaD", 2:3),
                  version = version)
    expect_error(
      bmm(bmf(kappa ~ 1), dat, model1, backend = "mock",
          mock_fit = 1, rename = FALSE),
      "All non-target distances to the target need to be postive."
    )
  }
})
