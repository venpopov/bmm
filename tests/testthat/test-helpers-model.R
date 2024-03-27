test_that("supported_models() returns a non-empty character vector", {
  expect_type(supported_models(print_call = FALSE), "character")
  expect_gt(length(supported_models(print_call = FALSE)), 0)
})

test_that("get_model() returns the correct function", {
  expect_equal(get_model("mixture2p"), .model_mixture2p)
})

test_that("check_model() refuses invalid models and accepts valid models", {
  expect_error(check_model("invalid_model"))
  expect_error(check_model(structure(list(), class = "invalid")))
  expect_error(check_model(sdm), "Did you forget")
  okmodels <- supported_models(print_call = FALSE)
  for (model in okmodels) {
    model <- get_model(model)()
    expect_silent(check_model(model))
    expect_type(check_model(model), "list")
  }
})

test_that("check_model() works with regular expressions", {
  dat <- oberauer_lin_2017
  models1 <- list(
    mixture3p("dev_rad",
      nt_features = paste0("col_nt", 1:7),
      set_size = "set_size"
    ),
    imm("dev_rad",
      nt_features = paste0("col_nt", 1:7),
      nt_distances = paste0("dist_nt", 1:7),
      set_size = "set_size"
    ),
    imm("dev_rad",
      nt_features = paste0("col_nt", 1:7),
      nt_distances = paste0("dist_nt", 1:7),
      set_size = "set_size",
      version = "bsc"
    ),
    imm("dev_rad",
      nt_features = paste0("col_nt", 1:7),
      set_size = "set_size",
      version = "abc"
    )
  )
  models2 <- list(
    mixture3p("dev_rad",
      nt_features = "col_nt",
      set_size = "set_size",
      regex = TRUE
    ),
    imm("dev_rad",
      nt_features = "col_nt",
      nt_distances = "dist_nt",
      set_size = "set_size",
      regex = TRUE
    ),
    imm("dev_rad",
      nt_features = "col_nt",
      nt_distances = "dist_nt",
      set_size = "set_size",
      regex = TRUE,
      version = "bsc"
    ),
    imm("dev_rad",
      nt_features = "col_nt",
      set_size = "set_size",
      regex = TRUE,
      version = "abc"
    )
  )

  for (i in 1:length(models1)) {
    check1 <- check_model(models1[[i]], dat)
    check2 <- check_model(models2[[i]], dat)
    attributes(check1) <- NULL
    attributes(check2) <- NULL
    expect_equal(check1, check2)
  }
})

test_that("use_model_template() prevents duplicate models", {
  skip_on_cran()
  okmodels <- supported_models(print_call = FALSE)
  for (model in okmodels) {
    expect_error(use_model_template(model))
  }

  model_files <- list.files(path = "R/", pattern = "^model_.*\\.R$")
  model_files_names <- gsub("^model_", "", model_files)
  model_files_names <- gsub("\\.R$", "", model_files_names)
  for (model in model_files_names) {
    expect_error(use_model_template(model))
  }
})

test_that("stancode() works with brmsformula", {
  ff <- brms::bf(count ~ zAge + zBase * Trt + (1 | patient))
  sd <- stancode(ff, data = brms::epilepsy, family = poisson())
  expect_equal(class(sd)[1], "character")
})

test_that("stancode() works with formula", {
  ff <- count ~ zAge + zBase * Trt + (1 | patient)
  sd <- stancode(ff, data = brms::epilepsy, family = poisson())
  expect_equal(class(sd)[1], "character")
})

test_that("stancode() works with bmmformula", {
  ff <- bmmformula(kappa ~ 1, thetat ~ 1, thetant ~ 1)
  sc <- stancode(ff, oberauer_lin_2017, model = mixture3p(resp_error = "dev_rad",
                                                         nt_features = "col_nt",
                                                         set_size = "set_size",
                                                         regex = T)
  )
  expect_equal(class(sc)[1], "character")
})

test_that("no check for with stancode function", {
  withr::local_options('bmm.sort_data' = 'check')
  expect_no_message(stancode(bmf(kappa ~ set_size, c ~ set_size),
                             oberauer_lin_2017,
                             sdm('dev_rad')))
})

test_that("change_constants() works", {
  model <- sdm(resp_error = "y")
  formula <- bmf(mu ~ set_size, kappa = 3, c ~ 1)
  model <- change_constants(model, formula)
})
