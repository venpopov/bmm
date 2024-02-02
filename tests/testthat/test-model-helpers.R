test_that("supported_models() returns a non-empty character vector", {
  expect_type(supported_models(), "character")
  expect_gt(length(supported_models()), 0)
})

test_that("get_model() returns the correct function", {
  expect_equal(get_model("mixture2p"), .model_mixture2p)
})

test_that("check_model() refuses invalid models and accepts valid models", {
  expect_error(check_model("invalid_model", NULL))
  okmodels <- supported_models()
  for (model in okmodels) {
    expect_silent(check_model(model, NULL))
    expect_type(check_model(model,NULL), "list")
  }
})

test_that("use_model_template() prevents duplicate models", {
  skip_on_cran()
  okmodels <- supported_models()
  for (model in okmodels) {
    expect_error(use_model_template(model))
  }

  model_files <- list.files(path = "R/", pattern = "^bmm_model_.*\\.R$")
  model_files_names <- gsub("^bmm_model_", "", model_files)
  model_files_names <- gsub("\\.R$", "", model_files_names)
  for (model in model_files_names) {
    expect_error(use_model_template(model))
  }
})
