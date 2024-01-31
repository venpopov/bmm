test_that("supported_models() returns a non-empty character vector", {
  expect_type(supported_models(), "character")
  expect_gt(length(supported_models()), 0)
})

test_that("get_model() returns the correct function", {
  expect_equal(get_model("2p"), .model_2p)
})

test_that("check_model() refuses invalid models and accepts valid models", {
  expect_error(check_model("invalid_model", NULL))
  expect_silent(check_model("2p",NULL))
  expect_type(check_model("2p",NULL), "character")
})



