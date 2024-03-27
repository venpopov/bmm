test_that("wrong_parameters works", {
  f <- bmf(c ~ 1, a ~ 1, s ~ 1, kappa ~ 1)
  expect_equal(length(wrong_parameters(imm(NA, NA, NA, NA), f)), 0)
  expect_equal(wrong_parameters(imm(NA, NA, NA, NA, version = "bsc"), f), "a")
  expect_equal(wrong_parameters(sdm(NA), f), c("a", "s"))
})

test_that("wrong_parameters doesnt reject non-linear transformations", {
  f <- bmf(c ~ 1, a ~ 1, s ~ 1, kappa ~ exp(logkappa), logkappa ~ 1)
  expect_equal(length(wrong_parameters(imm(NA, NA, NA, NA), f)), 0)
  expect_equal(wrong_parameters(imm(NA, NA, NA, NA, version = "bsc"), f), "a")
  expect_equal(wrong_parameters(sdm(NA), f), c("a", "s"))
})


test_that("add_missing_parameters works", {
  f <- bmf(c ~ 1)
  model_pars <- names(imm(NA, NA, NA, NA)$parameters)
  expect_equal(names(suppressMessages(
    add_missing_parameters(imm(NA, NA, NA, NA), f))),
    model_pars)

  f <- bmf(c ~ 1, s ~ 1, a ~ 1, kappa ~ 1)
  model_pars <- names(imm(NA, NA, NA, NA)$parameters)
  expect_equal(names(suppressMessages(add_missing_parameters(
    imm(NA, NA, NA, NA), f))),
    model_pars)
})


test_that("check_formula gives expected errors", {
  expect_error(
    check_formula(sdm("dev_rad"),
      data = NULL,
      formula = brmsf <- brms::bf(dev_rad ~ 1, c ~ 1)
    ),
    "The provided formula is a brms formula."
  )

  expect_error(
    check_formula(sdm("dev_rad"),
      data = NULL,
      formula = bmf(c ~ 1, kappa1 ~ 1)
    ),
    "Unrecognized model parameters"
  )
})

test_that("check_formula works", {
  expect_equal(
    names(check_formula(sdm("dev_rad"),
      data = NULL,
      formula = bmf(c ~ 1, kappa ~ 1)
    )),
    c("mu", "c", "kappa")
  )
  expect_equal(
    names(check_formula(sdm("dev_rad"),
      data = NULL,
      formula = bmf(c ~ 1)
    )),
    c("mu", "c", "kappa")
  )
})


test_that("has_intercept works", {
  expect_true(has_intercept(y ~ 1))
  expect_true(has_intercept(y ~ A))
  expect_true(has_intercept(y ~ A + B))
  expect_true(has_intercept(y ~ A * B))
  expect_true(has_intercept(y ~ 1 + A))
  expect_true(has_intercept(y ~ A + (A | ID)))

  expect_false(has_intercept(y ~ 0 + A))
  expect_false(has_intercept(y ~ 0 + A + B))
  expect_false(has_intercept(y ~ 0 + A * B))
  expect_false(has_intercept(y ~ 0 + A + (A | ID)))
})
