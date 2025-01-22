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
  expect_equal(
    names(suppressMessages(
      add_missing_parameters(imm(NA, NA, NA, NA), f)
    )),
    model_pars
  )

  f <- bmf(c ~ 1, s ~ 1, a ~ 1, kappa ~ 1)
  model_pars <- names(imm(NA, NA, NA, NA)$parameters)
  expect_equal(
    names(suppressMessages(add_missing_parameters(
      imm(NA, NA, NA, NA), f
    ))),
    model_pars
  )
})

test_that("check_formula gives expected errors", {
  expect_error(
    check_formula(sdm("dev_rad"),
      data = NULL,
      formula = brmsf <- brms::bf(dev_rad ~ 1, c ~ 1)
    ),
    "The provided formula is not a bmm formula"
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

test_that("rhs_vars works with bmmformulas", {
  f <- bmf(y ~ 1)
  expect_equal(rhs_vars(f), character(0))

  f <- bmf(y ~ a + b, x ~ c)
  expect_equal(rhs_vars(f), c("a", "b", "c"))

  f <- bmf(y ~ a + b + a:b, x ~ c)
  expect_equal(rhs_vars(f), c("a", "b", "c"))

  f <- bmf(y ~ a + b + a:b + (a | d), x ~ c)
  expect_equal(rhs_vars(f), c("a", "b", "d", "c"))

  f <- bmf(y ~ a + b + a:b + (a | d), x ~ c + d)
  expect_equal(rhs_vars(f), c("a", "b", "d", "c"))

  f <- bmf(y ~ a + b + a:b + (a | d), x ~ c + d, d ~ m)
  expect_equal(rhs_vars(f), c("a", "b", "d", "c", "m"))

  # test with non-linear transformations
  f <- bmf(y ~ a + b + a:b + (a | d), x ~ c + d, d ~ exp(m + j))
  expect_equal(rhs_vars(f), c("a", "b", "d", "c", "m", "j"))
})

test_that("lhs_vars works with regular formula", {
  f1 <- formula(y ~ x)
  f2 <- formula(~x)
  expect_equal(lhs_vars(f1), "y")
  expect_equal(lhs_vars(f2), character(0))
})

test_that("lhs_vars works with bmmformulas", {
  f <- bmf(y ~ 1)
  expect_equal(lhs_vars(f), "y")

  f <- bmf(y ~ a + b, x ~ c)
  expect_equal(lhs_vars(f), c("y", "x"))

  f <- bmf(y ~ a + b + a:b, x = 3)
  expect_equal(lhs_vars(f), c("y", "x"))

  f <- bmf(y ~ a + b + a:b + (a | d), x ~ c, d = 3)
  expect_equal(lhs_vars(f), c("y", "x", "d"))

  f <- bmf(y ~ a + b + a:b + (a | d), x ~ c + d, d ~ exp(m + j), m ~ 1)
  expect_equal(lhs_vars(f), c("y", "x", "d", "m"))
})

test_that("lhs_vars works with brmsformulas", {
  bf <- brms::bf
  nlf <- brms::nlf
  lf <- brms::lf

  bf(y ~ 1) |> 
    lhs_vars() |> 
    expect_equal("mu")
  bf(y ~ a, a ~ 1, nl = TRUE) |> 
    lhs_vars() |> 
    expect_equal(c("mu","a"))
  bf(y ~ a, a ~ 1, sigma ~ b, nl = TRUE) |> 
    lhs_vars() |> 
    expect_equal(c("mu","sigma", "a"))
})

test_that("assign_nl_attr works", {
  x <- bmf(y ~ c, c ~ a + b, a ~ d, m ~ 1)
  x <- assign_nl_attr(x)
  types <- is_nl(x)
  expect_equal(types, c(y = TRUE, c = TRUE, a = FALSE, m = FALSE))

  x <- bmf(y ~ 1)
  x <- assign_nl_attr(x)
  types <- is_nl(x)
  expect_equal(types, c(y = FALSE))

  f1 <- bmf(y ~ a)
  f2 <- bmf(a ~ 1)
  f3 <- f1 + f2
  f4 <- bmf(y ~ a, a ~ 1)
  expect_equal(f3, f4)
  types3 <- is_nl(f3)
  expect_equal(types3, c(y = TRUE, a = FALSE))
})

test_that("print.bmmformula works", {
  res <- utils::capture.output(bmf(a ~ 1, b = 2))
  expect_equal(res, c("a ~ 1", "b = 2"))
})
