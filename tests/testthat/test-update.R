test_that('update.bmmfit works', {
  skip_on_cran()
  fit1 <- bmmfit_example1
  data <- fit1$bmm$fit_args$data

  # formula is replaced
  up <- update(fit1, formula. = bmf(c ~ 1, kappa ~ 1), testmode = TRUE)
  expect_true(is(up, "bmmfit"))
  expect_equal(up$bmm$fit_args$formula$pforms$c, c ~ 1)

  # data is replaced, old formula is kept
  new_data <- data
  new_data$dev_rad <- rnorm(nrow(new_data),0,0.5)
  up <- update(fit1, newdata = new_data, save_pars = save_pars(group = FALSE),
               testmode = TRUE)
  expect_true(is(up, "bmmfit"))
  expect_equal(attr(up$data, "data_name"), "new_data")
  expect_equal(up$data$dev_rad, new_data$dev_rad)
  expect_equal(up$bmm$fit_args$formula$pforms$c, c ~ 0 + set_size, ignore_formula_env=T)

  # prior is replaced
  up <- update(fit1, formula. = bmf(c ~ 1, kappa ~ 1), testmode = TRUE,
               prior = brms::set_prior("normal(0,0.1)", class="Intercept", dpar='kappa'))
  expect_true(is(up, "bmmfit"))
  expect_equal(up$bmm$fit_args$prior$prior[3], "normal(0,0.1)")

  # refuse to change model
  expect_error(update(fit1, model = mixture2p(resp_err = "dev_rad")),
               "You cannot update with a different model")

  up <- update(fit1, save_pars = save_pars(group = FALSE), testmode = TRUE)
  expect_true(is(up, "bmmfit"))
  up <- update(fit1, save_pars = save_pars(latent = FALSE), testmode = TRUE)
  expect_true(is(up, "bmmfit"))
  expect_error(update(fit1, data = data), "use argument 'newdata'")
})
