test_that("default_prior() works with brmsformula", {
  ff <- brms::bf(count ~ zAge + zBase * Trt + (1 | patient))
  prior <- default_prior(ff, data = brms::epilepsy, family = poisson())
  expect_equal(class(prior)[1], "brmsprior")
})

test_that("default_prior() works with formula", {
  ff <- count ~ zAge + zBase * Trt + (1 | patient)
  prior <- default_prior(ff, data = brms::epilepsy, family = poisson())
  expect_equal(class(prior)[1], "brmsprior")
})

test_that("default_prior() works with bmmformula", {
  ff <- bmmformula(kappa ~ 1, thetat ~ 1, thetant ~ 1)
  prior <- default_prior(ff, oberauer_lin_2017, mixture3p(resp_error = "dev_rad",
                                                         nt_features = "col_nt",
                                                         set_size = "set_size",
                                                         regex = T))
  expect_equal(class(prior)[1], "brmsprior")
})

test_that("combine prior returns a brmsprior object", {
  prior1 <- brms::prior(normal(0, 1), class = "sd", dpar = "c")
  prior2 <- brms::prior(normal(0, 1), class = "sd", dpar = "kappa")

  # combine the prior
  prior <- combine_prior(prior1, prior2)
  expect_equal(class(prior)[1], "brmsprior")
})

test_that("in combine prior, prior2 overwrites only shared components with prior1", {
  prior1 <- brms::prior(normal(0, 1), class = "sd", dpar = "c1") +
    brms::prior(normal(0, 1), class = "sd", dpar = "c2") +
    brms::prior(normal(0, 1), class = "sd", dpar = "c3")
  prior2 <- brms::prior(normal(0, 1), class = "sd", dpar = "kappa") +
    brms::prior(normal(0, 2), class = "sd", dpar = "c2")

  # combine the prior
  prior <- combine_prior(prior1, prior2)
  expect_equal(nrow(prior), 4)
  expect_equal(dplyr::filter(prior, dpar == "c1"), dplyr::filter(prior1, dpar == "c1"))
  expect_equal(dplyr::filter(prior, dpar == "c2"), dplyr::filter(prior2, dpar == "c2"))
  expect_equal(dplyr::filter(prior, dpar == "c3"), dplyr::filter(prior1, dpar == "c3"))
  expect_equal(dplyr::filter(prior, dpar == "kappa"), dplyr::filter(prior2, dpar == "kappa"))
})

test_that("no check for sort_data with default_priors function", {
  withr::local_options('bmm.sort_data' = 'check')
  res <- capture_messages(default_prior(bmf(kappa ~ set_size, c ~ set_size),
                                        oberauer_lin_2017,
                                        sdm('dev_rad')))
  expect_false(any(grepl("sort", res)))
})



