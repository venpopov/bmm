test_that("get_prior() works with brmsformula", {
  ff <- brms::bf(count ~ zAge + zBase * Trt + (1 | patient))
  prior <- get_prior(ff, data = brms::epilepsy, family = poisson())
  expect_equal(class(prior)[1], "brmsprior")
})

test_that("get_prior() works with formula", {
  ff <- count ~ zAge + zBase * Trt + (1 | patient)
  prior <- get_prior(ff, data = brms::epilepsy, family = poisson())
  expect_equal(class(prior)[1], "brmsprior")
})

test_that("get_prior() works with bmmformula if brms >= 2.20.14", {
  # define formula
  ff <- bmmformula(
    kappa ~ 1,
    thetat ~ 1,
    thetant ~ 1
  )

  # simulate data
  dat <- OberauerLin_2017

  # fit the model
  if (utils::packageVersion("brms") >= "2.20.14") {
    prior <- get_prior(
      formula = ff,
      data = dat,
      model = mixture3p(
        resp_err = "dev_rad",
        nt_features = "col_nt",
        setsize = "set_size", regex = T
      )
    )
    expect_equal(class(prior)[1], "brmsprior")

    prior2 <- default_prior(
      object = ff,
      data = dat,
      model = mixture3p(
        resp_err = "dev_rad",
        nt_features = "col_nt",
        setsize = "set_size", regex = T
      )
    )
    expect_equal(prior, prior2)
  }
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


test_that("default priors are returned correctly", {
  if (utils::packageVersion("brms") >= "2.20.14") {
    dp <- default_prior(bmf(kappa ~ set_size, thetat ~ set_size),
                        OberauerLin_2017,
                        mixture2p('dev_rad'))
  } else {
    dp <- get_model_prior(bmf(kappa ~ set_size, thetat ~ set_size),
                        OberauerLin_2017,
                        mixture2p('dev_rad'))
  }

  expect_equal(dp[dp$coef == "" & dp$class == "b", ]$prior, c("","normal(0, 1)"))
  expect_equal(dp[dp$coef == "Intercept", ]$prior, c("normal(2, 1)", "logistic(0, 1)"))
})
