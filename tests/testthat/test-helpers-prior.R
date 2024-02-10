test_that("get_model_prior() returns a brmsprior object", {
  # define formula
  ff <- brms::bf(y ~ 1,
                 kappa ~ 1,
                 thetat ~ 1,
                 thetant ~ 1)

  # simulate data
  dat <- data.frame(y = rmixture3p(n = 200),
                    nt1_loc = 2,
                    nt2_loc = -1.5)

  # fit the model
  prior <- get_model_prior(formula = ff,
                           data = dat,
                           model = mixture3p(non_targets = paste0('nt',1,'_loc'), setsize = 2))
  expect_equal(class(prior)[1], "brmsprior")
})


test_that("combine prior returns a brmsprior object", {
  prior1 <- brms::prior(normal(0,1), class = 'sd', dpar='c')
  prior2 <- brms::prior(normal(0,1), class = 'sd', dpar='kappa')

  # combine the prior
  prior <- combine_prior(prior1,prior2)
  expect_equal(class(prior)[1], "brmsprior")
})

test_that("in combine prior, prior2 overwrites only shared components with prior1", {
  prior1 <- brms::prior(normal(0,1), class = 'sd', dpar='c1') +
    brms::prior(normal(0,1), class = 'sd', dpar='c2') +
    brms::prior(normal(0,1), class = 'sd', dpar='c3')
  prior2 <- brms::prior(normal(0,1), class = 'sd', dpar='kappa') +
    brms::prior(normal(0,2), class = 'sd', dpar='c2')

  # combine the prior
  prior <- combine_prior(prior1,prior2)
  expect_equal(nrow(prior), 4)
  expect_equal(dplyr::filter(prior, dpar == 'c1'), dplyr::filter(prior1, dpar == 'c1'))
  expect_equal(dplyr::filter(prior, dpar == 'c2'), dplyr::filter(prior2, dpar == 'c2'))
  expect_equal(dplyr::filter(prior, dpar == 'c3'), dplyr::filter(prior1, dpar == 'c3'))
  expect_equal(dplyr::filter(prior, dpar == 'kappa'), dplyr::filter(prior2, dpar == 'kappa'))
})
