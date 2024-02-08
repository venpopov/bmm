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
