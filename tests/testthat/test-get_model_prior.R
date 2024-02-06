test_that("get_model_prior() returns a brmsprior object", {
  # generate artificial data from the Bays et al (2009) 3-parameter mixture model
  dat <- gen_3p_data(N=2, pmem=0.6, pnt=0.3, kappa=10, setsize=4, relative_resp=T)

  # define formula
  ff <- brms::bf(y ~ 1,
                 kappa ~ 1,
                 thetat ~ 1,
                 thetant ~ 1)

  # simulate data
  dat <- gen_3p_data(N = 200)

  # fit the model
  prior <- get_model_prior(formula = ff,
                  data = dat,
                  model = mixture3p(non_targets = paste0('nt',1,'_loc'), setsize = 2))
  expect_equal(class(prior)[1], "brmsprior")
})
