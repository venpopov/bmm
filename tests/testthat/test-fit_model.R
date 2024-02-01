test_that('Available models run without errors',{
  dat <- gen_3p_data(N=100, pmem=0.6, pnt=0.3, kappa=10, setsize=4, relative_resp=T)
  f2p <- brms::bf(y ~ 1, kappa ~ 1, thetat ~ 1)
  fit <- fit_model(formula = f2p,
                   data = dat,
                   model_type = "2p",
                   parallel=T,
                   iter=500,
                   backend="mock")
})
