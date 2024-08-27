test_that("bmm version is added to mock model", {
  fit <- bmm(formula = bmmformula(c ~ 1, kappa ~ 1),
             data = data.frame(y = rsdm(n=10)),
             model = sdm(resp_error = "y"),
             backend = 'mock',
             rename = F,
             mock = 1)
  expect_true("bmm" %in% names(fit$version))
})


test_that("get_mu_pars works",{
  a <- brms::brm(y~ a, data.frame(y = c(1,2,3), a = c('A',"B","C")),
           backend = "mock", mock_fit = 1, rename=F)
  mus <- get_mu_pars(a)
  expect_equal(mus, c("Intercept", "aB", "aC"))
})
