test_that("bmm version is added to mock model", {
  dat <- data.frame(y = rsdm(n=10))
  ff <- brms::bf(y ~ 1,
                 c ~ 1,
                 kappa ~ 1)
  fit <- fit_model(formula = ff,
                   data = dat,
                   model = sdmSimple(),
                   parallel=T,
                   iter=500,
                   backend='mock',
                   rename = F,
                   mock = 1)
  expect_true("bmm" %in% names(fit$version))
})
