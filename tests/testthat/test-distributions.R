test_that("sdm distribution functions run without errors", {
  n <- 10
  res <- dsdm(runif(n, -pi, pi), mu = 1, c = 3, kappa = 1:n)
  expect_true(length(res) == n)
  res <- psdm(runif(n, -pi, pi), mu = rnorm(n), c = 0:(n - 1), kappa = 0:(n - 1))
  expect_true(length(res) == n)
  res <- rsdm(n, mu = rnorm(n), c = 0:(n - 1), kappa = 0:(n - 1))
  expect_true(length(res) == n)

  x <- runif(n, -pi, pi)
  res <- dsdm(x, mu = 1, c = 3, kappa = 1:n)
  res_log <- dsdm(x, mu = 1, c = 3, kappa = 1:n, log = TRUE)
  expect_true(all.equal(res, exp(res_log)))
})

test_that("dsdm integrates to 1", {
  expect_equal(integrate(dsdm, -pi, pi, mu = 0, c = 3, kappa = 3)$value, 1)
})


test_that("psdm is between 0 and 1", {
  res <- psdm(runif(1000, -pi, pi), mu = runif(1000, -pi, pi), c = 3, kappa = 3)
  expect_true(all(res >= 0) && all(res <= 1))
})

test_that("psdm returns 0 for q == -pi, 0.5 for q = mu, and 1 for q almost pi, when mu == 0", {
  expect_equal(psdm(-pi, mu = 0, c = 3, kappa = 3), 0)
  expect_equal(psdm(0, mu = 0, c = 3, kappa = 3), 0.5)
  expect_equal(psdm(pi - 0.00000000001, mu = 0, c = 3, kappa = 3), 1)
})


test_that("rsdm returns values between -pi and pi", {
  res <- rsdm(1000, mu = 0, c = 3, kappa = 3)
  expect_true(all(res >= -pi) && all(res <= pi))

  res <- rsdm(1000, mu = 0, c = 3, kappa = 3)
  expect_true(all(res >= -pi) && all(res <= pi))
})

test_that("conversion between sdm parametrizations works", {
  kappa <- rnorm(100, 5, 1)
  c_b <- rnorm(100, 5, 1)
  c_se <- c_bessel2sqrtexp(c_b, kappa)
  c_b2 <- c_sqrtexp2bessel(c_se, kappa)
  expect_equal(c_b, c_b2)
})

test_that("dsdm parametrization conversion returns accurate results", {
  y <- seq(-pi, pi, length.out = 100)
  kappa <- rnorm(100, 5, 1)
  c_b <- rnorm(100, 5, 1)
  c_se <- c_bessel2sqrtexp(c_b, kappa)
  d1 <- dsdm(y, 0, c_b, kappa, parametrization = "bessel")
  d2 <- dsdm(y, 0, c_se, kappa, parametrization = "sqrtexp")
  expect_equal(d1, d2)
})

test_that("dmixture2p integrates to 1", {
  expect_equal(integrate(dmixture2p, -pi, pi,
    mu = runif(1, min = -pi, pi),
    kappa = runif(1, min = 1, max = 20),
    p_mem = runif(1, min = 0, max = 1)
  )$value, 1)
})

test_that("dmixture3p integrates to 1", {
  expect_equal(integrate(dmixture3p, -pi, pi,
    mu = runif(3, min = -pi, pi),
    kappa = runif(1, min = 1, max = 20),
    p_mem = runif(1, min = 0, max = 0.6),
    p_nt = runif(1, min = 0, max = 0.3)
  )$value, 1)
})

test_that("dimm integrates to 1", {
  expect_equal(integrate(dimm, -pi, pi,
    mu = runif(3, min = -pi, pi),
    dist = c(0, runif(2, min = 0.1, max = pi)),
    kappa = runif(1, min = 1, max = 20),
    c = runif(1, min = 0, max = 3),
    a = runif(1, min = 0, max = 1),
    s = runif(1, min = 1, max = 20),
    b = 0
  )$value, 1)
})

test_that("rmixture2p returns values between -pi and pi", {
  res <- rmixture2p(500,
    mu = runif(1, min = -pi, pi),
    kappa = runif(1, min = 1, max = 20),
    p_mem = runif(1, min = 0, max = 1)
  )
  expect_true(all(res >= -pi) && all(res <= pi))
})

test_that("rmixture3p returns values between -pi and pi", {
  res <- rmixture3p(500,
    mu = runif(3, min = -pi, pi),
    kappa = runif(1, min = 1, max = 20),
    p_mem = runif(1, min = 0, max = 0.6),
    p_nt = runif(1, min = 0, max = 0.3)
  )
  expect_true(all(res >= -pi) && all(res <= pi))
})

test_that("rimm returns values between -pi and pi", {
  res <- rimm(500,
    mu = runif(3, min = -pi, pi),
    dist = c(0, runif(2, min = 0.1, max = pi)),
    kappa = runif(1, min = 1, max = 20),
    c = runif(1, min = 0, max = 3),
    a = runif(1, min = 0, max = 1),
    s = runif(1, min = 1, max = 20),
    b = 0
  )
  expect_true(all(res >= -pi) && all(res <= pi))
})
