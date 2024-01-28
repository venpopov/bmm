test_that("sdm distribution functions run without errors", {
  n <- 10
  res <- dsdm(runif(n, -pi, pi), mu = 1, c = 3, kappa = 1:n)
  expect_true(length(res) == n)
  res <- psdm(runif(n, -pi, pi), mu = rnorm(n), c = 0:(n-1), kappa = 0:(n-1))
  expect_true(length(res) == n)
  expect_error(rsdm(n, mu = rnorm(n), c = 0:(n-1), kappa = 0:(n-1)),
               "Function not vectorized yet. Only single values for mu, c, and kappa are supported")
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
  expect_equal(psdm(pi-0.00000000001, mu = 0, c = 3, kappa = 3), 1)
})


test_that("rsdm returns values between -pi and pi", {
  res <- rsdm(1000, mu = 0, c = 3, kappa = 3)
  expect_true(all(res >= -pi) && all(res <= pi))

  res <- rsdm(1000, mu = 0, c = 3, kappa = 3)
  expect_true(all(res >= -pi) && all(res <= pi))
})
