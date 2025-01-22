library(testthat)

test_that("k2sd works", {
  # Test format
  kappa <- runif(10)
  expect_length(k2sd(kappa), 10)
  expect_type(k2sd(kappa), "double")
  expect_length(k2sd(2), 1)
  expect_type(k2sd(2), "double")
  
  # Test edge cases
  expect_equal(k2sd(0), Inf)
  expect_equal(k2sd(Inf), 0)
  
  # Test vector of edge cases
  expect_equal(k2sd(c(0, Inf)), c(Inf, 0))
  
  # Test known values (compared to pre-computed results)
  expect_equal(k2sd(1), 1.270088, tolerance = 1e-6)
  expect_equal(k2sd(10), 0.3248638, tolerance = 1e-6)
  
  # Test NA handling
  expect_true(is.na(k2sd(NA)))
  expect_true(all(is.na(k2sd(c(1, NA, 3))[2])))
  
  # Test invalid inputs
  expect_error(k2sd("a"))
  expect_error(k2sd(NULL))
})

test_that("c parameter conversions work", {
  # Test basic conversion
  c_sqrtexp <- 4
  kappa <- 3
  c_bessel <- c_sqrtexp2bessel(c_sqrtexp, kappa)
  expect_equal(c_bessel2sqrtexp(c_bessel, kappa), c_sqrtexp)
  
  # Test vector inputs
  c_vec <- c(1, 2, 3)
  kappa_vec <- c(2, 3, 4)
  c_bessel_vec <- c_sqrtexp2bessel(c_vec, kappa_vec)
  expect_equal(c_bessel2sqrtexp(c_bessel_vec, kappa_vec), c_vec)
    
  # Test error handling
  expect_error(c_sqrtexp2bessel(-1, 2), "c must be non-negative")
  expect_error(c_bessel2sqrtexp(1, -2), "kappa must be non-negative")
})

