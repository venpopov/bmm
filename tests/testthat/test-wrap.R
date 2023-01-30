test_that("Returns the same for values between -pi and pi", {
  x <- runif(100, -pi, pi)
  expect_equal(wrap(x), x)
})

test_that("Returns the correct value for values between (pi, 2*pi)", {
  x <- pi+1
  expect_equal(wrap(x), -(pi-1))
})


test_that("Returns the correct value for values between (-2*pi, -pi)", {
  x <- -pi-1
  expect_equal(wrap(x), pi-1)
})

test_that("Returns the correct value for values over 2*pi", {
  x <- 2*pi+1
  expect_equal(wrap(x), 1)
})

test_that("Returns the correct value for values between (3*pi,4*pi)", {
  x <- 3*pi+1
  expect_equal(wrap(x), -(pi-1))
})
