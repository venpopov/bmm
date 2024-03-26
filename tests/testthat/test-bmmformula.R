test_that('+.bmmformula method works', {
  withr::local_options('bmm.silent'=2)
  f1 <- bmf(y~1)
  f2 <- bmf(kappa~1)
  f3 <- bmf(kappa~1, m ~ 1)
  f4 <- bmf(kappa~1, m ~ A+B+(A|ID))
  f5 <- bmf(c~set_size)
  f6 <- formula(c~1)
  f7 <- formula(m ~ A+B+(A|ID))

  # adding two bmmformulas works with one formula in each
  expect_equal(f1 + f2, bmf(y~1, kappa~1))

  # adding two bmmformulas works with different number of formulas in each
  expect_equal(f1 + f3, bmf(y~1, kappa~1, m ~ 1))

  # adding two more complex bmmformulas works
  expect_equal(f1 + f4, bmf(y~1,kappa~1, m ~ A+B+(A|ID)))

  # adding three bmmformulas work
  expect_equal(f1+f2+f5, bmf(y~1, kappa~1, c~set_size))

  # adding a formula to a bmmformula works
  expect_equal(f1 + f6, bmf(y~1, c~1))
  expect_equal(f1 + f7, bmf(y~1, m ~ A+B+(A|ID)))

  # adding a formula to a bmmformula overwrites shared parameters
  suppressMessages(expect_equal(f3 + f7, bmf(kappa~1, m ~ A+B+(A|ID))))

  # the first argument must be a bmmformula
  expect_error(f6 + f1, "The first argument must be a bmmformula.")

  # the second argument must be a formula or a bmmformula
  expect_error(f1 + 1, "The second argument must be a formula or a bmmformula.")
})


test_that('rhs_vars works', {
  f <- bmf(y ~ 1)
  expect_equal(rhs_vars(f),character(0))

  f <- bmf(y ~ a +b, x ~ c)
  expect_equal(rhs_vars(f),c("a","b","c"))

  f <- bmf(y ~ a + b + a:b, x ~ c)
  expect_equal(rhs_vars(f),c("a","b","c"))

  f <- bmf(y ~ a + b + a:b + (a | d), x ~ c)
  expect_equal(rhs_vars(f),c("a","b","d","c"))

  f <- bmf(y ~ a + b + a:b + (a | d), x ~ c + d)
  expect_equal(rhs_vars(f),c("a","b","d","c"))

  f <- bmf(y ~ a + b + a:b + (a | d), x ~ c + d, d ~ m)
  expect_equal(rhs_vars(f),c("a","b","d","c","m"))

  # test with non-linear transformations
  f <- bmf(y ~ a + b + a:b + (a | d), x ~ c + d, d ~ exp(m+j))
  expect_equal(rhs_vars(f),c("a","b","d","c","m","j"))
})

test_that('assign_nl works', {
  x <- bmf(y ~ c, c ~ a + b, a ~ d, m ~ 1)
  x <- assign_nl(x)
  types <- is_nl(x)
  expect_equal(types, c(y = TRUE, c = TRUE, a = FALSE, m = FALSE))

  x <- bmf(y ~ 1)
  x <- assign_nl(x)
  types <- is_nl(x)
  expect_equal(types, c(y = FALSE))

  f1 <- bmf(y ~ a)
  f2 <- bmf(a ~ 1)
  f3 <- f1 + f2
  f4 <- bmf(y ~ a, a ~ 1)
  expect_equal(f3, f4)
  types3 <- is_nl(f3)
  expect_equal(types3, c(y = TRUE, a = FALSE))
})


test_that('print.bmmformula works', {
  res <- utils::capture.output(bmf(a ~ 1, b = 2))
  expect_equal(res, c("a ~ 1", "b = 2"))
})
