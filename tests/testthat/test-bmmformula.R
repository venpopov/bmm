test_that('+.bmmformula method works', {
  withr::local_options('bmm.silent'=2)
  f1 <- bmf(y~1)
  f2 <- bmf(kappa~1)
  f3 <- bmf(kappa~1, m ~ 1)
  f4 <- bmf(kappa~1, m ~ A+B+(A|ID))
  f5 <- bmf(c~setsize)
  f6 <- formula(c~1)
  f7 <- formula(m ~ A+B+(A|ID))

  # adding two bmmformulas works with one formula in each
  expect_equal(f1 + f2, bmf(y~1, kappa~1))

  # adding two bmmformulas works with different number of formulas in each
  expect_equal(f1 + f3, bmf(y~1, kappa~1, m ~ 1))

  # adding two more complex bmmformulas works
  expect_equal(f1 + f4, bmf(y~1,kappa~1, m ~ A+B+(A|ID)))

  # adding three bmmformulas work
  expect_equal(f1+f2+f5, bmf(y~1, kappa~1, c~setsize))

  # adding a formula to a bmmformula works
  expect_equal(f1 + f6, bmf(y~1, c~1))
  expect_equal(f1 + f7, bmf(y~1, m ~ A+B+(A|ID)))

  # adding a formula to a bmmformula overwrites shared parameters
  suppressMessages(expect_equal(f3 + f7, bmf(kappa~1, m ~ A+B+(A|ID))))

  # the first argument must be a bmmformula
  expect_error(f6 + f1, "The first argument must be a bmmformula.")
})
