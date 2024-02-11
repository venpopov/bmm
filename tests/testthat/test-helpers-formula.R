test_that('extract vars returns the correct variables with one formula', {
  formula <- brms::bf(y ~ A + B + C + (1|D) + (F*G|E))
  vars <- extract_vars(formula)
  vars <- sort(vars)
  expect_equal(vars, LETTERS[1:7])

  vars <- extract_vars(formula, include_resp = T)
  vars <- sort(vars)
  expect_equal(vars, c(LETTERS[1:7],'y'))
})

test_that('extract vars returns the correct variables with more than one formula', {
  formula <- brms::bf(y ~ A,
                      sigma ~ B + C + (1|D) + (F*G|E))
  vars <- extract_vars(formula)
  vars <- sort(vars)
  expect_equal(vars, LETTERS[1:7])

  vars <- extract_vars(formula, include_resp = T)
  vars <- sort(vars)
  expect_equal(vars, c(LETTERS[1:7],'y'))
})

test_that('extract vars returns the correct variables with different family', {
  formula <- brms::bf(y ~ A,
                      kappa ~ B + C + (1|D) + (F*G|E))
  vars <- extract_vars(formula, brms::von_mises())
  vars <- sort(vars)
  expect_equal(vars, LETTERS[1:7])

  vars <- extract_vars(formula, brms::von_mises(), include_resp = T)
  vars <- sort(vars)
  expect_equal(vars, c(LETTERS[1:7],'y'))
})

