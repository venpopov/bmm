test_that("construct_m3_act_funs works with simple m3", {
  model <- m3(
    resp_cats = c("correct", "other", "npl"),
    num_options = c(1, 4, 5),
    version = "ss"
  )
  expect_equal(
    construct_m3_act_funs(model, warnings = FALSE),
    bmf(correct ~ b + a + c, other ~ b + a, npl ~ b),
    ignore_formula_env = TRUE
  )
})

test_that("construct_m3_act_funs works with complex span m3", {
  model <- m3(
    resp_cats = c("correct", "dist_context", "other", "dist_other", "npl"),
    num_options = c(1, 4, 5, 4, 5),
    version = "cs"
  )
  expect_equal(
    construct_m3_act_funs(model, warnings = FALSE),
    bmf(
      correct ~ b + a + c, 
      dist_context ~ b + f * a + f * c, 
      other ~ b + a, 
      dist_other ~ b + f * a,
      npl ~ b
    ),
    ignore_formula_env = TRUE
  )
})

test_that("construct_m3_act_funs gives error for other models", {
  model <- m3(
    resp_cats = c("correct", "dist_context", "other", "dist_other", "npl"),
    num_options = c(1, 4, 5, 4, 5),
    version = "custom"
  )
  expect_error(construct_m3_act_funs(model), "can only be generated for")

  model <- sdm("dev_rad")
  expect_error(construct_m3_act_funs(model), "can only be generated for")
})
