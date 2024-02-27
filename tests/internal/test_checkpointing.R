test_that('fit_model works with checkpointing', {
  folder <- 'local/checkpoints2'
  on.exit(unlink(folder, recursive = T, force = T))

  data1 <- dplyr::filter(OberauerLin_2017, set_size %in% c(1,2,3,4), ID %in% 1:10)
  formula <- bmf(c ~ set_size, kappa ~ 1)
  model <- sdmSimple('dev_rad')

  cat("\n\nRunning for 2 checkpoints then stopping\n\n")

  fit <- try(fit_model(formula, data1, model,
                      parallel = T,
                      backend = 'cmdstanr',
                      sort_data = T,
                      iter = 100,
                      checkpoint_every = 25,
                      stop_after = 50,
                      checkpoints_folder = folder),
             silent = T)

  cat("\n\nTrying to pick up where we stopped\n\n")

  fit <- try(fit_model(formula, data1, model,
                       parallel = T,
                       backend = 'cmdstanr',
                       sort_data = T,
                       iter = 100,
                       checkpoint_every = 25,
                       checkpoints_folder = folder),
             silent = T)

  expect_false(is(fit, 'try-error'))
})
