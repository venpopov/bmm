test_that("add_links works", {
  x <- .model_mixture2p()
  links <- x$links
  x$links <- NULL
  x <- add_links(x)
  expect_equal(x$links, links)
})

test_that("restructure works", {
  skip() # TODO: only run interactively - we need to add some reference fits to the repo internal datasets
  path <- test_path()
  file <- file.path(path, "../internal/ref_fits", "20240215_v0.2.1_mixture2p_seed-365_6ae900f5a4.rds")
  old_fit <- readRDS(file)
  # TODO: this should be part of the restructure
  old_fit$bmm$model <- structure(list(), class = c("bmmmodel", 'mixture2p'))
  new_fit <- restructure.bmm(old_fit)
  expect_equal(new_fit$bmm$model$links,.model_mixture2p()$links)
})


x <- c(        '- The response vairable should be in radians and
               represent the angular error relative to the target\n
               - The non-target features should be in radians and be ')

