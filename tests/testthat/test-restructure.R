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
  class(old_fit) <- c("bmmfit", class(old_fit))
  new_fit <- restructure(old_fit)
  expect_equal(new_fit$bmm$model$links,.model_mixture2p()$links)
})

