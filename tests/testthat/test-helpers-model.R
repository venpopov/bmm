test_that("supported_models() returns a non-empty character vector", {
  expect_type(supported_models(print_call=FALSE), "character")
  expect_gt(length(supported_models(print_call=FALSE)), 0)
})

test_that("get_model() returns the correct function", {
  expect_equal(get_model("mixture2p"), .model_mixture2p)
})

test_that("check_model() refuses invalid models and accepts valid models", {
  expect_error(check_model("invalid_model"))
  expect_error(check_model(structure(list(), class='invalid')))
  okmodels <- supported_models(print_call=FALSE)
  for (model in okmodels) {
    model <- get_model2(model)
    args_list <- formals(model)
    test_args <- lapply(args_list, function(x) {NULL})
    model <- brms::do_call(model, test_args)
    expect_silent(check_model(model))
    expect_type(check_model(model), "list")
  }
})

test_that("check_model() works with regular expressions", {
  dat <- OberauerLin_2017
  models1 <- list(mixture3p("dev_rad",
                            nt_features = paste0('col_nt',1:7),
                            setsize = 'set_size'),
                  IMMfull('dev_rad',
                          nt_features = paste0('col_nt',1:7),
                          nt_distances = paste0('dist_nt',1:7),
                          setsize = 'set_size'),
                  IMMbsc('dev_rad',
                          nt_features = paste0('col_nt',1:7),
                          nt_distances = paste0('dist_nt',1:7),
                          setsize = 'set_size'),
                  IMMabc('dev_rad',
                          nt_features = paste0('col_nt',1:7),
                          setsize = 'set_size')
                  )
  models2 <- list(mixture3p("dev_rad",
                            nt_features = 'col_nt',
                            setsize = 'set_size',
                            regex = TRUE),
                  IMMfull('dev_rad',
                          nt_features = 'col_nt',
                          nt_distances = 'dist_nt',
                          setsize = 'set_size',
                          regex = TRUE),
                  IMMbsc('dev_rad',
                          nt_features = 'col_nt',
                          nt_distances = 'dist_nt',
                          setsize = 'set_size',
                          regex = TRUE),
                  IMMabc('dev_rad',
                          nt_features = 'col_nt',
                          setsize = 'set_size',
                          regex = TRUE)
                  )

  for (i in 1:length(models1)) {
    check1 <- check_model(models1[[i]], dat)
    check2 <- check_model(models2[[i]], dat)
    attributes(check1) <- NULL
    attributes(check2) <- NULL
    expect_equal(check1, check2)
  }
})

test_that("use_model_template() prevents duplicate models", {
  skip_on_cran()
  okmodels <- supported_models(print_call=FALSE)
  for (model in okmodels) {
    expect_error(use_model_template(model))
  }

  model_files <- list.files(path = "R/", pattern = "^bmm_model_.*\\.R$")
  model_files_names <- gsub("^bmm_model_", "", model_files)
  model_files_names <- gsub("\\.R$", "", model_files_names)
  for (model in model_files_names) {
    expect_error(use_model_template(model))
  }
})


test_that("get_stancode() returns a string", {
  # define formula
  ff <- bmmformula(kappa ~ 1,
                    thetat ~ 1,
                    thetant ~ 1)

  # simulate data
  dat <- data.frame(y = rmixture3p(n = 200),
                    nt1_loc = 2,
                    nt2_loc = -1.5)

  # fit the model
  stancode <- get_stancode(formula = ff,
                           data = dat,
                           model = mixture3p(resp_err = "y", nt_features = paste0('nt',1,'_loc'), setsize = 2))
  expect_equal(class(stancode)[1], "character")
})
