generate_bmm_examples <- function(seed = 123) {
  withr::local_options(brms.backend = 'rstan',
                       bmm.parallel = TRUE,
                       bmm.sort_data = TRUE)
  withr::local_package('dplyr')
  set.seed(seed)


  ## Example 1: For use in testing
  data <- OberauerLin_2017 %>%
    mutate(ID = as.factor(ID),
           set_size = as.factor(set_size)) %>%
    dplyr::filter(ID %in% c(1,2,3,4,5,6,7,8,9,10),
           set_size %in% c(1,2,3,4)) %>%
    arrange(set_size, ID)
  formula <- bmf(c ~ 0 + set_size, kappa ~ 1)
  model <- sdmSimple('dev_rad')
  bmmfit_example1 <- fit_model(formula, data, model,
                               parallel = TRUE,
                               iter = 100,
                               refresh = 0,
                               init = 1,
                               chains = 1,
                               backend = 'rstan',
                               save_warmup = FALSE)
  bmmfit_example1$bmm$fit_args$data <- NULL


  ## Example 2: bmmfit_sdm_vignette
  set.seed(seed)
  cs <- c(2, 9, 2)
  kappas <- c(3, 1, 8)
  y <- c(rsdm(n = 1000, mu=0, c = cs[1], kappa = kappas[1], parametrization = "sqrtexp"),
         rsdm(n = 1000, mu=0, c = cs[2], kappa = kappas[2], parametrization = "sqrtexp"),
         rsdm(n = 1000, mu=0, c = cs[3], kappa = kappas[3], parametrization = "sqrtexp"))
  dat <- data.frame(y = y, cond = factor(rep(c('A','B','C'), each=1000)))
  formula <- bmf(c ~ 0 + cond, kappa ~ 0 + cond)
  model <- sdmSimple('y')
  bmmfit_sdm_vignette <- fit_model(formula, dat, model, init = 0.5, iter = 2000,
                                   chains = 4, save_pars = save_pars(group = FALSE))
  bmmfit_sdm_vignette$bmm$fit_args$data <- NULL


  ## Example 3: bmmfit_mixture2p_vignette
  set.seed(seed)
  dat <- mixtur::bays2009_full
  dat_preprocessed <- dat %>%
    mutate(error = wrap(response - target),
           non_target_1 = wrap(non_target_1 - target),
           non_target_2 = wrap(non_target_2 - target),
           non_target_3 = wrap(non_target_3 - target),
           non_target_4 = wrap(non_target_4 - target),
           non_target_5 = wrap(non_target_5 - target),
           set_size = as.factor(set_size))
  formula <- bmf(thetat ~ 0 + set_size + (0 + set_size || id),
                 kappa ~ 0 + set_size + (0 + set_size || id))
  model <- mixture2p(resp_err = "error")
  bmmfit_mixture2p_vignette <- fit_model(
    formula = formula,
    data = dat_preprocessed,
    model = model,
    parallel = T,
    iter = 2000,
    chains = 4,
    refresh = 100,
    save_pars = save_pars(group = FALSE),
    save_warmup = FALSE
  )
  bmmfit_mixture2p_vignette$bmm$fit_args$data <- NULL

  ## Example 4: bmmfit_imm_vignette
  set.seed(seed)
  Cs <- c(4,4,2,2)
  As <- c(0.5,1,0.5,0.5)
  Ss <- c(10,10,5,5)
  kappas <- c(15,10,15,10)
  nTrials = 1000
  setsize = 5
  simData <- data.frame()
  for (i in 1:length(Cs)) {
    item_location <- c(0, runif(setsize - 1, -pi,pi))
    item_distance <- c(0, runif(setsize - 1, min = 0.1, max = pi))
    genData <- rIMM(n = nTrials,
                    mu = item_location,
                    dist = item_distance,
                    c = Cs[i], a = As[i],
                    b = 0, s = Ss[i], kappa = kappas[i])
    condData <- data.frame(
      resp_err = genData,
      trialID = 1:nTrials,
      cond = i,
      color_item1 = 0,
      dist_item1 = 0
    )
    init_colnames <- colnames(condData)
    for (j in 1:(setsize - 1)) {
      condData <- cbind(condData,item_location[j + 1])
      condData <- cbind(condData,item_distance[j + 1])
    }
    colnames(condData) <- c(init_colnames,
                            paste0(rep(c("color_item","dist_item"),times = setsize - 1),
                                   rep(2:(setsize),each = 2)))
    simData <- rbind(simData,condData)
  }
  simData$cond <- as.factor(simData$cond)
  model_formula <- bmf(
    c ~ 0 + cond,
    a ~ 0 + cond,
    s ~ 0 + cond,
    kappa ~ 0 + cond
  )
  model <- IMMfull(resp_err = "resp_err",
                   nt_features = paste0("color_item",2:5),
                   setsize = setsize,
                   nt_distances = paste0("dist_item",2:5))
  bmmfit_imm_vignette <- fit_model(
    formula = model_formula,
    data = simData,
    model = model,
    chains = 4,
    save_pars = save_pars(group = FALSE),
    save_warmup = FALSE
  )
  bmmfit_imm_vignette$bmm$fit_args$data <- NULL

  usethis::use_data(bmmfit_example1, bmmfit_sdm_vignette, bmmfit_mixture2p_vignette, bmmfit_imm_vignette, internal = TRUE, overwrite = TRUE)

}

generate_bmm_examples(15234)
