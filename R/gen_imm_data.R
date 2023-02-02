#' @title Generate artificial data from the Interference measurement model
#' @description Given a set of parameters, and desired setsize, thi function generates
#' data following the assumptions of the interference measurement models
#'
#' @param parms Matrix/DataFrame. Matrix or Data frame of IMM parameters,
#'    must contain at least one row of values for:
#'      c = context activation,
#'      a = general activation,
#'      s = spatial similarity gradient,
#'      n = background noise,
#'      kappa = precision of memory representations
#' @param nResp Numeric. number of responses to simulate for each subject
#' @param setsize Numeric. Number of items in memory set
#'
#' @return A data.frame object. resp is the response, respErr is the relative
#'    response to the target, Item1 to ItemN is the absolute location of the
#'    N items from -pi to pi for the number of items N specified by the setsize
#'    variable, D1 to DN is the spatial distance of the N items for the N items
#'    specified by the setsize variable
#'
#' @export
gen_imm_data <- function(parms,
                         ntrials = 200, setsize=6){

  # pre-allocate & collect variables for the data simulation
  nsubj <- nrow(parms) # number of subject

  # pre-allocate activation matrix
  acts <- matrix(0, 1, setsize + 1)

  # switch off warnings -> rvonmises function throws a lot of information when
  # generating random responses
  options(warn = -1)

  # pre-allocate data frame for collecting all simulated data
  simData <- data.frame(
    ID = integer(), trial = integer(), setsize = integer(),
    resp = numeric(), respErr = numeric(),
    c = numeric(), a = numeric(), s = numeric(), n = numeric(), kappa = numeric()
  )
  names <- colnames(simData)

  # pre-allocate item location & spatial distance variable
  for (s in 1:(2*setsize)) {
    simData[,ncol(simData) + 1] <- numeric()
  }

  # update column names
  newNames <- c(names,paste0("Item",1:setsize,"_abs"),paste0("spaD",1:setsize))
  colnames(simData) <- newNames

  # Loop through all subject
  for (idx in 1:nsubj) {
    x <- NULL
    dev <- NaN

    memset <- matrix(0, ntrials, setsize)
    colnames(memset) <- paste0("Item",1:setsize,"_abs")

    D <- matrix(0, ntrials, setsize)
    colnames(D) <- paste0("spaD",1:setsize)

    # Loop through all trials
    for (trial in 1:ntrials) {
      # generate random memory set & distances for each trial
      memset[trial,1:setsize] <- bmm::wrap(circular::rvonmises(setsize, 0, 0,  control.circular = list(units = "radians")))  # draw items from uniform
      D[trial,2:setsize] <- runif(setsize - 1, 0.5, pi)   # draw distances of non-targets from uniform range 0.5 to pi

      # compute activation for all items + random guessing
      acts[1:setsize] <- parms[idx,"a"] + exp(-parms[idx,"s"]*D[trial,]) * parms[idx,"c"]
      acts[setsize + 1] <- parms[idx,"n"]

      # convert activations into probabilities
      P <- exp(acts)/sum(exp(acts))

      # randomly select from which distribution the response will come from
      cumP <- cumsum(P)
      draw <- runif(1,0,1)
      aa <- draw < cumP
      choice <- min(which(aa == TRUE))

      # draw response dependent on choice
      if (choice <= setsize) x[trial] <- bmm::wrap(circular::rvonmises(1, memset[trial,choice], parms[idx,"kappa"]))  # response from a memory distribution
      if (choice > setsize) x[trial] <- bmm::wrap(circular::rvonmises(1, 0, 0))               # response from noise distribution

      # compute response error as the deviation of the response from the target item
      dev[trial] = bmm::wrap(x[trial] - memset[trial,1])
    }

    # collect subject data
    subData <- data.frame(ID = idx, setsize = setsize, trial = 1:ntrials, resp = x, respErr = dev,
                          c = parms[idx,"c"], a = parms[idx,"a"], n = parms[idx, "n"],
                          s = parms[idx, "s"], kappa = parms[idx,"kappa"])

    # bind all subject data together
    subData <- cbind(subData,memset,D)

    # bind subject data to full data set
    simData <- rbind(simData,subData)
  }

  # switch warnings back on
  options(warn = 0)

  # add relative locations to the simulated data
  locData <- simData[,grepl("Item",colnames(simData))]
  relLocData <- bmm::wrap(locData - locData$Item1_abs)
  colnames(relLocData) <- paste0("Item",1:setsize,"_rel")
  simData <- cbind(simData,relLocData)

  # return the simulated data
  return(simData)
}
