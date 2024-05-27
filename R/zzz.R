.onLoad <- function(libname, pkgname) {
  suppressMessages(bmm_options(reset_options = TRUE))
}

.onAttach <- function(libname, pkgname) {
  # add banner of package
  banner <- " _
| |_ _____ _____
| . |     |     |
|___|_|_|_|_|_|_|
"
  local_version <- utils::packageVersion("bmm")
  versionMsg <- paste0("Loading bmm (version: ", local_version, ").\n")

  startUpMsg <- c(
    paste0(
      "A short introduction to package is available by calling help(\"bmm\"). \n",
      "More detailed articles on how to fit different models are available online at https://venpopov.github.io/bmm/articles \n",
      "You can view the list of currently available models by calling supported_models().\n"
    )
  )

  optionsMsg <- tryCatch2(bmm_options())$message

  if (interactive() && !isFALSE(as.logical(Sys.getenv("BMM_CHECK_CRAN_VERSION")))) {
    # test if local installation is behind CRAN
    cran_pkgs <- utils::available.packages(repos = "http://cran.us.r-project.org")
    cran_version <- cran_pkgs[which(cran_pkgs[, "Package"] == "bmm"), "Version"]
    behind_cran <- cran_version > local_version

    if (length(behind_cran) > 0 && behind_cran) {
      msg <- "A newer version of bmm is available on CRAN."
      packageStartupMessage(msg, "\nWould you like to install it?")
      if (utils::menu(c("Yes", "No")) == 1) {
        utils::update.packages("bmm")
      }
    } else {
      packageStartupMessage(banner, versionMsg, startUpMsg, optionsMsg)
    }
  }
}
