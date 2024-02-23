## set default options for afex_options:
.onLoad <- function(libname, pkgname) {
  options <- options()
  options_bmm <- list(
    bmm.default_priors = TRUE
  )

  options_to_set <- !(names(options_bmm) %in% names(options))
  if (any(options_to_set)) options(options_bmm[options_to_set])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  # test if local installation is behing CRAN
  cran_pkgs <- available.packages()
  cran_version <- cran_pkgs[which(cran_pkgs[,"Package"] == "bmm"),"Version"]
  local_version <- packageVersion("bmm")
  behind_cran <- cran_version > local_version

  startUpMsg <- c(
    paste0("Loading bmm (version: ",local_version,"). ",
    "A short introduction to package is available by calling help(\"bmm\"). More detailed
    articles on how to fit different models are available via vignettes(\"bmm\").")
  )

  if (interactive()) {
    if (length(behind_cran) > 0 && behind_cran) {
      msg <- "A newer version of bmm is available on CRAN."
      packageStartupMessage(msg, "\nWould you like to install it?")
      if (menu(c("Yes", "No")) == 1) {
        update.packages("vistributions")
      }
    } else {
      packageStartupMessage(paste(strwrap(startUpMsg), collapse = "\n"))
    }
  }
}
