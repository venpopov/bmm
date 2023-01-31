#' Calculate response error relative to non-target values
#'
#' @description Given a vector of responses, and the values of non-targets, this
#'   function computes the error relative to each of the non-targets.
#'
#' @param data A `data.frame` object where each row is a single observation
#' @param response Character. The name of the column in `data` which contains
#'   the response
#' @param lures Character vector. The names of the columns in `data` which
#'   contain the values of the non-targets
#'
#' @return A `data.frame` with n*m rows, where n is the number of rows of `data`
#'   and m is the number of non-target variables. It preserves all other columns
#'   of `data`, except for the non-target locations, and adds a column `y_nt`,
#'   which contains the transformed response error relative to the non-targets
#' @export
#'
calc_error_relative_to_nontargets <- function(data, response, lures) {
  y <- y_nt <- non_target_name <- non_target_value <- NULL
  data <- data %>%
    tidyr::gather(non_target_name, non_target_value, eval(lures))

  data$y_nt <- wrap(data[[response]]-data[["non_target_value"]])
  return(data)
}
