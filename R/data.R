#' Data from Experiment 2 reported by Zhang & Luck (2008)
#'
#' Raw data of 8 subjects for the response error in a continuous reproduction task
#' with set size 1, 2, 3, and 6 reported by Zhang & Luck (2008).
#'
#' @format ## `zhang_luck_2008`
#' A data frame with 4,000 rows and 9 columns:
#' \describe{
#'   \item{subID}{Integer uniquely identifying different subjects}
#'   \item{trial}{Trial identifyier}
#'   \item{setsize}{The set_size of the data in this row}
#'   \item{response_error}{The response error, that is the difference between the response
#'   given and the target color in radians.}
#'   \item{col_lure1, col_Lure2, col_Lure3, col_Lure4, col_Lure5}{Color value of the lure items coded relative to the target color.}
#'
#' }
#' @keywords dataset
#' @source <https://www.nature.com/articles/nature06860>
"zhang_luck_2008"


#' Data from Experiment 1 reported by Oberauer & Lin (2017)
#'
#' Raw data of 19 subjects that completed a continuous reproduction task
#' with set size 1 to 8 reported by Oberauer & Lin (2017).
#'
#' @format ## `oberauer_lin_2017`
#' A data frame with 15,200 rows and 19 columns:
#' \describe{
#'   \item{ID}{Integer uniquely identifying different subjects}
#'   \item{session}{Session number}
#'   \item{trial}{Trial number within each session}
#'   \item{set_size}{The set_size of the data in this row}
#'   \item{dev_rad}{The response error, that is the difference between the response
#'   given and the target color in radians.}
#'   \item{col_nt1, col_nt2, col_nt3, col_nt4, col_nt5, col_nt6, col_nt7}{The non-target items' color value relative to the target.}
#'   \item{dist_nt1, dist_nt2, dist_nt3, dist_nt4, dist_nt5, dist_nt6, dist_nt7, dist_nt8}{The spatial distance between all non-target items and the target item in radians.}
#'
#' }
#'
#' @keywords dataset
#' @source <https://osf.io/m4shu>
"oberauer_lin_2017"
