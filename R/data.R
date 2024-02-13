#' Data from Experiment 2 reported by Zhang & Luck (2008)
#'
#' Raw data of 8 subjects for the response error in a continuous reproduction task
#' with set size 1, 2, 3, and 6 reported by Zhang & Luck (2008).
#'
#' @format ## `ZhangLuck_2008`
#' A data frame with 4,000 rows and 9 columns:
#' \describe{
#'   \item{subID}{Integer uniquely identifying different subjects}
#'   \item{trial}{Trial identifyier}
#'   \item{setsize}{The setsize of the data in this row}
#'   \item{RespErr}{The response error, that is the difference between the reponse
#'   given and the target color in radians.}
#'   \item{Pos_Lure1, Pos_Lure2, Pos_Lure3, Pos_Lure4, Pos_Lure5}{Position of the lure items relative to the target color.}
#'
#' }
#'
#' @source <https://www.nature.com/articles/nature06860>
"ZhangLuck_2008"


#' Data from Experiment 1 reported by Oberauer & Lin (2017)
#'
#' Raw data of 19 subjects that completed a continuous reproduction task
#' with set size 1 to 8 reported by Oberauer & Lin (2017).
#'
#' @format ## `OberauerLin_2017`
#' A data frame with 15,200 rows and 39 columns:
#' \describe{
#'   \item{ID}{Integer uniquely identifying different subjects}
#'   \item{Session}{Session number}
#'   \item{Trial}{Trial number within each session}
#'   \item{SetSize}{The setsize of the data in this row}
#'   \item{Response}{The response in degrees given on the color wheel}
#'   \item{deviation}{The response error or deviation of the `Response` from the target color (i.e., `Item1_Col`) in degrees.}
#'   \item{dev_rad}{The response error converted from degrees to radians.}
#'   \item{Item1_Col,Item2_Col,Item3_Col,Item4_Col,Item5_Col,Item6_Col,Item7_Col,Item8_Col}{The absolute colors of all items in degrees of the color wheel. Although there are always eight values given even for set sizes smaller than 8, only the colors from item 1 to the respective set size were shown.}
#'   \item{Item1_Pos,Item2_Pos,Item3_Pos,Item4_Pos,Item5_Pos,Item6_Pos,Item7_Pos,Item8_Pos}{The position of all items in clockwise order around the circle. There were 12 possible positions, thus each position was 30 degrees apart from each other. Although positions are always given for all items, only item 1 to the respective set size was shown.}
#'   \item{Item1_Col_rad,Item2_Col_rad,Item3_Col_rad,Item4_Col_rad,Item5_Col_rad,Item6_Col,Item7_Col_rad,Item8_Col_rad}{The relative position of colors to the target item (i.e. `Item1_Col`) of all items in radians. Although there are always eight values given even for set sizes smaller than 8, only the colors from item 1 to the respective set size were shown.}
#'   \item{Item1_Pos_rad,Item2_Pos_rad,Item3_Pos_rad,Item4_Pos_rad,Item5_Pos_rad,Item6_Pos,Item7_Pos_rad,Item8_Pos_rad}{The relative position of all items to the position of the target item (i.e. `Item1_Pos`) in radians. Although there are always eight values given even for set sizes smaller than 8, only the colors from item 1 to the respective set size were shown.}
#'
#' }
#'
#' @source <https://osf.io/m4shu>
"OberauerLin_2017"
