#' Prices of tickets applicable in vehicles
#' operated on the lines organised by ZTM.
#'
#' A dataset containing the prices
#' and other attributes of 34 different types of ZTM tickets.
#'
#' @format A data frame with 34 rows and 7 variables:
#' \describe{
#'   \item{Lp}{ID number}
#'   \item{zone}{ticket zone}
#'   \item{time_min}{validity time in minutes}
#'   \item{discount}{Entitlement to a discount of 50\%}
#'   \item{price}{standard price - fare}
#'   \item{Warszawiak}{Entitlement to a Varsovian ticket discount}
#'   \item{name}{Types of ticket}
#'   ...
#' }
"ztm_price_table"
