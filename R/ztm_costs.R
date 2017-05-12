#' Calculate minimal daily cost of optimal ticket
#'
#' @description tickets_costs_daily_ZTM chooses optimat tickets and calculates minimal daily cost of commuting by warsaw public transport.
#' @param travel_time numeric. Daily travel time in minutes.
#' @param discount ligical. Entitlement to a discount of 50\%.
#' @param zone numeric. Ticket zone 1, 2 or 3 - 3 means zone 1 and 2
#' @param varsovian logical. Entitlement to a Varsovian ticket discount
#' @return tickets_costs_daily_ZTM returns a list containing following components
#'   \item{ztm_ticket}{data.frame. Attributes of optimal ticket}
#'   \item{number_of_tickets}{Number of optimal tickets}
#'   \item{tickets_cost}{Minimal total daily cost of commuting for optimal tickets in PLN}
#' @examples
#' tickets_costs_daily_ZTM(travel_time=60, discount=TRUE, zone=1, varsovian=TRUE)
#' @export
tickets_costs_daily_ZTM <- function(travel_time, discount, zone, varsovian) {
    # Exception handling for travel_time argument
    if (length(travel_time) != 1) {
        stop("Error: argument travel_time is not a vector of length 1. Please enter travel_time as numeric vector of length 1.")
    }
    if (is.numeric(travel_time) == FALSE) {
        stop("Error: argument travel_time is not numeric. Please enter travel_time as numeric vector of length 1.")
    }
    if (travel_time <= 0 & travel_time > 1440) {
        stop("Error: argument travel_time is smaller than or equal to 0 or greater than 1440. Please enter numeric from range (0,1440].")
    }
    # Exception handling for discount argument
    if (length(discount) != 1) {
        stop("Error: argument discount is not a vector of length 1. Please enter TRUE or FALSE.")
    }
    if (is.logical(discount) == FALSE | is.na(discount) ) {
        stop("Error: argument discount is not logical. Please enter TRUE or FALSE.")
    }
    # Exception handling for zone argument
    if (length(zone) != 1) {
        stop("Error: argument zone is not a vector of length 1. Please enter integer 1, 2 or 3")
    }
    if ( is.numeric(zone) == FALSE) {
      stop("Error: argument zone is not numeric. Please enter zone as integer 1, 2 or 3.")
    }
    if ((zone %in% 1:3) == FALSE) {
        stop("Error: argument zone is incorrect. Please enter integer 1, 2 or 3")
    }
    # Exception handling for varsovian argument
    if (length(varsovian) != 1) {
        stop("Error: argument varsovian is not a vector of length 1. Please enter TRUE or FALSE.")
    }
    if (is.logical(varsovian) == FALSE) {
        stop("Error: argument varsovian is not logical. Please enter TRUE or FALSE.")
    }
    # varsovian filter indicates what kind of tariff you are granted - regular (FALSE) or regular and varsovian (FALSE and TRUE)) )
    if (varsovian == FALSE) {
        # only regular tariff
        varsovian_filter <- FALSE
    } else {
        # regular and varsovian tariff
        varsovian_filter <- c(FALSE, TRUE)
    }
    # zone_filter indicates what kind of ticket you are allowed to use: 1st zone, 2nd zone or 1st and 2nd zone
    if (zone == 1) {
        # tickets for 1st zone and tickets for 1st and 2nd zone
        zone_filter <- c(1, 3)
    } else {
        if (zone == 2) {
            # tickets for 2nd zone and for 1st and 2nd zone
            zone_filter <- c(2, 3)
        } else {
            # tickets for 1st and 2nd zone
            zone_filter <- 3
        }
    }
    # selecting tickets form ztm_prices (tickets price list) based  on values of discount, zone_filter and varsovian_filter objects
    filter <- (ztm_prices$discount == discount & (ztm_prices$zone %in% zone_filter) & (ztm_prices$varsovian %in% varsovian_filter))
    # Calculating the number of tickets for a given travel time during one year -  number of tickets needed in one day multiplied by number of periods of time a given ticket type is valid
    tickets <- ceiling(travel_time/ztm_prices$time_min) * ceiling(1/ceiling(ztm_prices$time_min/1440)) # 1440 - number of minutes in one day
    # Calculating cost of tickets - number of tickets multiplied by price
    tickets_cost <- tickets * ztm_prices$price
    # exporting data on optimal ticket:
    ## ztm_ticket - optimal ticket characteristics
    ## number_of_tickets - number of optimal tickets
    ## tickets_cost - optimal ticket total cost
    return(list(ztm_ticket = ztm_prices[tickets_cost == min(tickets_cost[filter]) & filter, ], number_of_tickets = tickets[tickets_cost ==
        min(tickets_cost[filter]) & filter], tickets_cost = tickets_cost[tickets_cost == min(tickets_cost[filter]) & filter]))
}
#' Calculate minimal monthly cost of optimal ticket
#'
#' @description tickets_costs_monthly_ZTM chooses optimat tickets and calculates minimal monthly
#' cost of commuting by warsaw public transport.
#' @param travel_time numeric. Daily travel time in minutes.
#' @param discount ligical. Entitlement to a discount of 50\%.
#' @param zone numeric. Ticket zone 1, 2 or 3 (3 means zone 1 & 2)
#' @param varsovian logical. Entitlement to a Varsovian ticket discount
#' @return tickets_costs_monthly_ZTM returns a list containing following components:
#'   \item{ztm_ticket}{data.frame. Attributes of optimal ticket}
#'   \item{number_of_tickets}{Number of optimal tickets}
#'   \item{tickets_cost}{Minimal total daily cost of commuting for optimal tickets in PLN}
#' @examples
#' tickets_costs_monthly_ZTM(travel_time=60, discount=TRUE, zone=1, varsovian=TRUE)
#' @export
tickets_costs_monthly_ZTM <- function(travel_time, discount, zone, varsovian) {
    # Exception handling for travel_time argument
    if (length(travel_time) != 1) {
      stop("Error: argument travel_time is not a vector of length 1. Please enter travel_time as numeric vector of length 1.")
    }
    if (is.numeric(travel_time) == FALSE) {
      stop("Error: argument travel_time is not numeric. Please enter travel_time as numeric vector of length 1.")
    }
    if (travel_time <= 0 & travel_time > 1440) {
      stop("Error: argument travel_time is smaller than or equal to 0 or greater than 1440. Please enter numeric from range (0,1440].")
    }
    # Exception handling for discount argument
    if (length(discount) != 1) {
      stop("Error: argument discount is not a vector of length 1. Please enter TRUE or FALSE.")
    }
    if (is.logical(discount) == FALSE | is.na(discount)) {
      stop("Error: argument discount is not logical. Please enter TRUE or FALSE.")
    }
    # Exception handling for zone argument
    if (length(zone) != 1) {
      stop("Error: argument zone is not a vector of length 1. Please enter integer 1, 2 or 3")
    }
    if ( is.numeric(zone) == FALSE) {
      stop("Error: argument zone is not numeric. Please enter zone as integer 1, 2 or 3.")
    }
    if ((zone %in% 1:3) == FALSE) {
      stop("Error: argument zone is incorrect. Please enter integer 1, 2 or 3")
    }
    # Exception handling for varsovian argument
    if (length(varsovian) != 1) {
      stop("Error: argument varsovian is not a vector of length 1. Please enter TRUE or FALSE.")
    }
    if (is.logical(varsovian) == FALSE) {
      stop("Error: argument varsovian is not logical. Please enter TRUE or FALSE.")
    }
    # varsovian filter indicates what kind of tariff you are granted - regular (FALSE) or regular and varsovian (FALSE and TRUE)) )
    if (varsovian == FALSE) {
      # only regular tariff
      varsovian_filter <- FALSE
    } else {
      # regular and varsovian tariff
      varsovian_filter <- c(FALSE, TRUE)
    }
    # zone_filter indicates what kind of ticket you are allowed to use: 1st zone, 2nd zone or 1st and 2nd zone
    if (zone == 1) {
      # tickets for 1st and for 1st and 2nd zone
      zone_filter <- c(1, 3)
    } else {
      if (zone == 2) {
        # tickets for 2nd and for 1st and 2nd zone
        zone_filter <- c(2, 3)
      } else {
        # tickets for 1st and 2nd zone
        zone_filter <- 3
      }
    }
    # selecting tickets form ztm_prices (tickets price list) based  on values of discount, zone_filter and varsovian_filter objects
    filter <- (ztm_prices$discount == discount & (ztm_prices$zone %in% zone_filter) & (ztm_prices$varsovian %in% varsovian_filter))
    # Calculating the number of tickets for a given travel time during one year -  number of tickets needed in one day multiplied by number of periods of time a given ticket type is valid
    tickets <- ceiling(travel_time/ztm_prices$time_min) * ceiling(20/ceiling(ztm_prices$time_min/1440)) # 1440 - number of minutes in one day; 20 - number of working days in month
    # Calculating cost of tickets - number of tickets multiplied by price
    tickets_cost <- tickets * ztm_prices$price
    # exporting data on optimal ticket:
    ## ztm_ticket - optimal ticket characteristics
    ## number_of_tickets - number of optimal tickets
    ## tickets_cost - optimal ticket total cost
    return(list(ztm_ticket = ztm_prices[tickets_cost == min(tickets_cost[filter]) & filter, ], number_of_tickets = tickets[tickets_cost ==
        min(tickets_cost[filter]) & filter], tickets_cost = tickets_cost[tickets_cost == min(tickets_cost[filter]) & filter]))
}
#' Calculate minimal annual cost of optimal ticket
#'
#' @description tickets_costs_annual_ZTM chooses optimat tickets and calculates minimal annual
#' cost of commuting by warsaw public transport.
#' @param travel_time numeric. Daily travel time in minutes.
#' @param discount ligical. Entitlement to a discount of 50\%.
#' @param zone numeric. Ticket zone 1, 2 or 3 (3 means zone 1 & 2)
#' @param varsovian logical. Entitlement to a Varsovian ticket discount
#' @return tickets_costs_annual_ZTM returns a list containing following components:
#'   \item{ztm_ticket}{data.frame. Attributes of optimal ticket}
#'   \item{number_of_tickets}{Number of optimal tickets}
#'   \item{tickets_cost}{Minimal total annual cost of commuting for optimal tickets in PLN}
#' @examples
#' tickets_costs_annual_ZTM(travel_time=60, discount=TRUE, zone=1, varsovian=TRUE)
#' @export
tickets_costs_annual_ZTM <- function(travel_time, discount, zone, varsovian) {
    # Exception handling for travel_time argument
    if (length(travel_time) != 1) {
      stop("Error: argument travel_time is not a vector of length 1. Please enter travel_time as numeric vector of length 1.")
    }
    if (is.numeric(travel_time) == FALSE) {
      stop("Error: argument travel_time is not numeric. Please enter travel_time as numeric vector of length 1.")
    }
    if (travel_time <= 0 & travel_time > 1440) {
      stop("Error: argument travel_time is smaller than or equal to 0 or greater than 1440. Please enter numeric from range (0,1440].")
    }
    # Exception handling for discount argument
    if (length(discount) != 1 ) {
      stop("Error: argument discount is not a vector of length 1. Please enter TRUE or FALSE.")
    }
    if (is.logical(discount) == FALSE | is.na(discount)) {
      stop("Error: argument discount is not logical. Please enter TRUE or FALSE.")
    }
    # Exception handling for zone argument
    if (length(zone) != 1) {
      stop("Error: argument zone is not a vector of length 1. Please enter integer 1, 2 or 3")
    }
    if ( is.numeric(zone) == FALSE) {
      stop("Error: argument zone is not numeric. Please enter zone as integer 1, 2 or 3.")
    }
    if ((zone %in% 1:3) == FALSE) {
      stop("Error: argument zone is incorrect. Please enter integer 1, 2 or 3")
    }
    # Exception handling for varsovian argument
    if (length(varsovian) != 1) {
      stop("Error: argument varsovian is not a vector of length 1. Please enter TRUE or FALSE.")
    }
    if (is.logical(varsovian) == FALSE) {
      stop("Error: argument varsovian is not logical. Please enter TRUE or FALSE.")
    }
    # varsovian filter indicates what kind of tariff you are granted - regular (FALSE) or regular and varsovian (FALSE and TRUE)) )
    if (varsovian == FALSE) {
      # only regular tariff
      varsovian_filter <- FALSE
    } else {
      # regular and varsovian tariff
      varsovian_filter <- c(FALSE, TRUE)
    }
    # zone_filter indicates what kind of ticket you are allowed to use: 1st zone, 2nd zone or 1st and 2nd zone
    if (zone == 1) {
      # tickets for 1st and for 1st and 2nd zone
      zone_filter <- c(1, 3)
    } else {
      if (zone == 2) {
        # tickets for 2nd and for 1st and 2nd zone
        zone_filter <- c(2, 3)
      } else {
        # tickets for 1st and 2nd zone
        zone_filter <- 3
      }
    }
    # selecting tickets form ztm_prices (tickets price list) based  on values of discount, zone_filter and varsovian_filter objects
    filter <- (ztm_prices$discount == discount & (ztm_prices$zone %in% zone_filter) & (ztm_prices$varsovian %in% varsovian_filter))
    # Calculating the number of tickets for a given travel time during one year -  number of tickets needed in one day multiplied by number of periods of time a given ticket type is valid
    tickets <- ceiling(travel_time/ztm_prices$time_min) * ceiling(365/ceiling(ztm_prices$time_min/1440)) # 1440 - number of minutes in one day; 365 - number of days in year
    # Calculating cost of tickets - number of tickets multiplied by price
    tickets_cost <- tickets * ztm_prices$price
    # exporting data on optimal ticket:
    ## ztm_ticket - optimal ticket characteristics
    ## number_of_tickets - number of optimal tickets
    ## tickets_cost - optimal ticket total cost
    return(list(ztm_ticket = ztm_prices[tickets_cost == min(tickets_cost[filter]) & filter, ], number_of_tickets = tickets[tickets_cost ==
        min(tickets_cost[filter]) & filter], tickets_cost = tickets_cost[tickets_cost == min(tickets_cost[filter]) & filter]))
}
