#' Calculate minimal daily cost of optimal ticket
#'
#' @description tickets_costs_daily_ZTM chooses optimat tickets and calculates minimal daily cost of commuting by warsaw public transport.
#' @param travel_time numeric. Daily travel time in minutes.
#' @param discount ligical. Entitlement to a discount of 50\%.
#' @param zone numeric. Ticket zone 1, 2 or 3 - 3 means zone 1 and 2
#' @param warszawiak logical. Entitlement to a Varsovian ticket discount
#' @return tickets_costs_daily_ZTM returns a list containing following components
#'   \item{ztm_ticket}{data.frame. Attributes of optimal ticket}
#'   \item{number_of_tickets}{Number of optimal tickets}
#'   \item{tickets_cost}{Minimal total daily cost of commuting for optimal tickets}
#' @examples
#' tickets_costs_daily_ZTM(travel_time=60, discount=TRUE, zone=1, warszawiak=TRUE)
#' @export
tickets_costs_daily_ZTM <- function(travel_time, discount, zone, warszawiak) {
    # obsluga wyjatkow travel_time
    if (length(travel_time) != 1) {
        stop("travel_time musi byc pojedyncza liczba, czyli wektorem klasy 'numeric' o dlugosci 1")
    }
    if (is.numeric(travel_time) == FALSE) {
        stop("Błąd w czasie podróży. Podaj zmienną 'numeric' z przedziału (0,1440), czyli czas podróży w minutach, gdzie 1440 oznacza 24h")
    }
    if (travel_time <= 0 & travel_time > 1440) {
        stop("Błąd w zmiennej travel_time. Podaj liczbe z przedziału (0,1440], czyli czas podróży w minutach, gdzie 1440 oznacza 24h")
    }
    # obsluga wyjatkow discount
    if (length(discount) != 1) {
        stop("Błąd w zmiennej dicount. Podaj pojedyncza wartosc 'TRUE' lub 'FALSE'")
    }
    if (is.logical(discount) == FALSE) {
        stop("Błąd w zmiennej discount. Podaj wartość logiczna 'TRUE' lub 'FALSE'")
    }
    # obsluga wyjatkow zona
    if (length(zone) != 1) {
        stop("Błąd w zmiennej zone. Podaj pojedyncza wartosc 1, 2 lub 3")
    }
    if ((zone %in% 1:3) == FALSE) {
        stop("Błąd w zmiennej zone. Podaj pojedyncza wartość 1, 2 lub 3")
    }
    # obsluga wyjatkow warszawiak
    if (length(warszawiak) != 1) {
        stop("Błąd w zmiennej warszawiak. Podaj pojedyncza wartosc 'TRUE' lub 'FALSE'")
    }
    if (is.logical(warszawiak) == FALSE) {
        stop("Błąd w zmiennej warszawiaka. Podaj wartość 'TRUE' lub 'FALSE'")
    }
    # filtr ze wzgledu na przywileje warszawiaka
    if (warszawiak == FALSE) {
        warszawiak_filter <- FALSE
    } else {
        warszawiak_filter <- c(FALSE, TRUE)
    }
    # filtr ze wzgledu na strefe
    if (zone == 1) {
        zone_filter <- c(1, 3)
    } else {
        if (zone == 2) {
            zone_filter <- c(2, 3)
        } else {
            zone_filter <- 3
        }
    }
    # Filtr
    filter <- (ztm_prices$discount == discount & (ztm_prices$zone %in% zone_filter) & (ztm_prices$Warszawiak %in% warszawiak_filter))
    tickets <- ceiling(travel_time/ztm_prices$time_min) * ceiling(1/ceiling(ztm_prices$time_min/1440))
    # koszt biletow
    tickets_cost <- tickets * ztm_prices$price
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
#' @param warszawiak logical. Entitlement to a Varsovian ticket discount
#' @return tickets_costs_monthly_ZTM returns a list containing following components:
#'   \item{ztm_ticket}{data.frame. Attributes of optimal ticket}
#'   \item{number_of_tickets}{Number of optimal tickets}
#'   \item{tickets_cost}{Minimal total daily cost of commuting for optimal tickets}
#' @examples
#' tickets_costs_monthly_ZTM(travel_time=60, discount=TRUE, zone=1, warszawiak=TRUE)
#' @export
tickets_costs_monthly_ZTM <- function(travel_time, discount, zone, warszawiak) {
    # obsluga wyjatkow travel_time
    if (length(travel_time) != 1) {
        stop("travel_time musi byc pojedyncza liczba, czyli wektorem klasy 'numeric' o dlugosci 1")
    }
    if (is.numeric(travel_time) == FALSE) {
        stop("Błąd w czasie podróży. Podaj zmienną 'numeric' z przedziału (0,1440), czyli czas podróży w minutach, gdzie 1440 oznacza 24h")
    }
    if (travel_time <= 0 & travel_time > 1440) {
        stop("Błąd w zmiennej travel_time. Podaj liczbe z przedziału (0,1440], czyli czas podróży w minutach, gdzie 1440 oznacza 24h")
    }
    # obsluga wyjatkow discount
    if (length(discount) != 1) {
        stop("Błąd w zmiennej dicount. Podaj pojedyncza wartosc 'TRUE' lub 'FALSE'")
    }
    if (is.logical(discount) == FALSE) {
        stop("Błąd w zmiennej discount. Podaj wartość logiczna 'TRUE' lub 'FALSE'")
    }
    # obsluga wyjatkow zona
    if (length(zone) != 1) {
        stop("Błąd w zmiennej zone. Podaj pojedyncza wartosc 1, 2 lub 3")
    }
    if ((zone %in% 1:3) == FALSE) {
        stop("Błąd w zmiennej zone. Podaj pojedyncza wartość 1, 2 lub 3")
    }
    # obsluga wyjatkow warszawiak
    if (length(warszawiak) != 1) {
        stop("Błąd w zmiennej warszawiak. Podaj pojedyncza wartosc 'TRUE' lub 'FALSE'")
    }
    if (is.logical(warszawiak) == FALSE) {
        stop("Błąd w zmiennej warszawiaka. Podaj wartość 'TRUE' lub 'FALSE'")
    }
    # filtr ze wzgledu na przywileje warszawiaka
    if (warszawiak == FALSE) {
        warszawiak_filter <- FALSE
    } else {
        warszawiak_filter <- c(FALSE, TRUE)
    }
    # filtr ze wzgledu na strefe
    if (zone == 1) {
        zone_filter <- c(1, 3)
    } else {
        if (zone == 2) {
            zone_filter <- c(2, 3)
        } else {
            zone_filter <- 3
        }
    }
    # Filtr
    filter <- (ztm_prices$discount == discount & (ztm_prices$zone %in% zone_filter) & (ztm_prices$Warszawiak %in% warszawiak_filter))
    tickets <- ceiling(travel_time/ztm_prices$time_min) * ceiling(20/ceiling(ztm_prices$time_min/1440))
    # koszt biletow
    tickets_cost <- tickets * ztm_prices$price
    return(list(ztm_ticket = ztm_prices[tickets_cost == min(tickets_cost[filter]) & filter, ], number_of_tickets = tickets[tickets_cost ==
        min(tickets_cost[filter]) & filter], tickets_cost = tickets_cost[tickets_cost == min(tickets_cost[filter]) & filter]))
}
#' Calculate minimal annual cost of optimal ticket
#'
#' @description tickets_costs_annual_ZTM chooses optimat tickets and calculates minimal monthly
#' cost of commuting by warsaw public transport.
#' @param travel_time numeric. Daily travel time in minutes.
#' @param discount ligical. Entitlement to a discount of 50\%.
#' @param zone numeric. Ticket zone 1, 2 or 3 (3 means zone 1 & 2)
#' @param warszawiak logical. Entitlement to a Varsovian ticket discount
#' @return tickets_costs_annual_ZTM returns a list containing following components:
#'   \item{ztm_ticket}{data.frame. Attributes of optimal ticket}
#'   \item{number_of_tickets}{Number of optimal tickets}
#'   \item{tickets_cost}{Minimal total annual cost of commuting for optimal tickets}
#' @examples
#' tickets_costs_annual_ZTM(travel_time=60, discount=TRUE, zone=1, warszawiak=TRUE)
#' @export
tickets_costs_annual_ZTM <- function(travel_time, discount, zone, warszawiak) {
    # obsluga wyjatkow travel_time
    if (length(travel_time) != 1) {
        stop("travel_time musi byc pojedyncza liczba, czyli wektorem klasy 'numeric' o dlugosci 1")
    }
    if (is.numeric(travel_time) == FALSE) {
        stop("Błąd w czasie podróży. Podaj zmienną 'numeric' z przedziału (0,1440), czyli czas podróży w minutach, gdzie 1440 oznacza 24h")
    }
    if (travel_time <= 0 & travel_time > 1440) {
        stop("Błąd w zmiennej travel_time. Podaj liczbe z przedziału (0,1440], czyli czas podróży w minutach, gdzie 1440 oznacza 24h")
    }
    # obsluga wyjatkow discount
    if (length(discount) != 1) {
        stop("Błąd w zmiennej dicount. Podaj pojedyncza wartosc 'TRUE' lub 'FALSE'")
    }
    if (is.logical(discount) == FALSE) {
        stop("Błąd w zmiennej discount. Podaj wartość logiczna 'TRUE' lub 'FALSE'")
    }
    # obsluga wyjatkow zona
    if (length(zone) != 1) {
        stop("Błąd w zmiennej zone. Podaj pojedyncza wartosc 1, 2 lub 3")
    }
    if ((zone %in% 1:3) == FALSE) {
        stop("Błąd w zmiennej zone. Podaj pojedyncza wartość 1, 2 lub 3")
    }
    # obsluga wyjatkow warszawiak
    if (length(warszawiak) != 1) {
        stop("Błąd w zmiennej warszawiak. Podaj pojedyncza wartosc 'TRUE' lub 'FALSE'")
    }
    if (is.logical(warszawiak) == FALSE) {
        stop("Błąd w zmiennej warszawiaka. Podaj wartość 'TRUE' lub 'FALSE'")
    }
    # filtr ze wzgledu na przywileje warszawiaka
    if (warszawiak == FALSE) {
        warszawiak_filter <- FALSE
    } else {
        warszawiak_filter <- c(FALSE, TRUE)
    }
    # filtr ze wzgledu na strefe
    if (zone == 1) {
        zone_filter <- c(1, 3)
    } else {
        if (zone == 2) {
            zone_filter <- c(2, 3)
        } else {
            zone_filter <- 3
        }
    }
    # Filtr
    filter <- (ztm_prices$discount == discount & (ztm_prices$zone %in% zone_filter) & (ztm_prices$Warszawiak %in% warszawiak_filter))
    tickets <- ceiling(travel_time/ztm_prices$time_min) * ceiling(365/ceiling(ztm_prices$time_min/1440))
    # koszt biletow
    tickets_cost <- tickets * ztm_prices$price
    return(list(ztm_ticket = ztm_prices[tickets_cost == min(tickets_cost[filter]) & filter, ], number_of_tickets = tickets[tickets_cost ==
        min(tickets_cost[filter]) & filter], tickets_cost = tickets_cost[tickets_cost == min(tickets_cost[filter]) & filter]))
}
