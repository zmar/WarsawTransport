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
