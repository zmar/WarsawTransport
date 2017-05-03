#' Calculate distans, amount of petrol, cost of petrol, cost of petrol for a given travel by car
#'
#' @param START character. An adress of starting point.
#' @param STOP character. An adresss of destination.
#' @return car_travel_cost returns a list containing following components
#' @examples
#' car_travel_cost(START="plac Defilad 1, 00-901 Warszawa Polska",
#' STOP="Stanisława Kostki Potockiego 10/16, Warszawa Polska",
#' combustion=8, fuel_price=4.5, travelers_number = 1)
car_travel_cost <- function(START, STOP, combustion, fuel_price, travelers_number = 1) {
    # obsluga wyjatkow START & STOP
    if (length(START) != 1 | length(STOP) != 1) {
        stop("START i STOP muszą być pojedynczymi wyrażeniami - wektorami klasy 'character' o długości 1")
    }
    if ((is.character(START) & is.character(STOP)) == FALSE) {
        stop("START i STOP muszą być tekstowe - muszą być klasy 'character'")
    }
    # obsluga wyjatkow travelers_number
    if (length(travelers_number) != 1) {
        stop("travelers_number musi być pojedyncza liczba - wektorek klasy 'numeric' o dlugosci 1")
    }
    if (is.numeric(travelers_number) == FALSE) {
        stop("travelers_number musi być liczba  klasy 'numeric'")
    }
    if (travelers_number < 0 | travelers_number > 10) {
        stop("travelers_number nie może być mniejsza niż 1 i większa niż 10")
    }
    if ((travelers_number%%1) > 0) {
        stop("travelers_number musi być liczbą całkowitą")
    }
    # obsluga wyjatkow combustion
    if (length(combustion) != 1) {
        stop("combustion musi byc pojedyncza liczba - wektorem klasy 'numeric' o dlugosic 1")
    }
    if (is.numeric(combustion) == FALSE) {
        stop("combustion musi być klasy 'numeric'")
    }
    if (combustion <= 0 | combustion > 15) {
        stop("combustion musi sie zawierac w przedziale (0,15]")
    }
    # obsluga wyjatkow fuel_price
    if (length(fuel_price) != 1) {
        stop("fuel_price musi byc pojedyncza liczba - wektorem klasy 'numeric' o dlugosic 1")
    }
    if (is.numeric(fuel_price) == FALSE) {
        stop("fuel_price musi byc klasy 'numeric'")
    }
    if (fuel_price <= 0 | fuel_price > 10) {
        stop("fuel_price musi byc z zakresu (0,10]")
    }
    FROM <- stringi::stri_enc_toutf8(START)
    TO <- stringi::stri_enc_toutf8(STOP)
    Google_Maps_Query <- ggmap::mapdist(FROM, TO, mode = "driving")
    benzyna <- Google_Maps_Query$km/100 * combustion
    koszt_benzyny <- fuel_price * benzyna
    koszt_benzyny_na_osobe <- fuel_price * benzyna/travelers_number
    return(list(distance = Google_Maps_Query$km, fuel_amount = benzyna, fuel_cost = koszt_benzyny, fuel_price_per_traveler = koszt_benzyny_na_osobe))
}
#' Calculate total car travel cost
#'
#' @param START character. An adress of starting point.
#' @param STOP character. An adresss of destination.
#' @return car_costs returns a cost of car travel pre traveler
#' @examples
#' car_travel_cost(START="plac Defilad 1, 00-901 Warszawa Polska",
#' STOP="Stanisława Kostki Potockiego 10/16, Warszawa Polska",
#' combustion=8, fuel_price=4.5, travelers_number = 1,
#' insurence_fee=1000, parking_time=8)

car_costs <- function(START, STOP, travelers_number, combustion, fuel_price, insurence_fee, parking_time) {
    if (length(insurence_fee) != 1) {
        stop("insurence_fee musi być pojedyncza liczba - wektorem klasy 'numeric' o dlugosic 1")
    }
    if (is.numeric(insurence_fee) == FALSE) {
        stop("insurence_fee musi być klasy 'numeric'")
    }
    if (insurence_fee <= 0 | insurence_fee > 15000) {
        stop("insurence_fee musi z zakresu (0,15 000]")
    }
    if (length(insurence_fee) != 1) {
        stop("insurence_fee musi być wektorem o długości 1 (skalar)")
    }
    car_total_cost <- car_travel_cost(START = START, STOP = STOP, travelers_number = travelers_number, combustion = combustion,
        fuel_price = fuel_price)$fuel_price_per_traveler + insurence_fee/365 + cost_of_parking(parking_time = parking_time)
    return(car_total_cost)
}
