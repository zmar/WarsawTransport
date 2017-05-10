#' Calculates cost for a given travel by car
#'
#' @description car_travel_cost calculates distans, amount of petrol, cost of petrol, cost of petrol for a given travel by car
#' @param start_point character. An address of start_pointing point.
#' @param finish_point character. An address of destination.
#' @param fuel_consumption  numeric. Fuel consumption expressed as liters per 100 kilometers (L/100 km)
#' @param fuel_price numeric. Fuel price in PLN
#' @param travelers_number numeric. Number of travelers. Default value is 1.
#' @return car_travel_cost returns a list containing following components:
#'   \item{distance}{distance of travel in km}
#'   \item{fuel_amount}{Amount of fuel in liters}
#'   \item{fuel_cost}{Amount of fuel in PLN}
#'   \item{fuel_cost_per_traveler}{Cost of fuel per traveler}
#' @examples
#' car_travel_cost(start_point="plac Defilad 1, 00-901 Warszawa Polska",
#' finish_point="plac Zamkowy 4 Warszawa Polska",
#' fuel_consumption=8, fuel_price=4.5, travelers_number = 1)
#' @export
car_travel_cost <- function(start_point, finish_point, fuel_consumption, fuel_price, travelers_number = 1) {
    # Exception handling for start_point and finish_point arguments
    if (length(start_point) != 1 | length(finish_point) != 1) {
      stop("Error: argument start_point or finish_point is not character. Please enter start_point and finish_point as character vectors length 1")
    }
    if ((is.character(start_point) & is.character(finish_point)) == FALSE) {
      stop("Error: argument start_point or finish_point is not character. Please enter start_point and finish_point as character vectors length 1")
    }
    # Exception handling for travelers_number argument
    if (length(travelers_number) != 1) {
      stop("Error: argument travelers_number is not numeric. Please enter travelers_number as numeric vector length 1")
    }
    if (is.numeric(travelers_number) == FALSE) {
      stop("Error: argument travelers_number is not numeric. Please enter travelers_number as numeric vector length 1")
    }
    if (travelers_number < 0 | travelers_number > 10) {
      stop("Error: argument travelers_number is . Please enter ...")
    }
    if ((travelers_number%%1) > 0) {
      stop("travelers_number musi być liczbą całkowitą")
    }
    # obsluga wyjatkow fuel_consumption
    if (length(fuel_consumption) != 1) {
      stop("fuel_consumption musi byc pojedyncza liczba - wektorem klasy 'numeric' o dlugosic 1")
    }
    if (is.numeric(fuel_consumption) == FALSE) {
      stop("fuel_consumption musi być klasy 'numeric'")
    }
    if (fuel_consumption <= 0 | fuel_consumption > 15) {
      stop("fuel_consumption musi sie zawierac w przedziale (0,15]")
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
    from <- stringi::stri_enc_toutf8(start_point)
    to <- stringi::stri_enc_toutf8(finish_point)
    Google_Maps_Query <- ggmap::mapdist(from, to, mode = "driving")
    petrol <- Google_Maps_Query$km/100 * fuel_consumption
    petrol_cost <- fuel_price * petrol
    petrol_cost_na_per_traveler <- fuel_price * petrol/travelers_number
    return(list(distance = Google_Maps_Query$km, fuel_amount = petrol, fuel_cost = petrol_cost, fuel_cost_per_traveler = petrol_cost_na_per_traveler))
}
#' Calculate total car travel cost
#'
#' @param start_point character. An address of start_pointing point.
#' @param finish_point character. An address of destination.
#' @param fuel_consumption  numeric. Fuel consumption expressed as liters per 100 kilometers (L/100 km)
#' @param fuel_price numeric. Fuel price in PLN
#' @param travelers_number numeric. Number of travelers. Default value is 1.
#' @param insurence_fee numeric. Annual car insurance cost in PLN.
#' @param parking_time numeric. Parking time in hours.
#' @return car_costs returns a cost of car travel per traveler in PLN
#' @examples
#' car_costs(start_point="plac Defilad 1, 00-901 Warszawa Polska",
#' finish_point="plac Zamkowy 4 Warszawa Polska",
#' fuel_consumption=8, fuel_price=4.5, travelers_number = 1,
#' insurence_fee=1000, parking_time=8)
#' @export
car_costs <- function(start_point, finish_point, travelers_number, fuel_consumption, fuel_price, insurence_fee, parking_time) {
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
    car_total_cost <- car_travel_cost(start_point = start_point, finish_point = finish_point, travelers_number = travelers_number, fuel_consumption = fuel_consumption,
        fuel_price = fuel_price)$fuel_cost_per_traveler + insurence_fee/365 + cost_of_parking(parking_time = parking_time)
    return(car_total_cost)
}
