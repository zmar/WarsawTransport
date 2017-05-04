#' Calculate cost for parking
#'
#' @description cost_of_parking calculates cost of parking in the paid parking zone in Warsaw.
#' @param parking_time numeric. Parking time in hours.
#' @return car_travel_cost returns daily cost of parking in the paid parking zone in Warsaw.
#' @examples
#' cost_of_parking(parking_time=3)
#' @export
cost_of_parking <- function(parking_time) {
    if (length(parking_time) != 1) {
        stop("Czas parkowania musi byc liczba, czyli wektorem klasy 'numeric' o dlugosci 1 (skalarem).")
    }
    if (is.numeric(parking_time) == FALSE) {
        stop("Podaj czas parkowania liczbowo w godzinach np. poltorej godziny to 1.5 - klasa obiektu  'numeric'")
    }
    if (parking_time > 24 | parking_time < 0) {
        stop("Czas parkowania musi zawierac sis w przedziale [0,24] godzin.")
    }
    minutes <- parking_time%%1
    hours <- floor(parking_time)
    time <- 0:4 < hours + ceiling(minutes)
    tariff <- c(3, 3.6, 4.2, 3)
    if (length(tariff) < hours + ceiling(minutes)) {
        tariff <- c(tariff, rep(tariff[4], hours + ceiling(minutes) - length(tariff)))
    } else {
        tariff <- c(tariff[1:(hours + ceiling(minutes))])
    }
    if (minutes > 0) {
        cost <- tariff * c(rep(1, hours), minutes)
    } else {
        cost <- tariff * rep(1, hours)
    }
    cost <- sum(cost)
    return(cost)
}
