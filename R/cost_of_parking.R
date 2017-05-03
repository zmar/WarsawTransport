#' @export
cost_of_parking <- function(parking_time) {
    if (length(parking_time) != 1) {
        stop("Cza parkowania musi być liczbą, czyli wektorem klasy 'numeric' o długości 1 (skalarem).")
    }
    if (is.numeric(parking_time) == FALSE) {
        stop("Podaj czas parkowania liczbowo w godzinach np. półtorej godziny to 1.5 - klasa obiektu  'numeric'")
    }
    if (parking_time > 24 | parking_time < 0) {
        stop("Czas parkowania musi zawierać się w przedziale [0,24] godzin.")
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
