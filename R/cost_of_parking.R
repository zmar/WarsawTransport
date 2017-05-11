#' Calculate cost for parking
#'
#' @description cost_of_parking calculates cost of parking in the paid parking zone in Warsaw.
#' @param parking_time numeric. Parking time in hours.
#' @return car_travel_cost returns daily cost of parking in the paid parking zone in Warsaw.
#' @examples
#' cost_of_parking(parking_time=3)
#' @export
cost_of_parking <- function(parking_time) {
    # Exception handling for parking_time argument
    if (length(parking_time) != 1) {
        stop("Error: argument parking_time is not numeric. Please enter parking_time as numeric vectors of length 1.")
    }
    if (is.numeric(parking_time) == FALSE) {
        stop("Error: argument parking_time is not numeric. Please enter parking_time as numeric vectors of length 1.")
    }
    if (parking_time > 24 | parking_time < 0) {
        stop("Error: argument parking_time is smaller than 0 or greater than 24. Please enter numeric from range [0,24].")
    }
    # extracting minutes form parking time
    minutes <- parking_time%%1
    # extracting hours  form parking time
    hours <- floor(parking_time)
    # parking time - logical indicator of length of parking time: 1h, 2h, 3h or 4 and every next hour
    time <- 0:4 < (hours + ceiling(minutes))
    # parking tariff - prices (in PLN) for 1st, 2nd, 3th or 4th and each subsequent hour
    tariff <- c(3, 3.6, 4.2, 3)
    # if parking time < 5 hours
    if (length(tariff) < (hours + ceiling(minutes))) {
        # Shortening the tariff to the length of parking time
        tariff <- c(tariff, rep(tariff[4], hours + ceiling(minutes) - length(tariff)))
    } else {
    # if parking time >= 5 hours
        # Extending the tariff to the length of parking time
        tariff <- c(tariff[1:(hours + ceiling(minutes))])
    }
    # calculation cost of each hour of parking
    if (minutes > 0) {
        # if minutes > 0 - calculating cost of each hour of parking by multiplicating tariff vector by vector reprezenting each full hours by 1 and each started hour by minutes object
        cost <- tariff * c(rep(1, hours), minutes)
    } else {
        # if minutes <=0 - calculating cost of each hour of parking by multiplicating tariff vector by vector reprezenting each full hours by 1 and each started hour by minutes object
        cost <- tariff * rep(1, hours)
    }
    # adding costs together
    cost <- sum(cost)
    return(cost)
}
