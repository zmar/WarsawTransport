car_travel_cost <- function(START, STOP, combustion, cena_benzyny, travelers_number = 1) {
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
    # obsluga wyjatkow cena_benzyny
    if (length(cena_benzyny) != 1) {
        stop("cena_benzyny musi byc pojedyncza liczba - wektorem klasy 'numeric' o dlugosic 1")
    }
    if (is.numeric(cena_benzyny) == FALSE) {
        stop("cena_benzyny musi byc klasy 'numeric'")
    }
    if (cena_benzyny <= 0 | cena_benzyny > 10) {
        stop("cena_benzyny musi byc z zakresu (0,10]")
    }
    FROM <- stringi::stri_enc_toutf8(START)
    TO <- stringi::stri_enc_toutf8(STOP)
    Google_Maps_Query <- mapdist(FROM, TO, mode = "driving")
    benzyna <- Google_Maps_Query$km/100 * combustion
    koszt_benzyny <- cena_benzyny * benzyna
    koszt_benzyny_na_osobe <- cena_benzyny * benzyna/travelers_number
    # w dwie strony
    return(list(dystans = Google_Maps_Query$km, ilosc_benzyny = benzyna, koszt_benzyny = koszt_benzyny, koszt_benzyny_na_osobe = koszt_benzyny_na_osobe))
}


car_costs <- function(START, STOP, travelers_number, combustion, cena_benzyny, insurence_fee, parking_time) {
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
        cena_benzyny = cena_benzyny) + insurence_fee/365 + cost_of_parking(parking_time = parking_time)
    return(car_total_cost)
}
