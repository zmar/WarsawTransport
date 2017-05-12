library(WarsawTransport)

####################################### car_travel_cost ######################################

context("car_travel_cost")

test_that("car_travel_cost - correct input", {
  # correct input - results of distance, fuel_amount, fuel_cost, fuel_cost_per_traveler distance results is a numeric object
  expect_equal(
    car_travel_cost(
      start_point = "Lublin",
      finish_point = "Warszawa",
      fuel_consumption = 1,
      fuel_price = 1
    )$distance,
    170.07
  )
  # fuel_amount results is a numeric object
  expect_equal(
    car_travel_cost(
      start_point = "Lublin",
      finish_point = "Warszawa",
      fuel_consumption = 1,
      fuel_price = 1
    )$fuel_amount,
    1.7007
  )
  # fuel_cost results is a numeric object
  expect_equal(
    car_travel_cost(
      start_point = "Lublin",
      finish_point = "Warszawa",
      fuel_consumption = 1,
      fuel_price = 1
    )$fuel_cost,
    1.7007
  )
  # fuel_cost_per_traveler results is a numeric object
  expect_equal(
    car_travel_cost(
      start_point = "Lublin",
      finish_point = "Warszawa",
      fuel_consumption = 1,
      fuel_price = 1
    )$fuel_cost_per_traveler,
    1.7007
  )
})


test_that("car_travel_cost errors - value distance - handling incorect character input",
          {
            # distance input - start_point and finish_point as incorect character
            expect_equal(
              car_travel_cost(
                start_point = "askjdhakjsd",
                finish_point = "dasdd",
                fuel_consumption = 1,
                fuel_price = 1
              )$distance,
              NA_integer_
            )
            ## input - fuel_consumption as character
            expect_error(
              car_travel_cost(
                start_point = "Lublin",
                finish_point = "Warszawa",
                fuel_consumption = "1",
                fuel_price = 1
              )$distance,
              regexp = "Error: argument fuel_consumption is not numeric. Please enter fuel_consumption as numeric vector of length 1"
            )
            ## input - fuel_price as character
            expect_error(
              car_travel_cost(
                start_point = "Lublin",
                finish_point = "Warszawa",
                fuel_consumption = 1,
                fuel_price = "1"
              )$distance,
              regexp = "Error: argument fuel_price is not numeric. Please enter fuel_price as numeric vector of length 1"
            )
          })
test_that("car_travel_cost errors - value distance - handling NA input", {
  # distance input - start_point and finish_point as NA
  expect_error(
    car_travel_cost(
      start_point = NA,
      finish_point = NA,
      fuel_consumption = 1,
      fuel_price = 1
    )$distance,
    regexp = "Error: argument start_point or finish_point is not character. Please enter start_point and finish_point as character vectors of length 1"
  )
  ## input - fuel_consumption as NA
  expect_error(
    car_travel_cost(
      start_point = "Lublin",
      finish_point = "Warszawa",
      fuel_consumption = NA,
      fuel_price = 1
    )$distance,
    regexp = "Error: argument fuel_consumption is not numeric. Please enter fuel_consumption as numeric vector of length 1"
  )
  ## input - fuel_price as NA
  expect_error(
    car_travel_cost(
      start_point = "Lublin",
      finish_point = "Warszawa",
      fuel_consumption = 1,
      fuel_price = NA
    )$distance,
    regexp = "Error: argument fuel_price is not numeric. Please enter fuel_price as numeric vector of length 1"
  )
})
test_that("car_travel_cost errors - value distance - handling input vector of length 2",
          {
            # distance input - start_point and finish_point as c('Lublin','Lublin') andc('Warszawa','Warszawa')
            expect_error(
              car_travel_cost(
                start_point = c("Lublin", "Lublin"),
                finish_point = c("Warszawa", "Warszawa"),
                fuel_consumption = 1,
                fuel_price = 1
              )$distance,
              regexp = "Error: argument start_point or finish_point is not character. Please enter start_point and finish_point as character vectors of length 1"
            )
            ## input - fuel_consumption as c(1,1)
            expect_error(
              car_travel_cost(
                start_point = "Lublin",
                finish_point = "Warszawa",
                fuel_consumption = c(1, 1),
                fuel_price = 1
              )$distance,
              regexp = "Error: argument fuel_consumption is not numeric. Please enter fuel_consumption as numeric vector of length 1"
            )
            ## input - fuel_price as c(1,1)
            expect_error(
              car_travel_cost(
                start_point = "Lublin",
                finish_point = "Warszawa",
                fuel_consumption = 1,
                fuel_price = c(1, 1)
              )$distance,
              regexp = "Error: argument fuel_price is not numeric. Please enter fuel_price as numeric vector of length 1"
            )
          })


test_that("car_travel_cost errors - value fuel_amount - handling incorect character input",
          {
            # fuel_amount input - start_point and finish_point as incorect character
            expect_equal(
              car_travel_cost(
                start_point = "askjdhakjsd",
                finish_point = "dasdd",
                fuel_consumption = 1,
                fuel_price = 1
              )$fuel_amount,
              NA_integer_
            )
            ## input - fuel_consumption as character
            expect_error(
              car_travel_cost(
                start_point = "Lublin",
                finish_point = "Warszawa",
                fuel_consumption = "1",
                fuel_price = 1
              )$fuel_amount,
              regexp = "Error: argument fuel_consumption is not numeric. Please enter fuel_consumption as numeric vector of length 1"
            )
            ## input - fuel_price as character
            expect_error(
              car_travel_cost(
                start_point = "Lublin",
                finish_point = "Warszawa",
                fuel_consumption = 1,
                fuel_price = "1"
              )$fuel_amount,
              regexp = "Error: argument fuel_price is not numeric. Please enter fuel_price as numeric vector of length 1"
            )
          })
test_that("car_travel_cost errors - value fuel_amount - handling NA input", {
  # fuel_amount input - start_point and finish_point as NA
  expect_error(
    car_travel_cost(
      start_point = NA,
      finish_point = NA,
      fuel_consumption = 1,
      fuel_price = 1
    )$fuel_amount,
    regexp = "Error: argument start_point or finish_point is not character. Please enter start_point and finish_point as character vectors of length 1"
  )
  ## input - fuel_consumption as NA
  expect_error(
    car_travel_cost(
      start_point = "Lublin",
      finish_point = "Warszawa",
      fuel_consumption = NA,
      fuel_price = 1
    )$fuel_amount,
    regexp = "Error: argument fuel_consumption is not numeric. Please enter fuel_consumption as numeric vector of length 1"
  )
  ## input - fuel_price as NA
  expect_error(
    car_travel_cost(
      start_point = "Lublin",
      finish_point = "Warszawa",
      fuel_consumption = 1,
      fuel_price = NA
    )$fuel_amount,
    regexp = "Error: argument fuel_price is not numeric. Please enter fuel_price as numeric vector of length 1"
  )
})
test_that("car_travel_cost errors - value fuel_amount - handling input vector of length 2",
          {
            # fuel_amount input - start_point and finish_point as c('Lublin','Lublin') andc('Warszawa','Warszawa')
            expect_error(
              car_travel_cost(
                start_point = c("Lublin", "Lublin"),
                finish_point = c("Warszawa", "Warszawa"),
                fuel_consumption = 1,
                fuel_price = 1
              )$fuel_amount,
              regexp = "Error: argument start_point or finish_point is not character. Please enter start_point and finish_point as character vectors of length 1"
            )
            ## input - fuel_consumption as c(1,1)
            expect_error(
              car_travel_cost(
                start_point = "Lublin",
                finish_point = "Warszawa",
                fuel_consumption = c(1, 1),
                fuel_price = 1
              )$fuel_amount,
              regexp = "Error: argument fuel_consumption is not numeric. Please enter fuel_consumption as numeric vector of length 1"
            )
            ## input - fuel_price as c(1,1)
            expect_error(
              car_travel_cost(
                start_point = "Lublin",
                finish_point = "Warszawa",
                fuel_consumption = 1,
                fuel_price = c(1, 1)
              )$fuel_amount,
              regexp = "Error: argument fuel_price is not numeric. Please enter fuel_price as numeric vector of length 1"
            )
          })


test_that("car_travel_cost errors - value fuel_cost - handling incorect character input",
          {
            # fuel_cost input - start_point and finish_point as incorect character
            expect_equal(
              car_travel_cost(
                start_point = "askjdhakjsd",
                finish_point = "dasdd",
                fuel_consumption = 1,
                fuel_price = 1
              )$fuel_cost,
              NA_integer_
            )
            ## input - fuel_consumption as character
            expect_error(
              car_travel_cost(
                start_point = "Lublin",
                finish_point = "Warszawa",
                fuel_consumption = "1",
                fuel_price = 1
              )$fuel_cost,
              regexp = "Error: argument fuel_consumption is not numeric. Please enter fuel_consumption as numeric vector of length 1"
            )
            ## input - fuel_price as character
            expect_error(
              car_travel_cost(
                start_point = "Lublin",
                finish_point = "Warszawa",
                fuel_consumption = 1,
                fuel_price = "1"
              )$fuel_cost,
              regexp = "Error: argument fuel_price is not numeric. Please enter fuel_price as numeric vector of length 1"
            )
          })
test_that("car_travel_cost errors - value fuel_cost - handling NA input", {
  # fuel_cost input - start_point and finish_point as NA
  expect_error(
    car_travel_cost(
      start_point = NA,
      finish_point = NA,
      fuel_consumption = 1,
      fuel_price = 1
    )$fuel_cost,
    regexp = "Error: argument start_point or finish_point is not character. Please enter start_point and finish_point as character vectors of length 1"
  )
  ## input - fuel_consumption as NA
  expect_error(
    car_travel_cost(
      start_point = "Lublin",
      finish_point = "Warszawa",
      fuel_consumption = NA,
      fuel_price = 1
    )$fuel_cost,
    regexp = "Error: argument fuel_consumption is not numeric. Please enter fuel_consumption as numeric vector of length 1"
  )
  ## input - fuel_price as NA
  expect_error(
    car_travel_cost(
      start_point = "Lublin",
      finish_point = "Warszawa",
      fuel_consumption = 1,
      fuel_price = NA
    )$fuel_cost,
    regexp = "Error: argument fuel_price is not numeric. Please enter fuel_price as numeric vector of length 1"
  )
})
test_that("car_travel_cost errors - value fuel_cost - handling input vector of length 2",
          {
            # fuel_cost input - start_point and finish_point as c('Lublin','Lublin') andc('Warszawa','Warszawa')
            expect_error(
              car_travel_cost(
                start_point = c("Lublin", "Lublin"),
                finish_point = c("Warszawa", "Warszawa"),
                fuel_consumption = 1,
                fuel_price = 1
              )$fuel_cost,
              regexp = "Error: argument start_point or finish_point is not character. Please enter start_point and finish_point as character vectors of length 1"
            )
            ## input - fuel_consumption as c(1,1)
            expect_error(
              car_travel_cost(
                start_point = "Lublin",
                finish_point = "Warszawa",
                fuel_consumption = c(1, 1),
                fuel_price = 1
              )$fuel_cost,
              regexp = "Error: argument fuel_consumption is not numeric. Please enter fuel_consumption as numeric vector of length 1"
            )
            ## input - fuel_price as c(1,1)
            expect_error(
              car_travel_cost(
                start_point = "Lublin",
                finish_point = "Warszawa",
                fuel_consumption = 1,
                fuel_price = c(1, 1)
              )$fuel_cost,
              regexp = "Error: argument fuel_price is not numeric. Please enter fuel_price as numeric vector of length 1"
            )
          })


test_that(
  "car_travel_cost errors - value fuel_cost_per_traveler - handling incorect character input",
  {
    # fuel_cost_per_traveler input - start_point and finish_point as incorect character
    expect_equal(
      car_travel_cost(
        start_point = "askjdhakjsd",
        finish_point = "dasdd",
        fuel_consumption = 1,
        fuel_price = 1
      )$fuel_cost_per_traveler,
      NA_integer_
    )
    ## input - fuel_consumption as character
    expect_error(
      car_travel_cost(
        start_point = "Lublin",
        finish_point = "Warszawa",
        fuel_consumption = "1",
        fuel_price = 1
      )$fuel_cost_per_traveler,
      regexp = "Error: argument fuel_consumption is not numeric. Please enter fuel_consumption as numeric vector of length 1"
    )
    ## input - fuel_price as character
    expect_error(
      car_travel_cost(
        start_point = "Lublin",
        finish_point = "Warszawa",
        fuel_consumption = 1,
        fuel_price = "1"
      )$fuel_cost_per_traveler,
      regexp = "Error: argument fuel_price is not numeric. Please enter fuel_price as numeric vector of length 1"
    )
  }
)
test_that("car_travel_cost errors - value fuel_cost_per_traveler - handling NA input",
          {
            # fuel_cost_per_traveler input - start_point and finish_point as NA
            expect_error(
              car_travel_cost(
                start_point = NA,
                finish_point = NA,
                fuel_consumption = 1,
                fuel_price = 1
              )$fuel_cost_per_traveler,
              regexp = "Error: argument start_point or finish_point is not character. Please enter start_point and finish_point as character vectors of length 1"
            )
            ## input - fuel_consumption as NA
            expect_error(
              car_travel_cost(
                start_point = "Lublin",
                finish_point = "Warszawa",
                fuel_consumption = NA,
                fuel_price = 1
              )$fuel_cost_per_traveler,
              regexp = "Error: argument fuel_consumption is not numeric. Please enter fuel_consumption as numeric vector of length 1"
            )
            ## input - fuel_price as NA
            expect_error(
              car_travel_cost(
                start_point = "Lublin",
                finish_point = "Warszawa",
                fuel_consumption = 1,
                fuel_price = NA
              )$fuel_cost_per_traveler,
              regexp = "Error: argument fuel_price is not numeric. Please enter fuel_price as numeric vector of length 1"
            )
          })
test_that(
  "car_travel_cost errors - value fuel_cost_per_traveler - handling input vector of length 2",
  {
    # fuel_cost_per_traveler input - start_point and finish_point as c('Lublin','Lublin') andc('Warszawa','Warszawa')
    expect_error(
      car_travel_cost(
        start_point = c("Lublin", "Lublin"),
        finish_point = c("Warszawa", "Warszawa"),
        fuel_consumption = 1,
        fuel_price = 1
      )$fuel_cost_per_traveler,
      regexp = "Error: argument start_point or finish_point is not character. Please enter start_point and finish_point as character vectors of length 1"
    )
    ## input - fuel_consumption as c(1,1)
    expect_error(
      car_travel_cost(
        start_point = "Lublin",
        finish_point = "Warszawa",
        fuel_consumption = c(1, 1),
        fuel_price = 1
      )$fuel_cost_per_traveler,
      regexp = "Error: argument fuel_consumption is not numeric. Please enter fuel_consumption as numeric vector of length 1"
    )
    ## input - fuel_price as c(1,1)
    expect_error(
      car_travel_cost(
        start_point = "Lublin",
        finish_point = "Warszawa",
        fuel_consumption = 1,
        fuel_price = c(1, 1)
      )$fuel_cost_per_traveler,
      regexp = "Error: argument fuel_price is not numeric. Please enter fuel_price as numeric vector of length 1"
    )
  }
)

####################################### car_costs ######################################

context("car_costs")

test_that("car_costs correct results", {
  # output as numeric
  expect_equal(
    car_costs(
      start_point = "Lublin",
      finish_point = "Warszawa",
      fuel_consumption = 1,
      fuel_price = 1,
      insurence_fee = 100,
      parking_time = 1,
      travelers_number = 1
    ),
    4.974673 - 3.97e-07
  )
})

test_that("car_costs errors - handling character input", {
  ## input - insurence_feet as character
  expect_error(
    car_costs(
      start_point = "Lublin",
      finish_point = "Warszawa",
      fuel_consumption = 1,
      fuel_price = 1,
      insurence_fee = "100",
      parking_time = 1,
      travelers_number = 1
    ),
    regexp = "Error: argument insurence_fee is not numeric. Please enter insurence_fee as numeric vector of length 1"
  )
  ## input - parking_time as character
  expect_error(
    car_costs(
      start_point = "Lublin",
      finish_point = "Warszawa",
      fuel_consumption = 1,
      fuel_price = 1,
      insurence_fee = 100,
      parking_time = "1",
      travelers_number = 1
    ),
    regexp = "Error: argument parking_time is not numeric. Please enter parking_time as numeric vectors of length 1."
  )
  ## input - travelers_number as character
  expect_error(
    car_costs(
      start_point = "Lublin",
      finish_point = "Warszawa",
      fuel_consumption = 1,
      fuel_price = 1,
      insurence_fee = 100,
      parking_time = 1,
      travelers_number = "1"
    ),
    regexp = "Error: argument travelers_number is not numeric. Please enter travelers_number as numeric vector of length 1"
  )
})

test_that("car_costs errors - handling NA input", {
  ## input - insurence_feet as NA
  expect_error(
    car_costs(
      start_point = "Lublin",
      finish_point = "Warszawa",
      fuel_consumption = 1,
      fuel_price = 1,
      insurence_fee = NA,
      parking_time = 1,
      travelers_number = 1
    ),
    regexp = "Error: argument insurence_fee is not numeric. Please enter insurence_fee as numeric vector of length 1"
  )
  ## input - parking_time as NA
  expect_error(
    car_costs(
      start_point = "Lublin",
      finish_point = "Warszawa",
      fuel_consumption = 1,
      fuel_price = 1,
      insurence_fee = 100,
      parking_time = NA,
      travelers_number = 1
    ),
    regexp = "Error: argument parking_time is not numeric. Please enter parking_time as numeric vectors of length 1."
  )
  ## input - travelers_number as NA
  expect_error(
    car_costs(
      start_point = "Lublin",
      finish_point = "Warszawa",
      fuel_consumption = 1,
      fuel_price = 1,
      insurence_fee = 100,
      parking_time = 1,
      travelers_number = NA
    ),
    regexp = "Error: argument travelers_number is not numeric. Please enter travelers_number as numeric vector of length 1"
  )
})

test_that("car_costs errors - handling input vector of length 2", {
  ## input - insurence_feet as c(100, 101)
  expect_error(
    car_costs(
      start_point = "Lublin",
      finish_point = "Warszawa",
      fuel_consumption = 1,
      fuel_price = 1,
      insurence_fee = c(100, 101),
      parking_time = 1,
      travelers_number = 1
    ),
    regexp = "Error: argument insurence_fee is not numeric. Please enter insurence_fee as numeric vector of length 1"
  )
  ## input - parking_time as c(1,1)
  expect_error(
    car_costs(
      start_point = "Lublin",
      finish_point = "Warszawa",
      fuel_consumption = 1,
      fuel_price = 1,
      insurence_fee = 100,
      parking_time = c(1, 1),
      travelers_number = 1
    ),
    regexp = "Error: argument parking_time is not numeric. Please enter parking_time as numeric vectors of length 1."
  )
  ## input - travelers_number as c(1,1)
  expect_error(
    car_costs(
      start_point = "Lublin",
      finish_point = "Warszawa",
      fuel_consumption = 1,
      fuel_price = 1,
      insurence_fee = 100,
      parking_time = 1,
      travelers_number = c(1,1)
    ),
    regexp = "Error: argument travelers_number is not numeric. Please enter travelers_number as numeric vector of length 1"
  )
})
