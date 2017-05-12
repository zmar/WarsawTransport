library(WarsawTransport)
context("cost_of_parking")

test_that("parking_time - correct input", {
  # parking_time equel to 1
  expect_equal(cost_of_parking(parking_time = 1), 3)
  # the smallest valid value of the parking_time - 0
  expect_equal(cost_of_parking(parking_time = 0), 0)
  # the greatest valid value of the parking_time - 24
  expect_equal(cost_of_parking(parking_time = 24), 73.8)
})

test_that("cost_of_parking - handling errors", {
  # parking_time beyound the greatest valid value
  expect_error(cost_of_parking(parking_time = 25),
               regexp = "Error: argument parking_time is smaller than 0 or greater than 24. Please enter numeric from range \\[0,24\\].")
  # parking_time below the smallest valid value
  expect_error(cost_of_parking(parking_time = -1),
               regexp = "Error: argument parking_time is smaller than 0 or greater than 24. Please enter numeric from range \\[0,24\\].")
  # parking_time as character
  expect_error(cost_of_parking(parking_time = "1"),
               regexp = "Error: argument parking_time is not numeric. Please enter parking_time as numeric vectors of length 1.")
  # parking_time as vector of length 2
  expect_error(cost_of_parking(parking_time = c(1, 2)),
               regexp = "Error: argument parking_time is not numeric. Please enter parking_time as numeric vectors of length 1.")
})

