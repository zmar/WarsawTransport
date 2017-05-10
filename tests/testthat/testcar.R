library(WarsawTransport)
context("car_travel_cost")

test_that("car_travel_cost correct results", {
  expect_equal(car_travel_cost(start_point = "Lublin", finish_point="Warszawa",fuel_consumption = 1, fuel_price = 1)$distance, 170.07)
  expect_equal(car_travel_cost(start_point = "Lublin", finish_point="Warszawa",fuel_consumption = 1, fuel_price = 1)$fuel_amount, 1.7007)
  expect_equal(car_travel_cost(start_point = "Lublin", finish_point="Warszawa",fuel_consumption = 1, fuel_price = 1)$fuel_cost, 1.7007)
  expect_equal(car_travel_cost(start_point = "Lublin", finish_point="Warszawa",fuel_consumption = 1, fuel_price = 1)$fuel_cost_per_traveler, 1.7007)
})

test_that("car_travel_cost incorrect start_point and finish_point results in NA", {
  expect_equal(car_travel_cost(start_point = "askjdhakjsd", finish_point="dasdd",fuel_consumption = 1, fuel_price = 1)$distance, NA_integer_)
  expect_equal(car_travel_cost(start_point = "askjdhakjsd", finish_point="dasdd",fuel_consumption = 1, fuel_price = 1)$fuel_amount, NA_integer_)
  expect_equal(car_travel_cost(start_point = "askjdhakjsd", finish_point="dasdd",fuel_consumption = 1, fuel_price = 1)$fuel_cost, NA_integer_)
  expect_equal(car_travel_cost(start_point = "askjdhakjsd", finish_point="dasdd",fuel_consumption = 1, fuel_price = 1)$fuel_cost_per_traveler, NA_integer_)
})

test_that("car_travel_cost incorrect input", {
  expect_error(car_travel_cost(start_point = "Lublin", finish_point="Warszawa",fuel_consumption = "1", fuel_price = 1)$distance)
  expect_error(car_travel_cost(start_point = "Lublin", finish_point="Warszawa",fuel_consumption = 1, fuel_price = "1")$fuel_amount)
  expect_error(car_travel_cost(start_point = "Lublin", finish_point="Warszawa",fuel_consumption = c(1,1), fuel_price = 1)$fuel_cost)
  expect_error(car_travel_cost(start_point = "Lublin", finish_point="Warszawa",fuel_consumption = 1, fuel_price = c(1,1))$fuel_cost_per_traveler)
})


context("car_costs")

test_that("car_costs correct results", {
  expect_equal(car_costs(start_point = "Lublin", finish_point="Warszawa",fuel_consumption = 1, fuel_price = 1,insurence_fee = 100,parking_time = 1,travelers_number = 1), 4.974673-3.97e-07)
})

test_that("car_costs incorrect results", {
  expect_error(car_costs(start_point = "Lublin", finish_point="Warszawa",fuel_consumption = c(1,1), fuel_price = 1,insurence_fee = 100,parking_time = 1,travelers_number = 1))
  expect_error(car_costs(start_point = "Lublin", finish_point="Warszawa",fuel_consumption = 1, fuel_price = c(1,1),insurence_fee = 100,parking_time = 1,travelers_number = 1))
  expect_error(car_costs(start_point = "Lublin", finish_point="Warszawa",fuel_consumption = 1, fuel_price = 1,insurence_fee = c(1,1),parking_time = 1,travelers_number = 1))
  expect_error(car_costs(start_point = "Lublin", finish_point="Warszawa",fuel_consumption = 1, fuel_price = 1,insurence_fee = 100,parking_time = c(1,1),travelers_number = 1))
  expect_error(car_costs(start_point = "Lublin", finish_point="Warszawa",fuel_consumption = 1, fuel_price = 1,insurence_fee = 100,parking_time = c(1,1),travelers_number = c(1,1)))
})
