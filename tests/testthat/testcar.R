library(WarsawTransport)
context("car_travel_cost")

test_that("car_travel_cost is number of characters", {
  expect_equal(car_travel_cost(START=, STOP=, combustion=1, fuel_price=1), 1)
  expect_equal(car_travel_cost("ab"), 2)
  expect_equal(car_travel_cost("abc"), 3)
})

test_that("car_travel_cost of factor is length of level", {
  expect_equal(car_travel_cost(factor("a")), 1)
  expect_equal(car_travel_cost(factor("ab")), 2)
  expect_equal(car_travel_cost(factor("abc")), 3)
})

test_that("car_travel_cost of missing is missing", {
  expect_equal(car_travel_cost(NA), NA_integer_)
  expect_equal(car_travel_cost(c(NA, 1)), c(NA, 1))
  expect_equal(car_travel_cost("NA"), 2)
})
