library(WarsawTransport)
context("cost_of_parking")

test_that("cost_of_parking correct results", {
  expect_equal(cost_of_parking(1), 3)
})

test_that("cost_of_parking incorrect input", {
  expect_error(cost_of_parking(25))
  expect_error(cost_of_parking(-1))
  expect_error(cost_of_parking("1"))
})

