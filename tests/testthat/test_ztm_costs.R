library(WarsawTransport)
context("tickets_costs_daily_ZTM")

test_that("tickets_costs_daily_ZTM - correct input", {
  # correct input - results of number_of_tickets, tickets_cost and ztm_ticket
  # number_of_tickets results is a numeric object
  expect_equal(tickets_costs_daily_ZTM(travel_time = 30, discount=TRUE, zone=1, varsovian=TRUE)$number_of_tickets, 1)
  # tickets_cost results ia a numeric object
  expect_equal(tickets_costs_daily_ZTM(travel_time = 30, discount=TRUE, zone=1, varsovian=TRUE)$tickets_cost, 2.2)
  # ztm_ticket results is a data.frame object
  expect_is(tickets_costs_daily_ZTM(travel_time = 30, discount=TRUE, zone=1, varsovian=TRUE)$ztm_ticket, "data.frame")
})

test_that("tickets_costs_daily_ZTM errors - handling character input for number_of_tickets", {
  # number_of_tickets output
  ##  input - travel_time as character
  expect_error(tickets_costs_daily_ZTM(travel_time = "30", discount=TRUE, zone=1, varsovian=TRUE)$number_of_tickets)
  ##  input - discount as character
  expect_error(tickets_costs_daily_ZTM(travel_time = 30, discount="TRUE", zone=1, varsovian=TRUE)$number_of_tickets)
  ##  input - zone as character
  expect_error(tickets_costs_daily_ZTM(travel_time = 30, discount=TRUE, zone="1", varsovian=TRUE)$number_of_tickets)
  ##  input - varsovian  as character
  expect_error(tickets_costs_daily_ZTM(travel_time = 30, discount=TRUE, zone=1, varsovian="TRUE")$number_of_tickets)
  expect_error(tickets_costs_daily_ZTM(travel_time = "30", discount=TRUE, zone=1, varsovian=TRUE)$tickets_cost)
  expect_error(tickets_costs_daily_ZTM(travel_time = 30, discount="TRUE", zone=1, varsovian=TRUE)$tickets_cost)
  expect_error(tickets_costs_daily_ZTM(travel_time = 30, discount=TRUE, zone="1", varsovian=TRUE)$tickets_cost)
  expect_error(tickets_costs_daily_ZTM(travel_time = 30, discount=TRUE, zone=1, varsovian="TRUE")$tickets_cost)
  expect_error(tickets_costs_daily_ZTM(travel_time = "30", discount=TRUE, zone=1, varsovian=TRUE)$ztm_ticket)
  expect_error(tickets_costs_daily_ZTM(travel_time = 30, discount="TRUE", zone=1, varsovian=TRUE)$ztm_ticket)
  expect_error(tickets_costs_daily_ZTM(travel_time = 30, discount=TRUE, zone="1", varsovian=TRUE)$ztm_ticket)
  expect_error(tickets_costs_daily_ZTM(travel_time = 30, discount=TRUE, zone=1, varsovian="TRUE")$ztm_ticket)
})


context("tickets_costs_monthly_ZTM")

test_that("tickets_costs_monthly_ZTM correct results", {
  expect_equal(tickets_costs_monthly_ZTM(travel_time = 30, discount=TRUE, zone=1, varsovian=TRUE)$number_of_tickets, 20)
  expect_equal(tickets_costs_monthly_ZTM(travel_time = 30, discount=TRUE, zone=1, varsovian=TRUE)$tickets_cost, 44)
  expect_is(tickets_costs_monthly_ZTM(travel_time = 30, discount=TRUE, zone=1, varsovian=TRUE)$ztm_ticket, "data.frame")
})

test_that("tickets_costs_monthly_ZTM incorrect results", {
  expect_error(tickets_costs_monthly_ZTM(travel_time = "30", discount=TRUE, zone=1, varsovian=TRUE)$number_of_tickets)
  expect_error(tickets_costs_monthly_ZTM(travel_time = 30, discount="TRUE", zone=1, varsovian=TRUE)$number_of_tickets)
  expect_error(tickets_costs_monthly_ZTM(travel_time = 30, discount=TRUE, zone="1", varsovian=TRUE)$number_of_tickets)
  expect_error(tickets_costs_monthly_ZTM(travel_time = 30, discount=TRUE, zone=1, varsovian="TRUE")$number_of_tickets)
  expect_error(tickets_costs_monthly_ZTM(travel_time = "30", discount=TRUE, zone=1, varsovian=TRUE)$tickets_cost)
  expect_error(tickets_costs_monthly_ZTM(travel_time = 30, discount="TRUE", zone=1, varsovian=TRUE)$tickets_cost)
  expect_error(tickets_costs_monthly_ZTM(travel_time = 30, discount=TRUE, zone="1", varsovian=TRUE)$tickets_cost)
  expect_error(tickets_costs_monthly_ZTM(travel_time = 30, discount=TRUE, zone=1, varsovian="TRUE")$tickets_cost)
  expect_error(tickets_costs_monthly_ZTM(travel_time = "30", discount=TRUE, zone=1, varsovian=TRUE)$ztm_ticket)
  expect_error(tickets_costs_monthly_ZTM(travel_time = 30, discount="TRUE", zone=1, varsovian=TRUE)$ztm_ticket)
  expect_error(tickets_costs_monthly_ZTM(travel_time = 30, discount=TRUE, zone="1", varsovian=TRUE)$ztm_ticket)
  expect_error(tickets_costs_monthly_ZTM(travel_time = 30, discount=TRUE, zone=1, varsovian="TRUE")$ztm_ticket)
})


context("tickets_costs_annual_ZTM")

test_that("tickets_costs_annual_ZTM correct results", {
  expect_equal(tickets_costs_annual_ZTM(travel_time = 30, discount=TRUE, zone=1, varsovian=TRUE)$number_of_tickets, 5)
  expect_equal(tickets_costs_annual_ZTM(travel_time = 30, discount=TRUE, zone=1, varsovian=TRUE)$tickets_cost, 625)
  expect_is(tickets_costs_annual_ZTM(travel_time = 30, discount=TRUE, zone=1, varsovian=TRUE)$ztm_ticket, "data.frame")
})

test_that("tickets_costs_annual_ZTM incorrect results", {
  expect_error(tickets_costs_annual_ZTM(travel_time = "30", discount=TRUE, zone=1, varsovian=TRUE)$number_of_tickets)
  expect_error(tickets_costs_annual_ZTM(travel_time = 30, discount="TRUE", zone=1, varsovian=TRUE)$number_of_tickets)
  expect_error(tickets_costs_annual_ZTM(travel_time = 30, discount=TRUE, zone="1", varsovian=TRUE)$number_of_tickets)
  expect_error(tickets_costs_annual_ZTM(travel_time = 30, discount=TRUE, zone=1, varsovian="TRUE")$number_of_tickets)
  expect_error(tickets_costs_annual_ZTM(travel_time = "30", discount=TRUE, zone=1, varsovian=TRUE)$tickets_cost)
  expect_error(tickets_costs_annual_ZTM(travel_time = 30, discount="TRUE", zone=1, varsovian=TRUE)$tickets_cost)
  expect_error(tickets_costs_annual_ZTM(travel_time = 30, discount=TRUE, zone="1", varsovian=TRUE)$tickets_cost)
  expect_error(tickets_costs_annual_ZTM(travel_time = 30, discount=TRUE, zone=1, varsovian="TRUE")$tickets_cost)
  expect_error(tickets_costs_annual_ZTM(travel_time = "30", discount=TRUE, zone=1, varsovian=TRUE)$ztm_ticket)
  expect_error(tickets_costs_annual_ZTM(travel_time = 30, discount="TRUE", zone=1, varsovian=TRUE)$ztm_ticket)
  expect_error(tickets_costs_annual_ZTM(travel_time = 30, discount=TRUE, zone="1", varsovian=TRUE)$ztm_ticket)
  expect_error(tickets_costs_annual_ZTM(travel_time = 30, discount=TRUE, zone=1, varsovian="TRUE")$ztm_ticket)
})
