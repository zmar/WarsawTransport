library(WarsawTransport)
context("tickets_costs_daily_ZTM")

test_that("tickets_costs_daily_ZTM correct results", {
  expect_equal(tickets_costs_daily_ZTM(travel_time = 30, discount=TRUE, zone=1, warszawiak=TRUE)$number_of_tickets, 1)
  expect_equal(tickets_costs_daily_ZTM(travel_time = 30, discount=TRUE, zone=1, warszawiak=TRUE)$tickets_cost, 2.2)
  expect_is(tickets_costs_daily_ZTM(travel_time = 30, discount=TRUE, zone=1, warszawiak=TRUE)$ztm_ticket, "data.frame")
})

test_that("tickets_costs_daily_ZTM incorrect results", {
  expect_error(tickets_costs_daily_ZTM(travel_time = "30", discount=TRUE, zone=1, warszawiak=TRUE)$number_of_tickets)
  expect_error(tickets_costs_daily_ZTM(travel_time = 30, discount="TRUE", zone=1, warszawiak=TRUE)$number_of_tickets)
  expect_error(tickets_costs_daily_ZTM(travel_time = 30, discount=TRUE, zone="1", warszawiak=TRUE)$number_of_tickets)
  expect_error(tickets_costs_daily_ZTM(travel_time = 30, discount=TRUE, zone=1, warszawiak="TRUE")$number_of_tickets)
  expect_error(tickets_costs_daily_ZTM(travel_time = "30", discount=TRUE, zone=1, warszawiak=TRUE)$tickets_cost)
  expect_error(tickets_costs_daily_ZTM(travel_time = 30, discount="TRUE", zone=1, warszawiak=TRUE)$tickets_cost)
  expect_error(tickets_costs_daily_ZTM(travel_time = 30, discount=TRUE, zone="1", warszawiak=TRUE)$tickets_cost)
  expect_error(tickets_costs_daily_ZTM(travel_time = 30, discount=TRUE, zone=1, warszawiak="TRUE")$tickets_cost)
  expect_error(tickets_costs_daily_ZTM(travel_time = "30", discount=TRUE, zone=1, warszawiak=TRUE)$ztm_ticket)
  expect_error(tickets_costs_daily_ZTM(travel_time = 30, discount="TRUE", zone=1, warszawiak=TRUE)$ztm_ticket)
  expect_error(tickets_costs_daily_ZTM(travel_time = 30, discount=TRUE, zone="1", warszawiak=TRUE)$ztm_ticket)
  expect_error(tickets_costs_daily_ZTM(travel_time = 30, discount=TRUE, zone=1, warszawiak="TRUE")$ztm_ticket)
})


context("tickets_costs_monthly_ZTM")

test_that("tickets_costs_monthly_ZTM correct results", {
  expect_equal(tickets_costs_monthly_ZTM(travel_time = 30, discount=TRUE, zone=1, warszawiak=TRUE)$number_of_tickets, 20)
  expect_equal(tickets_costs_monthly_ZTM(travel_time = 30, discount=TRUE, zone=1, warszawiak=TRUE)$tickets_cost, 44)
  expect_is(tickets_costs_monthly_ZTM(travel_time = 30, discount=TRUE, zone=1, warszawiak=TRUE)$ztm_ticket, "data.frame")
})

test_that("tickets_costs_monthly_ZTM incorrect results", {
  expect_error(tickets_costs_monthly_ZTM(travel_time = "30", discount=TRUE, zone=1, warszawiak=TRUE)$number_of_tickets)
  expect_error(tickets_costs_monthly_ZTM(travel_time = 30, discount="TRUE", zone=1, warszawiak=TRUE)$number_of_tickets)
  expect_error(tickets_costs_monthly_ZTM(travel_time = 30, discount=TRUE, zone="1", warszawiak=TRUE)$number_of_tickets)
  expect_error(tickets_costs_monthly_ZTM(travel_time = 30, discount=TRUE, zone=1, warszawiak="TRUE")$number_of_tickets)
  expect_error(tickets_costs_monthly_ZTM(travel_time = "30", discount=TRUE, zone=1, warszawiak=TRUE)$tickets_cost)
  expect_error(tickets_costs_monthly_ZTM(travel_time = 30, discount="TRUE", zone=1, warszawiak=TRUE)$tickets_cost)
  expect_error(tickets_costs_monthly_ZTM(travel_time = 30, discount=TRUE, zone="1", warszawiak=TRUE)$tickets_cost)
  expect_error(tickets_costs_monthly_ZTM(travel_time = 30, discount=TRUE, zone=1, warszawiak="TRUE")$tickets_cost)
  expect_error(tickets_costs_monthly_ZTM(travel_time = "30", discount=TRUE, zone=1, warszawiak=TRUE)$ztm_ticket)
  expect_error(tickets_costs_monthly_ZTM(travel_time = 30, discount="TRUE", zone=1, warszawiak=TRUE)$ztm_ticket)
  expect_error(tickets_costs_monthly_ZTM(travel_time = 30, discount=TRUE, zone="1", warszawiak=TRUE)$ztm_ticket)
  expect_error(tickets_costs_monthly_ZTM(travel_time = 30, discount=TRUE, zone=1, warszawiak="TRUE")$ztm_ticket)
})


context("tickets_costs_annual_ZTM")

test_that("tickets_costs_annual_ZTM correct results", {
  expect_equal(tickets_costs_annual_ZTM(travel_time = 30, discount=TRUE, zone=1, warszawiak=TRUE)$number_of_tickets, 5)
  expect_equal(tickets_costs_annual_ZTM(travel_time = 30, discount=TRUE, zone=1, warszawiak=TRUE)$tickets_cost, 625)
  expect_is(tickets_costs_annual_ZTM(travel_time = 30, discount=TRUE, zone=1, warszawiak=TRUE)$ztm_ticket, "data.frame")
})

test_that("tickets_costs_annual_ZTM incorrect results", {
  expect_error(tickets_costs_annual_ZTM(travel_time = "30", discount=TRUE, zone=1, warszawiak=TRUE)$number_of_tickets)
  expect_error(tickets_costs_annual_ZTM(travel_time = 30, discount="TRUE", zone=1, warszawiak=TRUE)$number_of_tickets)
  expect_error(tickets_costs_annual_ZTM(travel_time = 30, discount=TRUE, zone="1", warszawiak=TRUE)$number_of_tickets)
  expect_error(tickets_costs_annual_ZTM(travel_time = 30, discount=TRUE, zone=1, warszawiak="TRUE")$number_of_tickets)
  expect_error(tickets_costs_annual_ZTM(travel_time = "30", discount=TRUE, zone=1, warszawiak=TRUE)$tickets_cost)
  expect_error(tickets_costs_annual_ZTM(travel_time = 30, discount="TRUE", zone=1, warszawiak=TRUE)$tickets_cost)
  expect_error(tickets_costs_annual_ZTM(travel_time = 30, discount=TRUE, zone="1", warszawiak=TRUE)$tickets_cost)
  expect_error(tickets_costs_annual_ZTM(travel_time = 30, discount=TRUE, zone=1, warszawiak="TRUE")$tickets_cost)
  expect_error(tickets_costs_annual_ZTM(travel_time = "30", discount=TRUE, zone=1, warszawiak=TRUE)$ztm_ticket)
  expect_error(tickets_costs_annual_ZTM(travel_time = 30, discount="TRUE", zone=1, warszawiak=TRUE)$ztm_ticket)
  expect_error(tickets_costs_annual_ZTM(travel_time = 30, discount=TRUE, zone="1", warszawiak=TRUE)$ztm_ticket)
  expect_error(tickets_costs_annual_ZTM(travel_time = 30, discount=TRUE, zone=1, warszawiak="TRUE")$ztm_ticket)
})
