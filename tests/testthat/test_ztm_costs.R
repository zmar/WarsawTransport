library(WarsawTransport)

####################################### tickets_costs_daily_ZTM #######################################

context("tickets_costs_daily_ZTM")

test_that("tickets_costs_daily_ZTM - correct input", {
  # correct input - results of number_of_tickets, tickets_cost and ztm_ticket number_of_tickets results is a numeric object
  expect_equal(
    tickets_costs_daily_ZTM(
      travel_time = 30,
      discount = TRUE,
      zone = 1,
      varsovian = TRUE
    )$number_of_tickets,
    1
  )
  # tickets_cost results ia a numeric object
  expect_equal(
    tickets_costs_daily_ZTM(
      travel_time = 30,
      discount = TRUE,
      zone = 1,
      varsovian = TRUE
    )$tickets_cost,
    2.2
  )
  # ztm_ticket results is a data.frame object
  expect_is(
    tickets_costs_daily_ZTM(
      travel_time = 30,
      discount = TRUE,
      zone = 1,
      varsovian = TRUE
    )$ztm_ticket,
    "data.frame"
  )
})
test_that(
  "tickets_costs_daily_ZTM errors - value number_of_tickets - handling character input",
  {
    # number_of_tickets input - travel_time as character
    expect_error(
      tickets_costs_daily_ZTM(
        travel_time = "30",
        discount = TRUE,
        zone = 1,
        varsovian = TRUE
      )$number_of_tickets,
      regexp = "Error: argument travel_time is not numeric. Please enter travel_time as numeric vector of length 1."
    )
    ## input - discount as character
    expect_error(
      tickets_costs_daily_ZTM(
        travel_time = 30,
        discount = "TRUE",
        zone = 1,
        varsovian = TRUE
      )$number_of_tickets,
      regexp = "Error: argument discount is not logical. Please enter TRUE or FALSE."
    )
    ## input - zone as character
    expect_error(
      tickets_costs_daily_ZTM(
        travel_time = 30,
        discount = TRUE,
        zone = "1",
        varsovian = TRUE
      )$number_of_tickets,
      regexp = "Error: argument zone is not numeric. Please enter zone as integer 1, 2 or 3."
    )
    ## input - varsovian as character
    expect_error(
      tickets_costs_daily_ZTM(
        travel_time = 30,
        discount = TRUE,
        zone = 1,
        varsovian = "TRUE"
      )$number_of_tickets,
      regexp = "Error: argument varsovian is not logical. Please enter TRUE or FALSE."

    )
  }
)
test_that("tickets_costs_daily_ZTM errors - value number_of_tickets - handling NA input",
          {
            # number_of_tickets input - travel_time as NA
            expect_error(
              tickets_costs_daily_ZTM(
                travel_time = NA,
                discount = TRUE,
                zone = 1,
                varsovian = TRUE
              )$number_of_tickets,
              regexp = "Error: argument travel_time is not numeric. Please enter travel_time as numeric vector of length 1."
            )
            ## input - discount as NA
            expect_error(
              tickets_costs_daily_ZTM(
                travel_time = 30,
                discount = NA,
                zone = 1,
                varsovian = TRUE
              )$number_of_tickets,
              regexp = "Error: argument discount is not logical. Please enter TRUE or FALSE."
            )
            ## input - zone as NA
            expect_error(
              tickets_costs_daily_ZTM(
                travel_time = 30,
                discount = TRUE,
                zone = NA,
                varsovian = TRUE
              )$number_of_tickets,
              regexp = "Error: argument zone is not numeric. Please enter zone as integer 1, 2 or 3."
            )
            ## input - varsovian as NA
            expect_error(
              tickets_costs_daily_ZTM(
                travel_time = 30,
                discount = TRUE,
                zone = 1,
                varsovian = NA
              )$number_of_tickets,
              regexp = "Error: argument varsovian is not logical. Please enter TRUE or FALSE."
            )
          })
test_that(
  "tickets_costs_daily_ZTM errors - value number_of_tickets - handling input vector of length 2",
  {
    # number_of_tickets input - travel_time as c(30,30)
    expect_error(
      tickets_costs_daily_ZTM(
        travel_time = c(30, 30),
        discount = TRUE,
        zone = 1,
        varsovian = TRUE
      )$number_of_tickets,
      regexp = "Error: argument travel_time is not a vector of length 1. Please enter travel_time as numeric vector of length 1."
    )
    ## input - discount as c(TRUE,FALSE)
    expect_error(
      tickets_costs_daily_ZTM(
        travel_time = 30,
        discount = c(TRUE, FALSE),
        zone = 1,
        varsovian = TRUE
      )$number_of_tickets,
      regexp = "Error: argument discount is not a vector of length 1. Please enter TRUE or FALSE."
    )
    ## input - zone as c(1,2)
    expect_error(
      tickets_costs_daily_ZTM(
        travel_time = 30,
        discount = TRUE,
        zone = c(1, 2),
        varsovian = TRUE
      )$number_of_tickets,
      regexp = "Error: argument zone is not a vector of length 1. Please enter integer 1, 2 or 3"
    )
    ## input - varsovian as c(TRUE, FALSE)
    expect_error(
      tickets_costs_daily_ZTM(
        travel_time = 30,
        discount = TRUE,
        zone = 1,
        varsovian = c(TRUE, FALSE)
      )$number_of_tickets,
      regexp = "Error: argument varsovian is not a vector of length 1. Please enter TRUE or FALSE."
    )
  }
)
test_that("tickets_costs_daily_ZTM errors - value tickets_cost - handling character input",
          {
            # tickets_cost input - travel_time as character
            expect_error(
              tickets_costs_daily_ZTM(
                travel_time = "30",
                discount = TRUE,
                zone = 1,
                varsovian = TRUE
              )$tickets_cost,
              regexp = "Error: argument travel_time is not numeric. Please enter travel_time as numeric vector of length 1."
            )
            ## input - discount as character
            expect_error(
              tickets_costs_daily_ZTM(
                travel_time = 30,
                discount = "TRUE",
                zone = 1,
                varsovian = TRUE
              )$tickets_cost,
              regexp = "Error: argument discount is not logical. Please enter TRUE or FALSE."
            )
            ## input - zone as character
            expect_error(
              tickets_costs_daily_ZTM(
                travel_time = 30,
                discount = TRUE,
                zone = "1",
                varsovian = TRUE
              )$tickets_cost,
              regexp = "Error: argument zone is not numeric. Please enter zone as integer 1, 2 or 3."
            )
            ## input - varsovian as character
            expect_error(
              tickets_costs_daily_ZTM(
                travel_time = 30,
                discount = TRUE,
                zone = 1,
                varsovian = "TRUE"
              )$tickets_cost,
              regexp = "Error: argument varsovian is not logical. Please enter TRUE or FALSE."
            )
          })
test_that("tickets_costs_daily_ZTM errors - value tickets_cost - handling NA input",
          {
            # tickets_cost input - travel_time as NA
            expect_error(
              tickets_costs_daily_ZTM(
                travel_time = NA,
                discount = TRUE,
                zone = 1,
                varsovian = TRUE
              )$tickets_cost,
              regexp = "Error: argument travel_time is not numeric. Please enter travel_time as numeric vector of length 1."
            )
            ## input - discount as NA
            expect_error(
              tickets_costs_daily_ZTM(
                travel_time = 30,
                discount = NA,
                zone = 1,
                varsovian = TRUE
              )$tickets_cost,
              regexp = "Error: argument discount is not logical. Please enter TRUE or FALSE."
            )
            ## input - zone as NA
            expect_error(
              tickets_costs_daily_ZTM(
                travel_time = 30,
                discount = TRUE,
                zone = NA,
                varsovian = TRUE
              )$tickets_cost,
              regexp = "Error: argument zone is not numeric. Please enter zone as integer 1, 2 or 3."
            )
            ## input - varsovian as NA
            expect_error(
              tickets_costs_daily_ZTM(
                travel_time = 30,
                discount = TRUE,
                zone = 1,
                varsovian = NA
              )$tickets_cost,
              regexp = "Error: argument varsovian is not logical. Please enter TRUE or FALSE."
            )
          })
test_that(
  "tickets_costs_daily_ZTM errors - value tickets_cost - handling input vector of length 2",
  {
    # tickets_cost input - travel_time as c(30,30)
    expect_error(
      tickets_costs_daily_ZTM(
        travel_time = c(30, 30),
        discount = TRUE,
        zone = 1,
        varsovian = TRUE
      )$tickets_cost,
      regexp = "Error: argument travel_time is not a vector of length 1. Please enter travel_time as numeric vector of length 1."
    )
    ## input - discount as c(TRUE,FALSE)
    expect_error(
      tickets_costs_daily_ZTM(
        travel_time = 30,
        discount = c(TRUE, FALSE),
        zone = 1,
        varsovian = TRUE
      )$tickets_cost,
      regexp = "Error: argument discount is not a vector of length 1. Please enter TRUE or FALSE."
    )
    ## input - zone as c(1,2)
    expect_error(
      tickets_costs_daily_ZTM(
        travel_time = 30,
        discount = TRUE,
        zone = c(1, 2),
        varsovian = TRUE
      )$tickets_cost,
      regexp = "Error: argument zone is not a vector of length 1. Please enter integer 1, 2 or 3"
    )
    ## input - varsovian as c(TRUE, FALSE)
    expect_error(
      tickets_costs_daily_ZTM(
        travel_time = 30,
        discount = TRUE,
        zone = 1,
        varsovian = c(TRUE, FALSE)
      )$tickets_cost,
      regexp = "Error: argument varsovian is not a vector of length 1. Please enter TRUE or FALSE."
    )
  }
)
# test_that("tickets_costs_daily_ZTM errors - value ztm_ticket - handling character input",
#           {
#             # ztm_ticket input - travel_time as character
#             expect_error(
#               tickets_costs_daily_ZTM(
#                 travel_time = "30",
#                 discount = TRUE,
#                 zone = 1,
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp = "Error: argument travel_time is not numeric. Please enter travel_time as numeric vector of length 1."
#             )
#             ## input - discount as character
#             expect_error(
#               tickets_costs_daily_ZTM(
#                 travel_time = 30,
#                 discount = "TRUE",
#                 zone = 1,
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp = "Error: argument discount is not logical. Please enter TRUE or FALSE."
#             )
#             ## input - zone as character
#             expect_error(
#               tickets_costs_daily_ZTM(
#                 travel_time = 30,
#                 discount = TRUE,
#                 zone = "1",
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp = "Error: argument zone is not numeric. Please enter zone as integer 1, 2 or 3."
#             )
#             ## input - varsovian as character
#             expect_error(
#               tickets_costs_daily_ZTM(
#                 travel_time = 30,
#                 discount = TRUE,
#                 zone = 1,
#                 varsovian = "TRUE"
#               )$ztm_ticket,
#               regexp = "Error: argument varsovian is not logical. Please enter TRUE or FALSE."
#             )
#           })
# test_that("tickets_costs_daily_ZTM errors - value ztm_ticket - handling NA input",
#           {
#             # ztm_ticket input - travel_time as NA
#             expect_error(
#               tickets_costs_daily_ZTM(
#                 travel_time = NA,
#                 discount = TRUE,
#                 zone = 1,
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp =  "Error: argument travel_time is not numeric. Please enter travel_time as numeric vector of length 1."
#             )
#             ## input - discount as NA
#             expect_error(
#               tickets_costs_daily_ZTM(
#                 travel_time = 30,
#                 discount = NA,
#                 zone = 1,
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp =  "Error: argument discount is not logical. Please enter TRUE or FALSE."
#             )
#             ## input - zone as NA
#             expect_error(
#               tickets_costs_daily_ZTM(
#                 travel_time = 30,
#                 discount = TRUE,
#                 zone = NA,
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp =  "Error: argument zone is not numeric. Please enter zone as integer 1, 2 or 3."
#             )
#             ## input - varsovian as NA
#             expect_error(
#               tickets_costs_daily_ZTM(
#                 travel_time = 30,
#                 discount = TRUE,
#                 zone = 1,
#                 varsovian = NA
#               )$ztm_ticket,
#               regexp =  "Error: argument varsovian is not logical. Please enter TRUE or FALSE."
#             )
#           })
# test_that("tickets_costs_daily_ZTM errors - value ztm_ticket - handling input vector of length 2",
#           {
#             # ztm_ticket input - travel_time as c(30,30)
#             expect_error(
#               tickets_costs_daily_ZTM(
#                 travel_time = c(30, 30),
#                 discount = TRUE,
#                 zone = 1,
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp = "Error: argument travel_time is not a vector of length 1. Please enter travel_time as numeric vector of length 1."
#             )
#             ## input - discount as c(TRUE,FALSE)
#             expect_error(
#               tickets_costs_daily_ZTM(
#                 travel_time = 30,
#                 discount = c(TRUE, FALSE),
#                 zone = 1,
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp = "Error: argument discount is not a vector of length 1. Please enter TRUE or FALSE."
#             )
#             ## input - zone as c(1,2)
#             expect_error(
#               tickets_costs_daily_ZTM(
#                 travel_time = 30,
#                 discount = TRUE,
#                 zone = c(1, 2),
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp = "Error: argument zone is not a vector of length 1. Please enter integer 1, 2 or 3"
#             )
#             ## input - varsovian as c(TRUE, FALSE)
#             expect_error(
#               tickets_costs_daily_ZTM(
#                 travel_time = 30,
#                 discount = TRUE,
#                 zone = 1,
#                 varsovian = c(TRUE, FALSE)
#               )$ztm_ticket,
#               regexp = "Error: argument varsovian is not a vector of length 1. Please enter TRUE or FALSE."
#             )
#           })
####################################### tickets_costs_monthly_ZTM #######################################

context("tickets_costs_monthly_ZTM")

test_that("tickets_costs_monthly_ZTM - correct input", {
  # correct input - results of number_of_tickets, tickets_cost and ztm_ticket number_of_tickets results is a numeric object
  expect_equal(
    tickets_costs_monthly_ZTM(
      travel_time = 30,
      discount = TRUE,
      zone = 1,
      varsovian = TRUE
    )$number_of_tickets,
    20
  )
  # tickets_cost results ia a numeric object
  expect_equal(
    tickets_costs_monthly_ZTM(
      travel_time = 30,
      discount = TRUE,
      zone = 1,
      varsovian = TRUE
    )$tickets_cost,
    44
  )
  # ztm_ticket results is a data.frame object
  expect_is(
    tickets_costs_monthly_ZTM(
      travel_time = 30,
      discount = TRUE,
      zone = 1,
      varsovian = TRUE
    )$ztm_ticket,
    "data.frame"
  )
})

test_that(
  "tickets_costs_monthly_ZTM errors - value number_of_tickets - handling character input",
  {
    # input - travel_time as character
    expect_error(
      tickets_costs_monthly_ZTM(
        travel_time = "30",
        discount = TRUE,
        zone = 1,
        varsovian = TRUE
      )$number_of_tickets,
      regexp = "Error: argument travel_time is not numeric. Please enter travel_time as numeric vector of length 1."
    )
    ## input - discount as character
    expect_error(
      tickets_costs_monthly_ZTM(
        travel_time = 30,
        discount = "TRUE",
        zone = 1,
        varsovian = TRUE
      )$number_of_tickets,
      regexp = "Error: argument discount is not logical. Please enter TRUE or FALSE."
    )
    ## input - zone as character
    expect_error(
      tickets_costs_monthly_ZTM(
        travel_time = 30,
        discount = TRUE,
        zone = "1",
        varsovian = TRUE
      )$number_of_tickets,
      regexp = "Error: argument zone is not numeric. Please enter zone as integer 1, 2 or 3."
    )
    ## input - varsovian as character
    expect_error(
      tickets_costs_monthly_ZTM(
        travel_time = 30,
        discount = TRUE,
        zone = 1,
        varsovian = "TRUE"
      )$number_of_tickets,
      regexp = "Error: argument varsovian is not logical. Please enter TRUE or FALSE."

    )
  }
)
test_that("tickets_costs_monthly_ZTM errors - value number_of_tickets - handling NA input",
          {
            # input - travel_time as NA
            expect_error(
              tickets_costs_monthly_ZTM(
                travel_time = NA,
                discount = TRUE,
                zone = 1,
                varsovian = TRUE
              )$number_of_tickets,
              regexp = "Error: argument travel_time is not numeric. Please enter travel_time as numeric vector of length 1."
            )
            ## input - discount as NA
            expect_error(
              tickets_costs_monthly_ZTM(
                travel_time = 30,
                discount = NA,
                zone = 1,
                varsovian = TRUE
              )$number_of_tickets,
              regexp = "Error: argument discount is not logical. Please enter TRUE or FALSE."
            )
            ## input - zone as NA
            expect_error(
              tickets_costs_monthly_ZTM(
                travel_time = 30,
                discount = TRUE,
                zone = NA,
                varsovian = TRUE
              )$number_of_tickets,
              regexp = "Error: argument zone is not numeric. Please enter zone as integer 1, 2 or 3."
            )
            ## input - varsovian as NA
            expect_error(
              tickets_costs_monthly_ZTM(
                travel_time = 30,
                discount = TRUE,
                zone = 1,
                varsovian = NA
              )$number_of_tickets,
              regexp = "Error: argument varsovian is not logical. Please enter TRUE or FALSE."
            )
          })
test_that(
  "tickets_costs_monthly_ZTM errors - value number_of_tickets - handling input vector of length 2",
  {
    # input - travel_time as c(30,30)
    expect_error(
      tickets_costs_monthly_ZTM(
        travel_time = c(30, 30),
        discount = TRUE,
        zone = 1,
        varsovian = TRUE
      )$number_of_tickets,
      regexp = "Error: argument travel_time is not a vector of length 1. Please enter travel_time as numeric vector of length 1."
    )
    ## input - discount as c(TRUE,FALSE)
    expect_error(
      tickets_costs_monthly_ZTM(
        travel_time = 30,
        discount = c(TRUE, FALSE),
        zone = 1,
        varsovian = TRUE
      )$number_of_tickets,
      regexp = "Error: argument discount is not a vector of length 1. Please enter TRUE or FALSE."
    )
    ## input - zone as c(1,2)
    expect_error(
      tickets_costs_monthly_ZTM(
        travel_time = 30,
        discount = TRUE,
        zone = c(1, 2),
        varsovian = TRUE
      )$number_of_tickets,
      regexp = "Error: argument zone is not a vector of length 1. Please enter integer 1, 2 or 3"
    )
    ## input - varsovian as c(TRUE, FALSE)
    expect_error(
      tickets_costs_monthly_ZTM(
        travel_time = 30,
        discount = TRUE,
        zone = 1,
        varsovian = c(TRUE, FALSE)
      )$number_of_tickets,
      regexp = "Error: argument varsovian is not a vector of length 1. Please enter TRUE or FALSE."
    )
  }
)
test_that("tickets_costs_monthly_ZTM errors - value tickets_cost - handling character input",
          {
            # input - travel_time as character
            expect_error(
              tickets_costs_monthly_ZTM(
                travel_time = "30",
                discount = TRUE,
                zone = 1,
                varsovian = TRUE
              )$tickets_cost,
              regexp = "Error: argument travel_time is not numeric. Please enter travel_time as numeric vector of length 1."
            )
            ## input - discount as character
            expect_error(
              tickets_costs_monthly_ZTM(
                travel_time = 30,
                discount = "TRUE",
                zone = 1,
                varsovian = TRUE
              )$tickets_cost,
              regexp = "Error: argument discount is not logical. Please enter TRUE or FALSE."
            )
            ## input - zone as character
            expect_error(
              tickets_costs_monthly_ZTM(
                travel_time = 30,
                discount = TRUE,
                zone = "1",
                varsovian = TRUE
              )$tickets_cost,
              regexp = "Error: argument zone is not numeric. Please enter zone as integer 1, 2 or 3."
            )
            ## input - varsovian as character
            expect_error(
              tickets_costs_monthly_ZTM(
                travel_time = 30,
                discount = TRUE,
                zone = 1,
                varsovian = "TRUE"
              )$tickets_cost,
              regexp = "Error: argument varsovian is not logical. Please enter TRUE or FALSE."
            )
          })
test_that("tickets_costs_monthly_ZTM errors - value tickets_cost - handling NA input",
          {
            # input - travel_time as NA
            expect_error(
              tickets_costs_monthly_ZTM(
                travel_time = NA,
                discount = TRUE,
                zone = 1,
                varsovian = TRUE
              )$tickets_cost,
              regexp = "Error: argument travel_time is not numeric. Please enter travel_time as numeric vector of length 1."
            )
            ## input - discount as NA
            expect_error(
              tickets_costs_monthly_ZTM(
                travel_time = 30,
                discount = NA,
                zone = 1,
                varsovian = TRUE
              )$tickets_cost,
              regexp = "Error: argument discount is not logical. Please enter TRUE or FALSE."
            )
            ## input - zone as NA
            expect_error(
              tickets_costs_monthly_ZTM(
                travel_time = 30,
                discount = TRUE,
                zone = NA,
                varsovian = TRUE
              )$tickets_cost,
              regexp = "Error: argument zone is not numeric. Please enter zone as integer 1, 2 or 3."
            )
            ## input - varsovian as NA
            expect_error(
              tickets_costs_monthly_ZTM(
                travel_time = 30,
                discount = TRUE,
                zone = 1,
                varsovian = NA
              )$tickets_cost,
              regexp = "Error: argument varsovian is not logical. Please enter TRUE or FALSE."
            )
          })
test_that(
  "tickets_costs_monthly_ZTM errors - value tickets_cost - handling input vector of length 2",
  {
    # input - travel_time as c(30,30)
    expect_error(
      tickets_costs_monthly_ZTM(
        travel_time = c(30, 30),
        discount = TRUE,
        zone = 1,
        varsovian = TRUE
      )$tickets_cost,
      regexp = "Error: argument travel_time is not a vector of length 1. Please enter travel_time as numeric vector of length 1."
    )
    ## input - discount as c(TRUE,FALSE)
    expect_error(
      tickets_costs_monthly_ZTM(
        travel_time = 30,
        discount = c(TRUE, FALSE),
        zone = 1,
        varsovian = TRUE
      )$tickets_cost,
      regexp = "Error: argument discount is not a vector of length 1. Please enter TRUE or FALSE."
    )
    ## input - zone as c(1,2)
    expect_error(
      tickets_costs_monthly_ZTM(
        travel_time = 30,
        discount = TRUE,
        zone = c(1, 2),
        varsovian = TRUE
      )$tickets_cost,
      regexp = "Error: argument zone is not a vector of length 1. Please enter integer 1, 2 or 3"
    )
    ## input - varsovian as c(TRUE, FALSE)
    expect_error(
      tickets_costs_monthly_ZTM(
        travel_time = 30,
        discount = TRUE,
        zone = 1,
        varsovian = c(TRUE, FALSE)
      )$tickets_cost,
      regexp = "Error: argument varsovian is not a vector of length 1. Please enter TRUE or FALSE."
    )
  }
)
# test_that("tickets_costs_monthly_ZTM errors - value ztm_ticket - handling character input",
#           {
#             # input - travel_time as character
#             expect_error(
#               tickets_costs_monthly_ZTM(
#                 travel_time = "30",
#                 discount = TRUE,
#                 zone = 1,
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp = "Error: argument travel_time is not numeric. Please enter travel_time as numeric vector of length 1."
#             )
#             ## input - discount as character
#             expect_error(
#               tickets_costs_monthly_ZTM(
#                 travel_time = 30,
#                 discount = "TRUE",
#                 zone = 1,
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp = "Error: argument discount is not logical. Please enter TRUE or FALSE."
#             )
#             ## input - zone as character
#             expect_error(
#               tickets_costs_monthly_ZTM(
#                 travel_time = 30,
#                 discount = TRUE,
#                 zone = "1",
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp = "Error: argument zone is not numeric. Please enter zone as integer 1, 2 or 3."
#             )
#             ## input - varsovian as character
#             expect_error(
#               tickets_costs_monthly_ZTM(
#                 travel_time = 30,
#                 discount = TRUE,
#                 zone = 1,
#                 varsovian = "TRUE"
#               )$ztm_ticket,
#               regexp = "Error: argument varsovian is not logical. Please enter TRUE or FALSE."
#             )
#           })
# test_that("tickets_costs_monthly_ZTM errors - value ztm_ticket - handling NA input",
#           {
#             # input - travel_time as NA
#             expect_error(
#               tickets_costs_monthly_ZTM(
#                 travel_time = NA,
#                 discount = TRUE,
#                 zone = 1,
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp =  "Error: argument travel_time is not numeric. Please enter travel_time as numeric vector of length 1."
#             )
#             ## input - discount as NA
#             expect_error(
#               tickets_costs_monthly_ZTM(
#                 travel_time = 30,
#                 discount = NA,
#                 zone = 1,
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp =  "Error: argument discount is not logical. Please enter TRUE or FALSE."
#             )
#             ## input - zone as NA
#             expect_error(
#               tickets_costs_monthly_ZTM(
#                 travel_time = 30,
#                 discount = TRUE,
#                 zone = NA,
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp =  "Error: argument zone is not numeric. Please enter zone as integer 1, 2 or 3."
#             )
#             ## input - varsovian as NA
#             expect_error(
#               tickets_costs_monthly_ZTM(
#                 travel_time = 30,
#                 discount = TRUE,
#                 zone = 1,
#                 varsovian = NA
#               )$ztm_ticket,
#               regexp =  "Error: argument varsovian is not logical. Please enter TRUE or FALSE."
#             )
#           })
# test_that("tickets_costs_monthly_ZTM errors - value ztm_ticket - handling input vector of length 2",
#           {
#             # input - travel_time as c(30,30)
#             expect_error(
#               tickets_costs_monthly_ZTM(
#                 travel_time = c(30, 30),
#                 discount = TRUE,
#                 zone = 1,
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp = "Error: argument travel_time is not a vector of length 1. Please enter travel_time as numeric vector of length 1."
#             )
#             ## input - discount as c(TRUE,FALSE)
#             expect_error(
#               tickets_costs_monthly_ZTM(
#                 travel_time = 30,
#                 discount = c(TRUE, FALSE),
#                 zone = 1,
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp = "Error: argument discount is not a vector of length 1. Please enter TRUE or FALSE."
#             )
#             ## input - zone as c(1,2)
#             expect_error(
#               tickets_costs_monthly_ZTM(
#                 travel_time = 30,
#                 discount = TRUE,
#                 zone = c(1, 2),
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp = "Error: argument zone is not a vector of length 1. Please enter integer 1, 2 or 3"
#             )
#             ## input - varsovian as c(TRUE, FALSE)
#             expect_error(
#               tickets_costs_monthly_ZTM(
#                 travel_time = 30,
#                 discount = TRUE,
#                 zone = 1,
#                 varsovian = c(TRUE, FALSE)
#               )$ztm_ticket,
#               regexp = "Error: argument varsovian is not a vector of length 1. Please enter TRUE or FALSE."
#             )
#           })
####################################### tickets_costs_annual_ZTM #######################################
context("tickets_costs_annual_ZTM")

test_that("tickets_costs_annual_ZTM correct results", {
  # correct input - results of number_of_tickets, tickets_cost and ztm_ticket number_of_tickets results is a numeric object
  expect_equal(
    tickets_costs_annual_ZTM(
      travel_time = 30,
      discount = TRUE,
      zone = 1,
      varsovian = TRUE
    )$number_of_tickets,
    5
  )
  # tickets_cost results ia a numeric object
  expect_equal(
    tickets_costs_annual_ZTM(
      travel_time = 30,
      discount = TRUE,
      zone = 1,
      varsovian = TRUE
    )$tickets_cost,
    625
  )
  # ztm_ticket results is a data.frame object
  expect_is(
    tickets_costs_annual_ZTM(
      travel_time = 30,
      discount = TRUE,
      zone = 1,
      varsovian = TRUE
    )$ztm_ticket,
    "data.frame"
  )
})


test_that(
  "tickets_costs_annual_ZTM errors - value number_of_tickets - handling character input",
  {
    # input - travel_time as character
    expect_error(
      tickets_costs_annual_ZTM(
        travel_time = "30",
        discount = TRUE,
        zone = 1,
        varsovian = TRUE
      )$number_of_tickets,
      regexp = "Error: argument travel_time is not numeric. Please enter travel_time as numeric vector of length 1."
    )
    ## input - discount as character
    expect_error(
      tickets_costs_annual_ZTM(
        travel_time = 30,
        discount = "TRUE",
        zone = 1,
        varsovian = TRUE
      )$number_of_tickets,
      regexp = "Error: argument discount is not logical. Please enter TRUE or FALSE."
    )
    ## input - zone as character
    expect_error(
      tickets_costs_annual_ZTM(
        travel_time = 30,
        discount = TRUE,
        zone = "1",
        varsovian = TRUE
      )$number_of_tickets,
      regexp = "Error: argument zone is not numeric. Please enter zone as integer 1, 2 or 3."
    )
    ## input - varsovian as character
    expect_error(
      tickets_costs_annual_ZTM(
        travel_time = 30,
        discount = TRUE,
        zone = 1,
        varsovian = "TRUE"
      )$number_of_tickets,
      regexp = "Error: argument varsovian is not logical. Please enter TRUE or FALSE."

    )
  }
)
test_that("tickets_costs_annual_ZTM errors - value number_of_tickets - handling NA input",
          {
            # input - travel_time as NA
            expect_error(
              tickets_costs_annual_ZTM(
                travel_time = NA,
                discount = TRUE,
                zone = 1,
                varsovian = TRUE
              )$number_of_tickets,
              regexp = "Error: argument travel_time is not numeric. Please enter travel_time as numeric vector of length 1."
            )
            ## input - discount as NA
            expect_error(
              tickets_costs_annual_ZTM(
                travel_time = 30,
                discount = NA,
                zone = 1,
                varsovian = TRUE
              )$number_of_tickets,
              regexp = "Error: argument discount is not logical. Please enter TRUE or FALSE."
            )
            ## input - zone as NA
            expect_error(
              tickets_costs_annual_ZTM(
                travel_time = 30,
                discount = TRUE,
                zone = NA,
                varsovian = TRUE
              )$number_of_tickets,
              regexp = "Error: argument zone is not numeric. Please enter zone as integer 1, 2 or 3."
            )
            ## input - varsovian as NA
            expect_error(
              tickets_costs_annual_ZTM(
                travel_time = 30,
                discount = TRUE,
                zone = 1,
                varsovian = NA
              )$number_of_tickets,
              regexp = "Error: argument varsovian is not logical. Please enter TRUE or FALSE."
            )
          })
test_that(
  "tickets_costs_annual_ZTM errors - value number_of_tickets - handling input vector of length 2",
  {
    # input - travel_time as c(30,30)
    expect_error(
      tickets_costs_annual_ZTM(
        travel_time = c(30, 30),
        discount = TRUE,
        zone = 1,
        varsovian = TRUE
      )$number_of_tickets,
      regexp = "Error: argument travel_time is not a vector of length 1. Please enter travel_time as numeric vector of length 1."
    )
    ## input - discount as c(TRUE,FALSE)
    expect_error(
      tickets_costs_annual_ZTM(
        travel_time = 30,
        discount = c(TRUE, FALSE),
        zone = 1,
        varsovian = TRUE
      )$number_of_tickets,
      regexp = "Error: argument discount is not a vector of length 1. Please enter TRUE or FALSE."
    )
    ## input - zone as c(1,2)
    expect_error(
      tickets_costs_annual_ZTM(
        travel_time = 30,
        discount = TRUE,
        zone = c(1, 2),
        varsovian = TRUE
      )$number_of_tickets,
      regexp = "Error: argument zone is not a vector of length 1. Please enter integer 1, 2 or 3"
    )
    ## input - varsovian as c(TRUE, FALSE)
    expect_error(
      tickets_costs_annual_ZTM(
        travel_time = 30,
        discount = TRUE,
        zone = 1,
        varsovian = c(TRUE, FALSE)
      )$number_of_tickets,
      regexp = "Error: argument varsovian is not a vector of length 1. Please enter TRUE or FALSE."
    )
  }
)
test_that("tickets_costs_annual_ZTM errors - value tickets_cost - handling character input",
          {
            # input - travel_time as character
            expect_error(
              tickets_costs_annual_ZTM(
                travel_time = "30",
                discount = TRUE,
                zone = 1,
                varsovian = TRUE
              )$tickets_cost,
              regexp = "Error: argument travel_time is not numeric. Please enter travel_time as numeric vector of length 1."
            )
            ## input - discount as character
            expect_error(
              tickets_costs_annual_ZTM(
                travel_time = 30,
                discount = "TRUE",
                zone = 1,
                varsovian = TRUE
              )$tickets_cost,
              regexp = "Error: argument discount is not logical. Please enter TRUE or FALSE."
            )
            ## input - zone as character
            expect_error(
              tickets_costs_annual_ZTM(
                travel_time = 30,
                discount = TRUE,
                zone = "1",
                varsovian = TRUE
              )$tickets_cost,
              regexp = "Error: argument zone is not numeric. Please enter zone as integer 1, 2 or 3."
            )
            ## input - varsovian as character
            expect_error(
              tickets_costs_annual_ZTM(
                travel_time = 30,
                discount = TRUE,
                zone = 1,
                varsovian = "TRUE"
              )$tickets_cost,
              regexp = "Error: argument varsovian is not logical. Please enter TRUE or FALSE."
            )
          })
test_that("tickets_costs_annual_ZTM errors - value tickets_cost - handling NA input",
          {
            # input - travel_time as NA
            expect_error(
              tickets_costs_annual_ZTM(
                travel_time = NA,
                discount = TRUE,
                zone = 1,
                varsovian = TRUE
              )$tickets_cost,
              regexp = "Error: argument travel_time is not numeric. Please enter travel_time as numeric vector of length 1."
            )
            ## input - discount as NA
            expect_error(
              tickets_costs_annual_ZTM(
                travel_time = 30,
                discount = NA,
                zone = 1,
                varsovian = TRUE
              )$tickets_cost,
              regexp = "Error: argument discount is not logical. Please enter TRUE or FALSE."
            )
            ## input - zone as NA
            expect_error(
              tickets_costs_annual_ZTM(
                travel_time = 30,
                discount = TRUE,
                zone = NA,
                varsovian = TRUE
              )$tickets_cost,
              regexp = "Error: argument zone is not numeric. Please enter zone as integer 1, 2 or 3."
            )
            ## input - varsovian as NA
            expect_error(
              tickets_costs_annual_ZTM(
                travel_time = 30,
                discount = TRUE,
                zone = 1,
                varsovian = NA
              )$tickets_cost,
              regexp = "Error: argument varsovian is not logical. Please enter TRUE or FALSE."
            )
          })
test_that(
  "tickets_costs_annual_ZTM errors - value tickets_cost - handling input vector of length 2",
  {
    # input - travel_time as c(30,30)
    expect_error(
      tickets_costs_annual_ZTM(
        travel_time = c(30, 30),
        discount = TRUE,
        zone = 1,
        varsovian = TRUE
      )$tickets_cost,
      regexp = "Error: argument travel_time is not a vector of length 1. Please enter travel_time as numeric vector of length 1."
    )
    ## input - discount as c(TRUE,FALSE)
    expect_error(
      tickets_costs_annual_ZTM(
        travel_time = 30,
        discount = c(TRUE, FALSE),
        zone = 1,
        varsovian = TRUE
      )$tickets_cost,
      regexp = "Error: argument discount is not a vector of length 1. Please enter TRUE or FALSE."
    )
    ## input - zone as c(1,2)
    expect_error(
      tickets_costs_annual_ZTM(
        travel_time = 30,
        discount = TRUE,
        zone = c(1, 2),
        varsovian = TRUE
      )$tickets_cost,
      regexp = "Error: argument zone is not a vector of length 1. Please enter integer 1, 2 or 3"
    )
    ## input - varsovian as c(TRUE, FALSE)
    expect_error(
      tickets_costs_annual_ZTM(
        travel_time = 30,
        discount = TRUE,
        zone = 1,
        varsovian = c(TRUE, FALSE)
      )$tickets_cost,
      regexp = "Error: argument varsovian is not a vector of length 1. Please enter TRUE or FALSE."
    )
  }
)
# test_that("tickets_costs_annual_ZTM errors - value ztm_ticket - handling character input",
#           {
#             # input - travel_time as character
#             expect_error(
#               tickets_costs_annual_ZTM(
#                 travel_time = "30",
#                 discount = TRUE,
#                 zone = 1,
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp = "Error: argument travel_time is not numeric. Please enter travel_time as numeric vector of length 1."
#             )
#             ## input - discount as character
#             expect_error(
#               tickets_costs_annual_ZTM(
#                 travel_time = 30,
#                 discount = "TRUE",
#                 zone = 1,
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp = "Error: argument discount is not logical. Please enter TRUE or FALSE."
#             )
#             ## input - zone as character
#             expect_error(
#               tickets_costs_annual_ZTM(
#                 travel_time = 30,
#                 discount = TRUE,
#                 zone = "1",
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp = "Error: argument zone is not numeric. Please enter zone as integer 1, 2 or 3."
#             )
#             ## input - varsovian as character
#             expect_error(
#               tickets_costs_annual_ZTM(
#                 travel_time = 30,
#                 discount = TRUE,
#                 zone = 1,
#                 varsovian = "TRUE"
#               )$ztm_ticket,
#               regexp = "Error: argument varsovian is not logical. Please enter TRUE or FALSE."
#             )
#           })
# test_that("tickets_costs_annual_ZTM errors - value ztm_ticket - handling NA input",
#           {
#             # input - travel_time as NA
#             expect_error(
#               tickets_costs_annual_ZTM(
#                 travel_time = NA,
#                 discount = TRUE,
#                 zone = 1,
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp =  "Error: argument travel_time is not numeric. Please enter travel_time as numeric vector of length 1."
#             )
#             ## input - discount as NA
#             expect_error(
#               tickets_costs_annual_ZTM(
#                 travel_time = 30,
#                 discount = NA,
#                 zone = 1,
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp =  "Error: argument discount is not logical. Please enter TRUE or FALSE."
#             )
#             ## input - zone as NA
#             expect_error(
#               tickets_costs_annual_ZTM(
#                 travel_time = 30,
#                 discount = TRUE,
#                 zone = NA,
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp =  "Error: argument zone is not numeric. Please enter zone as integer 1, 2 or 3."
#             )
#             ## input - varsovian as NA
#             expect_error(
#               tickets_costs_annual_ZTM(
#                 travel_time = 30,
#                 discount = TRUE,
#                 zone = 1,
#                 varsovian = NA
#               )$ztm_ticket,
#               regexp =  "Error: argument varsovian is not logical. Please enter TRUE or FALSE."
#             )
#           })
# test_that("tickets_costs_annual_ZTM errors - value ztm_ticket - handling input vector of length 2",
#           {
#             # input - travel_time as c(30,30)
#             expect_error(
#               tickets_costs_annual_ZTM(
#                 travel_time = c(30, 30),
#                 discount = TRUE,
#                 zone = 1,
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp = "Error: argument travel_time is not a vector of length 1. Please enter travel_time as numeric vector of length 1."
#             )
#             ## input - discount as c(TRUE,FALSE)
#             expect_error(
#               tickets_costs_annual_ZTM(
#                 travel_time = 30,
#                 discount = c(TRUE, FALSE),
#                 zone = 1,
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp = "Error: argument discount is not a vector of length 1. Please enter TRUE or FALSE."
#             )
#             ## input - zone as c(1,2)
#             expect_error(
#               tickets_costs_annual_ZTM(
#                 travel_time = 30,
#                 discount = TRUE,
#                 zone = c(1, 2),
#                 varsovian = TRUE
#               )$ztm_ticket,
#               regexp = "Error: argument zone is not a vector of length 1. Please enter integer 1, 2 or 3"
#             )
#             ## input - varsovian as c(TRUE, FALSE)
#             expect_error(
#               tickets_costs_annual_ZTM(
#                 travel_time = 30,
#                 discount = TRUE,
#                 zone = 1,
#                 varsovian = c(TRUE, FALSE)
#               )$ztm_ticket,
#               regexp = "Error: argument varsovian is not a vector of length 1. Please enter TRUE or FALSE."
#             )
#           })
