rm(list = ls())

library(foreign)
library(dplyr)
library(readr)
library(tseries)
library(vars)
library(forecast)
library(gdata)
library(lubridate)
library(zoo)
library(PerformanceAnalytics)
library(psych)
library(ggplot2)
library(stargazer)
library(readxl)
library(xts)
library(ggplot2)

# Import the dataset
datafile <- "Data/FX-carry_set_source  - CARRYOVERVIEW.xlsx"
FXspot <- read_excel(datafile, sheet = 1, skip = 6, col_types = "numeric")
FXspot_bid <- read_excel(datafile, sheet = 2, skip = 6, col_types = "numeric")
FXspot_ask <- read_excel(datafile, sheet = 3, skip = 6, col_types = "numeric")
FX1M_bid <- read_excel(datafile, sheet = 4, skip = 6, col_types = "numeric")
FX1M_ask <- read_excel(datafile, sheet = 5, skip = 6, col_types = "numeric")

FXspot$Dates <- as.Date(FXspot$Dates, origin = "1899-12-30")
FXspot_bid$Dates <- as.Date(FXspot_bid$Dates, origin = "1899-12-30")
FXspot_ask$Dates <- as.Date(FXspot_ask$Dates, origin = "1899-12-30")
FX1M_bid$Dates <- as.Date(FX1M_bid$Dates, origin = "1899-12-30")
FX1M_ask$Dates <- as.Date(FX1M_ask$Dates, origin = "1899-12-30")

FXspot <- FXspot[year(FXspot$Dates) >= 1997, ]
FXspot_bid <- FXspot_bid[year(FXspot_bid$Dates) >= 1997, ]
FXspot_ask <- FXspot_ask[year(FXspot_ask$Dates) >= 1997, ]
FX1M_bid <- FX1M_bid[year(FX1M_bid$Dates) >= 1997, ]
FX1M_ask <- FX1M_ask[year(FX1M_ask$Dates) >= 1997, ]

FXspot <- head(FXspot, -7)
FXspot_bid <- head(FXspot_bid, -7)
FXspot_ask <- head(FXspot_ask, -7)
FX1M_bid <- head(FX1M_bid, -7)
FX1M_ask <- head(FX1M_ask, -7)

# Change the forward points as price increments

FX1M_bid_nodate <- FX1M_bid[, -1]
FX1M_ask_nodate <- FX1M_ask[, -1]
FXspot_nodate <- FXspot[, -1]
FXspotbid_nodate <- FXspot_bid[, -1]
FXspotask_nodate <- FXspot_ask[, -1]

# 1M forward
FX1M_bid_nodate <- FX1M_bid_nodate / 10000
FX1M_ask_nodate <- FX1M_ask_nodate / 10000

FX1M_bid_nodate$`EURJPY1M Curncy` <- FX1M_bid_nodate$`EURJPY1M Curncy` * 100
FX1M_ask_nodate$`EURJPY1M Curncy` <- FX1M_ask_nodate$`EURJPY1M Curncy` * 100

FX1M_bid_price <- FX1M_bid_nodate + FXspotbid_nodate
FX1M_ask_price <- FX1M_ask_nodate + FXspotask_nodate


# Add dates
FX1M_bid_price$Dates <- FXspot$Dates
FX1M_ask_price$Dates <- FXspot$Dates

# Shift dates to the first
FX1M_bid_price <- FX1M_bid_price[, c(ncol(FX1M_bid_price), 1:(ncol(FX1M_bid_price) - 1))]
FX1M_ask_price <- FX1M_ask_price[, c(ncol(FX1M_ask_price), 1:(ncol(FX1M_ask_price) - 1))]

#Create Time-Series
FX1M_bid_price_ts <- xts(FX1M_bid_price[, - 1], order.by = FX1M_bid_price$Dates)
FX1M_ask_price_ts <- xts(FX1M_ask_price[, - 1], order.by = FX1M_ask_price$Dates)
FXspot_ts <- xts(FXspot[, - 1], order.by = FXspot$Dates)
FXspot_bid_ts <- xts(FXspot_bid[, - 1], order.by = FXspot_bid$Dates)
FXspot_ask_ts <- xts(FXspot_ask[, - 1], order.by = FXspot_ask$Dates)

#Function for last day of the month:
last_make_month <- function(v_ts) {
  data_1 <- as.data.frame(v_ts)
  data_1 <- data_1 %>%
    mutate(Date = as.Date(index(v_ts)))

  data_2 <- data_1 %>%
    mutate(month = month(Date), #mutating for weeks as well, grouping my week, month year gives last day of the weeks (it should, haven't tried it yet)
           year = year(Date)) %>%
    group_by(month, year) %>%
    filter(Date == max(Date))   #max date gives last day

  v_ts <- xts(data_2, order.by = data_2$Date)
  v_ts
}

#Function for first day
first_make_month <- function(v_ts) {
  data_1 <- as.data.frame(v_ts)
  data_1 <- data_1 %>%
    mutate(Date = as.Date(index(v_ts)))
  data_2 <- data_1 %>%
    mutate(month = month(Date), #mutating for weeks as well, grouping my week, month year gives last day of the weeks (it should, haven't tried it yet)
           year = year(Date)) %>%
    group_by(month, year) %>%
    filter(Date == min(Date))   #min date gives first day
  v_ts <- xts(data_2, order.by = data_2$Date)
  v_ts
}
#Get last day of month
FX1M_bid_price_ts_last <- last_make_month(FX1M_bid_price_ts)
FX1M_ask_price_ts_last <- last_make_month(FX1M_ask_price_ts)
FXspot_ts_last <- last_make_month(FXspot_ts)
FXspot_bid_ts_last <- last_make_month(FXspot_bid_ts)
FXspot_ask_ts_last <- last_make_month(FXspot_ask_ts)

#Get first day of month
FX1M_bid_price_ts_first <- first_make_month(FX1M_bid_price_ts)
FX1M_ask_price_ts_first <- first_make_month(FX1M_ask_price_ts)
FXspot_ts_first <- first_make_month(FXspot_ts)
FXspot_bid_ts_first <- first_make_month(FXspot_bid_ts)
FXspot_ask_ts_first <- first_make_month(FXspot_ask_ts)

#Monthly Return Function for TS
#fwd_sign and spt_sign for long is -1 and 1, for short is 1 and -1 -> makes sure we add the markup properly
FXReturn <- function(Spot_dta, Forward_dta, fwdTC_bps, sptTC_bps, tenureTC, fwd_sign, spt_sign) {
  Tenure_multiplier <- tenureTC / 12
  SpotTC <- sptTC_bps / 10000
  ForwardTC <- fwdTC_bps / 10000

  Spot <- Spot_dta
  Spot_date <- index(Spot)                      #Creating Date index to use it as EOM Return
  Spot <- coredata(Spot)
  Spot <- data.frame(Spot)
  Spot <- Spot[, 1:(length(Spot) - 3)] # last three columns are date, month and year from TS function
  
  Forward <- Forward_dta
  Forward <- coredata(Forward)
  Forward <- data.frame(Forward)
  Forward <- Forward[, 1 :(length(Forward) - 3)]

  i <- c(seq_len(length(Spot)))
  Spot[, i] <- apply(Spot[, i], 2,             # Specify own function within apply to change full Data Frame to Numeric
                      function(x) as.numeric(as.character(x)))
  ii <- c(seq_len(length(Forward)))
  Forward[, ii] <- apply(Forward[, ii], 2,       # Specify own function within apply to change full Data Frame to Numeric; before was only i in bracets not ii
                          function(x) as.numeric(as.character(x)))

  Spot[is.na(Spot)] <- 0                          #necessary, in order to be able to calculate. Change back afterwards.
  Forward[is.na(Forward)] <- 0

  Spot <- Spot * (1 + ((SpotTC * Tenure_multiplier) * spt_sign))
  Forward <- Forward * (1 + (((ForwardTC * Tenure_multiplier) + (SpotTC * Tenure_multiplier)) * fwd_sign))
  Return <- Forward / Spot                        #Spot in t and Forward in t-30 or Spot EOM and Forward BOM



  Return <- do.call(data.frame,                      # Replace Inf, NaN in data by NA
                    lapply(Return,
                           function(x) replace(x, !is.finite(x), NA)))
  Return[Return == 0] <- NA
  Return <- xts(Return - 1, order.by = Spot_date)

}