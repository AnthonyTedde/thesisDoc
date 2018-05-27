library(purrr) 
library(StockPriceSimulator)
library(ggplot2)  
library(quantmod)
library(dplyr)

##
# Some variable cursors
#
from_date <- "2017-01-01"
to_date <- "2018-05-18"
dt <- 1/365

##
# Load the time series
#
getSymbols.yahoo("AAPL",
                 env = .GlobalEnv,
                 return.class = 'data.frame',
                 index.class = 'Date',
                 from = from_date,
                 to = to_date)
quote <- AAPL$AAPL.Adjusted

##
# Compute the empirical volatility
#
n <- length(quote)
quotediff <- quote[-1] / quote[-n]
day <- rownames(AAPL)
ndays <- length(day)
interval <- as.datetime(day[-1]) - day[-ndays]

df <- data.frame(quotediff, interval)
u <- log(filter(df, interval ==1 )$quotediff)

alpha_bar <- mean(quote)
sigma_bar <- sd(quote)

sigma <- sigma_bar / sqrt(dt)
alpha <- alpha_bar / dt + sigma ^2 / 2
   

##
# Estimate the volatility drift
#