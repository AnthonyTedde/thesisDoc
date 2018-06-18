library(purrr) 
library(StockPriceSimulator)
library(ggplot2)  
library(Quandl)
library(dplyr)
library(fOptions)
library(tidyverse)


## 0. prerequisites
rm(list = ls())
setwd("/Users/anthony/workspace/thesis/thesis/data")


###############
## Variables ##
###############
# Convention ACT360 used
t <- 1 / 365
## day represents the number of days from the start date where the data
## should be downladed
day <-  365 
dt <- day ** -1
date_sequence <- seq(ISOdate(2017, 5, 18), ISOdate(2018, 5, 18), by = 'day')

days <- seq(ISOdate(2017,01,01), by = "day", length.out = day) %>%
  format("%F") %>%
  paste(collapse = ",")


########################################################################
##  Hedging strategy using GBM 
########################################################################
# Hedging performance in a perfectly log-normal world
#
# 1. Get all the maturity and all the strike prices
#
load(file="option-quote.RData")

# 1.1. All the strike price available for all the maturity
avail_stikes <- map(AAPL_o, `$`, calls) %>%
  map(`$`, Strike) %>%
  unlist %>% unname %>% unique %>% sort

# 1.2 Covered maturities
maturity <- names(AAPL_o)

# 1.3 Construct the price surface
dfs_call <- map(AAPL_o, `$`, call) %>%
  map(~.x[, 1:2]) %>%
  Filter(f = Negate(is.null))

price_surface <- dfs_call[[1]]
for(i in names(dfs_call[-1])){
  price_surface <- merge(price_surface, dfs_call[[i]], by = "Strike", all = T)
}
colnames(price_surface) <- c("Strike", names(dfs_call)) 
# First subsetting of the price surface
price_surface <- structure(price_surface[23:57, -4])
rownames(price_surface) <- 1:nrow(price_surface)

# 1.4 Convert the maturity date based on the price surface
maturity <- names(price_surface)[-1] %>%
  as.Date(format = "%b.%d.%Y") - as.Date("2018-05-18")

# 1.5 Compute the volatility surface
volatility_surface <- vector("list", length(price_surface))
volatility_surface[[1]] <- price_surface[1]
for(i in seq_along(names(price_surface)[-1])){
  a <-  rbind(price_surface[[i + 1]],
              maturity[i] / 365,
              price_surface$Strike,
              AAPL$Last)
  a <- map(as.data.frame(a), .f = function(x){
    structure(as.list(x),
              names = c("price", "Time", "X", "S" ))
  })
  b <- a %>%
    map(c,TypeFlag = "c", r = .05, b= 0) %>%
    map(.f = function (x){
      if(!is.na(x$price))
        tryCatch(do.call(GBSVolatility, x),
                 error = function(c) NA)
      else NA
    })
  volatility_surface[[i + 1]] <- unname(unlist(b))
}

volatility_surface <- as.data.frame(volatility_surface) %>% round(digit = 4)
names(volatility_surface) <- names(price_surface)
volatility_surface


maturity <- as.double(maturity)
strike <- volatility_surface$Strike

maturity_mtrx <- matrix(maturity, ncol = 9, byrow = T)
strike_mtrx <- matrix(strike, ncol = 7, byrow = T)

library("xtable")
print(
  xtable(data.frame("maturity" = maturity_mtrx),
         caption = "Maturities explored during the hedging performance measurement"),
  include.rownames = FALSE,
  include.colnames = F
)


print(
  xtable(data.frame("strike" = strike_mtrx),
         caption = "strike prices explored during the hedging performance measurement"),
  include.rownames = FALSE,
  include.colnames = F
)













