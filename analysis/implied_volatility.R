library(purrr) 
library(StockPriceSimulator)
library(ggplot2)  
library(Quandl)
library(dplyr)
library(fOptions)
library(tidyverse)
library(pracma)
library(FME)

ggplot(DATA, aes(Strike, price)) + 
  geom_line() +
  geom_point(data = DATA, aes(Strike, Last))+
  facet_grid(. ~ maturity)


price_heston <- map_dbl(as.data.frame(t(DATA)), function(y){
  call_heston(initial_stock_price = y[1], 
              initial_volatility = x['v0'],
              theta = x['theta'],
              kappa = x['kappa'],
              sigma = x['sigma'],
              alpha = y[8],
              rho = x['rho'],
              time_to_maturity = y[2]/365,
              K = y[3])
}) %>% unname

DATA <- data.frame(DATA, price_heston)

##############################################################
#  Compute the implied volatility
# GBSVolatility(price, TypeFlag, S, X, Time, r, b, tol, maxiter)
##############################################################

imply.volatility <- map_dbl(as.data.frame(t(DATA)), function(y){
  GBSVolatility(price = y[9], 
                TypeFlag = 'c',
                S = y[1],
                X = y[3],
                Time = y[2]/365,
                r = y[8],
                b = 0)
}) %>% unname


imply.volatility_heston <- map_dbl(as.data.frame(t(DATA)), function(y){
  GBSVolatility(price = y[10], 
                TypeFlag = 'c',
                S = y[1],
                X = y[3],
                Time = y[2]/365,
                r = y[8],
                b = 0)
}) %>% unname

DATA <- data.frame(DATA, imply.volatility, imply.volatility_heston)


####################################################################
# plot
####################################################################

ggplot(DATA, aes(Strike, imply.volatility)) + 
  geom_line() +
  geom_point(data = DATA, aes(Strike, imply.volatility_heston))+
  facet_grid(. ~ maturity)





