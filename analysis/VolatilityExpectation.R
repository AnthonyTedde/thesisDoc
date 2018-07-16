library(dplyr)
library(StockPriceSimulator)
library(ggplot2)
library(purrr)
###############
## Variables ##
###############
# Convention ACT360 used
t <- 1 / 360
## day represents the number of days from the start date where the data
## should be downladed
day = 365 
days <- seq(ISOdate(2015,1,12), by = "day", length.out = day) %>%
  format("%F") %>%
  paste(collapse = ",")

# Load data
setwd("c:/Users/ATE/thesisDoc/data")
load("S_list.RData")

S_list[[1]]
  
##################
## compute data ##
##################
moment <- map(S_list, .f = function(x){
  quote <- x$close
  day <- x$date
  days <- length(day)
  interval <- day[-1] - day[-days]
  
  quotes <- length(quote)
  quotediff <- quote[-1] / quote[-quotes]
  
  df <- data.frame(quotediff, interval)
  u <- log(dplyr::filter(df, interval ==1 )$quotediff)
  
  
  ubar <- mean(u)  
  s <- sqrt(1/(length(u) - 1) * sum((u - ubar)^2)) 
  
  sigma <- s / sqrt(t)
  mean <- ubar / t + sigma ^2 / 2
  c(sigma, mean) 
})

std <- map_dbl(moment, `[`, 1)

quantile(std, probs = c(.05, .5, .95))

# plot the distribution of the moment
ggplot(data.frame(std)) +
  stat_density(aes(std),
               geom = "line",
               colour = 'steelblue')

# OPTIONAL
######################################################################
## Simulate a Geometric Brownian Motion based on the collected data ##
######################################################################
GBM <- sstock(initial_stock_price = quote[1],
              alpha = mean,
              sigma = sigma,
              time_to_maturity = 1,
              scale = 360)
# and compute the log-return
GBM_price <- GBM$stock_price_path
GBM_prices <- length(GBM_price)
GBM_logreturn <- log(GBM_price[-1] / GBM_price[-GBM_prices])

  
ggplot(data.frame(u)) +
  stat_density(aes(u),
               geom = "line",
               colour = 'steelblue') +
  stat_density(data = data.frame(GBM_logreturn), aes(GBM_logreturn),
               geom = "line",
               colour = 'darkred') +
  stat_function(fun = dnorm,
                colour = "black",
                args = list(
                  mean = ubar,
                  sd = sigma * sqrt(t) ))