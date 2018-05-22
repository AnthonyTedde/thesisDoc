library(purrr) 
library(StockPriceSimulator)
library(ggplot2)  
library(Quandl)
library(dplyr)

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

###################
## Download data ##
###################
Quandl.api_key("VZSMB9uWUzm_VBrugWy5") 
AAPL <- Quandl.datatable('WIKI/PRICES', date = days, ticker = "AAPL")

##################
## Extract data ##
##################
quote <- AAPL$close
day <- AAPL$date
days <- length(day)
interval <- day[-1] - day[-days]

quotes <- length(quote)
quotediff <- quote[-1] / quote[-quotes]

df <- data.frame(quotediff, interval)
u <- log(filter(df, interval ==1 )$quotediff)


## u <- log(quote[-1] / quote[-quotes])
ubar <- mean(u)

s <- sqrt(1/(length(u) - 1) * sum((u - ubar)^2)) 

sigma <- s / sqrt(t)
mean <- ubar / t + sigma ^2 / 2

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
