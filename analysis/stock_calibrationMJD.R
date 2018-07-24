library(purrr) 
detach("package:StockPriceSimulator", unload = T)
library(StockPriceSimulator)
library(ggplot2)  
library(Quandl)
library(dplyr)
library(fOptions)
library(tidyverse)
library(pracma)
library(quantmod)
library(MASS)
library(elliptic)
# library(FME)


## 0. prerequisites
# rm(list = ls())
setwd("c:/Users/ATE/thesisDoc/data")

load(file = "optimalHestonCalibration.RData")


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

###################
## Download data ##
###################
## 1. Load the data
## Option Datalist 18 May 16h00 (FRIDAY)
##
Quandl.api_key("VZSMB9uWUzm_VBrugWy5") 
AAPL <- Quandl.datatable('WIKI/PRICES', date = days, ticker = "AAPL")

##################
## Extract data ##
##################
quote <- (AAPL$high + AAPL$low)/2
day <- AAPL$date
days <- length(day)
interval <- day[-1] - day[-days]

quotes <- length(quote)
quotediff <- quote[-1] / quote[-quotes]

df <- data.frame(quotediff, interval)
u <- log(dplyr::filter(df, interval ==1 )$quotediff)


## u <- log(quote[-1] / quote[-quotes])
ubar <- mean(u)

s <- sqrt(1/(length(u) - 1) * sum((u - ubar)^2)) 

#
# Sigma and mean will be used inside the GBM simulation
#
sigma <- s / sqrt(t)
alpha <- ubar / t + sigma ^2 / 2


########################################################
# Plot the graph for log-return only
########################################################
setwd("c:/Users/ATE/thesisDoc")
# tikzDevice::tikz(file = "figures/appl.logreturns.density.tex", width = 6, height = 3)
ggplot() +
  stat_density(data = data.frame(u), aes(u),
               geom = "line",
               colour = 'steelblue') +
  xlab("log-returns")
# dev.off()
setwd("c:/Users/ATE/thesisDoc/data")

########################################################
# Heston on the same period with risk neutral data
########################################################
h <- sstock_jump(initial_stock_price = 115.5450,
                 time_to_maturity = 1,
                 seed = 1,
                 scale = 365,
                 sigma = x_merton['sigma'],
                 alpha = 0.02400,
                 lambda = x_merton['lambda'],
                 jumps_intensity_parameters = list(mean = x_merton['mu'],
                                                   sd = x_merton['delta'])
                 )

quote <- h$stock_price_path


quotes <- length(quote)
quotediff <- quote[-1] / quote[-quotes]

u_heston <- log(quotediff)

# 
# ## u <- log(quote[-1] / quote[-quotes])
# ubar <- mean(u)
# 
# s <- sqrt(1/(length(u) - 1) * sum((u - ubar)^2)) 

#
# Sigma and mean will be used inside the GBM simulation
#
# sigma <- s / sqrt(t)
# alpha <- ubar / t + sigma ^2 / 2



setwd("c:/Users/ATE/thesisDoc")
tikzDevice::tikz(file = "figures/appl.logreturns.density.merton.riskneutral.tex", width = 4, height = 2)
ggplot() +
  stat_density(data = data.frame(u), aes(u),
               geom = "line",
               colour = 'steelblue')+
  stat_density(data = data.frame(u_heston),aes(u_heston),
               geom = "line",
               colour = 'darkred')+
  xlab("log-returns")
dev.off()
setwd("c:/Users/ATE/thesisDoc/data")
















########################################################################
# fit the distrib
# FOllowing: https://arxiv.org/pdf/cond-mat/0203046.pdf
########################################################################
(2^31-1)

lambda <- 1
t <- 1/365
sigma = 0.1958536
alpha = 0.4822917
mu = x_merton['mu']
delta <- x_merton['delta']
y <- seq(-.02, .02, by = .001)


Merton_logreturns_probability <- function(y,
                                          lambda = 1,
                                          t = 1/365,
                                          sigma = 0.1958536,
                                          alpha = 0.4822917,
                                          mu = x_merton['mu'],
                                          delta= x_merton['delta']
                                          ){
  i <- 0:100
  a <- exp(-lambda * t) * (lambda * t) ^ (i) / (factorial(i))
  
  k <- exp(mu + 0.5 * delta^2) - 1
  mean <- (alpha - sigma^2/2 - lambda * k) * t + i * mu
  sd <- sigma^2 * t + i * delta^2
  map_dbl(y, ~ sum(a * dnorm(.x, mean, sqrt(sd))))
}




r <- MASS::fitdistr(x = u, densfun = Merton_logreturns_probability , 
                    start = list(
                      mu = x_merton['mu'],
                      lambda = x_merton['lambda'],
                      alpha = 0.02400,
                      delta = x_merton['delta'],
                      sigma = x_merton['sigma']
                    ),
                    t = 1/365)
# ,  lower = c(-0.05, 0, 0, 0, 0), upper = c(0.05, 80, 0.5, 0.5, 0.5))

#############################################
# plot
#############################################
setwd("c:/Users/ATE/thesisDoc")
tikzDevice::tikz(file = "figures/appl.logreturns.density.merton.riskaverse.tex", width = 4, height = 2)

r[[1]]['delta'] <- abs(r[[1]]['delta'])
ggplot(data = data.frame(u)) +
  stat_density(aes(u),
               geom = "line",
               colour = 'steelblue')+
  # stat_density(data = data.frame(u_heston),aes(u_heston),
  #              geom = "line",
  #              colour = 'darkred')+
  stat_function(fun = Merton_logreturns_probability,
                colour = "darkred",
                args = as.list(c(r[[1]], t = 1/365))
  )

dev.off()
setwd("c:/Users/ATE/thesisDoc/data")


################################################################################
# generation from empirical dummy log-return
################################################################################

h <- map(1:150, ~ sstock_jump(initial_stock_price = 115.5450,
                              time_to_maturity = 1,
                              seed = .x,
                              scale = 365,
                              sigma = 0.1020368856,#x_merton['sigma'],
                              alpha = 0.4816753642,
                              lambda = 99.5434345733,
                              jumps_intensity_parameters = list(mean = -0.0006791266,
                                                                sd = 0.0161092106)))



quote <- map(h, ~ .x$stock_price_path[!is.na(.x$stock_price_path)])




quotediff <- map(quote, function(x){
  quotes <- length(x)
  x[-1] / x[-quotes]
}) %>% unlist

u_heston <- log(quotediff)

# setwd("c:/Users/ATE/thesisDoc")
# tikzDevice::tikz(file = "figures/appl.logreturns.density.heston.riskaverse.tex", width = 6, height = 3)
ggplot() +
  stat_density(data = data.frame(u), aes(u),
               geom = "line",
               colour = 'steelblue')+
  stat_density(data = data.frame(u_heston),aes(u_heston),
               geom = "line",
               colour = 'darkred')+
  xlab("log-returns") + xlim(-0.04, 0.04)


################################################################################
# save
################################################################################

setwd("c:/Users/ATE/thesisDoc/data")
merton_riskaverse <- r[[1]]
save(merton_riskaverse , file = "optimalMertonRiskaverse.RData")


