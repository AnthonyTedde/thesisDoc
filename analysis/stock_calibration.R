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
rm(list = ls())
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
tikzDevice::tikz(file = "figures/appl.logreturns.density.tex", width = 4, height = 2)
ggplot() +
  stat_density(data = data.frame(u), aes(u),
               geom = "line",
               colour = 'steelblue') +
  xlab("Log-returns") + ylab("Density")
dev.off()
setwd("c:/Users/ATE/thesisDoc/data")

########################################################
# Heston on the same period with risk neutral data
########################################################
h <- heston(initial_stock_price = quote[1],
            initial_volatility = x["v0"],
            time_to_maturity = 1,
            seed = 1,
            scale = 365,
            alpha = 0.02160,
            rho = x["rho"],
            kappa = x["kappa"],
            theta = x["theta"],
            sigma = x["sigma"])

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
tikzDevice::tikz(file = "figures/appl.logreturns.density.heston.riskneutral.tex", width = 4, height = 2)
ggplot() +
  stat_density(data = data.frame(u), aes(u),
               geom = "line",
               colour = 'steelblue')+
  stat_density(data = data.frame(u_heston),aes(u_heston),
               geom = "line",
               colour = 'darkred')+
  xlab("log-return") + ylab("Density")
dev.off()
setwd("c:/Users/ATE/thesisDoc/data")
  ########################################################################
  # fit the distrib
  ########################################################################
  cst <- function(lambda){
    # print(lambda)
    sub <- c(rep(T,length(u)),rep(F,length(u_heston) -length(u)))
    sub <- sample(sub, replace = F)
    kappa <- x["kappa"] - lambda
    theta <- x["theta"] * x["kappa"] / kappa
    print(kappa)
    quote <- heston(initial_stock_price = quote[1],
           initial_volatility = x["v0"],
           time_to_maturity = 1,
           seed = 1,
           scale = length(u) - 1,
           alpha = 0.02160,
           rho = x["rho"],
           kappa = kappa,
           theta = theta,
           sigma = x["sigma"])$stock_price_path

    
    
    quotes <- length(quote)
    quotediff <- quote[-1] / quote[-quotes]
    
    log(quotediff)
  }
  

  
  dens <- function(y, alpha = 0, lambda, ...){
    # print(alpha)
    # print(lambda)
    kappa <- x["kappa"] - lambda
    theta <- x["theta"] * x["kappa"] / kappa
    hc <- heston_characteristic(initial_stock_price = u[1],
                                initial_volatility = x["v0"],
                                time_to_maturity = t,
                                alpha = alpha,
                                rho = x["rho"],
                                kappa = kappa,
                                theta = theta,
                                sigma = x["sigma"])
    # print(hc(1))
    # function(y){
      # print("ici")
      unlist(map(y, function(x){ 
        re_p <- function(w){
          Re(exp(-1i  * x *w) * hc(w))
        }
        im_p <- function(w){
          Im(exp(-1i  *x * w) * hc(w))
        }
        # return(1/(2*pi) * unname(integral(p, -Inf, Inf)[1]))
        i <- 1/(2*pi) * (integrate(re_p, -Inf, Inf)$value +
          1i * integrate( im_p, -Inf, Inf)$value)
        # print(i)
        return(i)
      }))
      # }
    
  }
  
  MASS::fitdistr(x = u, densfun = dens, start = list(alpha = 0, lambda = .5), 
                 lower = c(0.1, -6), upper = c(2, 4))
  
  MASS::fitdistr(x = u, densfun = dens, start = list(alpha = ubar, lambda = .5),
                 lower = c(0, -5), upper = c(0.1, 4))
  
  
  MASS::fitdistr(x = u, densfun = dens, start = list(alpha = ubar), lambda = .5,
                 lower = c(-5), upper = c(4))
  
  # MASS::fitdistr(x = u, densfun = dens, start = list(lambda = .5), 
  #                lower = -2, upper = 2)
  # 
  ggplot() +
    stat_density(data = data.frame(u), aes(u),
                 geom = "line",
                 colour = 'steelblue')+
  stat_function(fun = dens,
                colour = "black",
                args = list(
                  alpha = 0,
                  lambda = 2 ))
  
  
  
  
  # 
  # 1.0953862751   -2.0000000000 
  # alpha    
  # -4.7883278229 
  # ( 0.0003861428
    
  kappa <- x["kappa"] + 5.4883278229
  theta <- x["theta"] * x["kappa"] / kappa
  # sigma <- x["v0"] / sqrt(t)
  alpha <- 4.496164e-04 / t + sigma ^2 / 2
  h <- map(1:150, ~ heston(initial_stock_price = 115.5450,
              initial_volatility = x["v0"],
              time_to_maturity = 1,
              seed = .x,
              scale = 365,
              alpha = 0.4822917,
              rho = x["rho"],
              kappa = kappa,
              theta = theta,
              sigma = x["sigma"]))
  
  quote <- map(h, ~ .x$stock_price_path[!is.na(.x$stock_price_path)])
  
  

  
  quotediff <- map(quote, function(x){
    quotes <- length(x)
    x[-1] / x[-quotes]
  }) %>% unlist
  
  u_heston <- log(quotediff)
  
  


  
  setwd("c:/Users/ATE/thesisDoc")
  tikzDevice::tikz(file = "figures/appl.logreturns.density.heston.riskaverse.tex", width = 4, height = 2)
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
  
  
  
  
  heston_probability <- function(z,
                                 t,
                                 kappa,
                                 rho,
                                 sigma,
                                 theta,
                                 lambda = 0){
    # kappa <- kappa - lambda
    # theta <- theta * x["kappa"] / kappa
    integrand <- function(w, iix){
      gamma <- kappa + 1i * rho * sigma * w
      omega <- sqrt(gamma ^2 + sigma ^2 * (w^2 - 1i * w))
      Ft <- kappa * theta / sigma ^ 2 * gamma * t -
        2 * kappa * theta / sigma ^ 2 *
        log(
          cosh(omega * t / 2) +
            (omega ^2 - gamma ^2 + 2 * kappa * gamma) / (2 * kappa * omega) * 
            sinh(omega * t / 2)
        )
      Re(exp(1i * w * x + Ft))
      # exp(1i * w * x + Ft)
    }
    map_dbl(z, function(y){
      1/(2 * pi) * integrate(integrand, -Inf, Inf, iix = y,subdivisions = 5000,
                              rel.tol = 1.5e-8)$value
      
    })
  }
  
  MASS::fitdistr(x = u, densfun = heston_probability, 
                 start = list(lambda = 0),
                 t = t,
                 rho = x['rho'],
                 sigma = x['sigma'],
                 kappa = x['kappa'], 
                 theta = x['theta'],
                 lower = c(-5), upper = c(5))
  
  # TEST
  heston_probability(z = c(0, 0.001, 0.002, 0.025),
                     t = t,
                     kappa = x['kappa'],
                     rho = x['rho'],
                     sigma = x['sigma'],
                     theta = x['theta'])
  
  kappa <- x["kappa"] + 4.7883278229
  theta <- x["theta"] * x["kappa"] / kappa 
  heston_probability(z = c(0),
                     t = t,
                     kappa = kappa,
                     rho = x['rho'],
                     sigma = x['sigma'],
                     theta = theta)
  
  
  
  
  
  
  dens <- function(y, alpha, lambda){
    kappa <- x["kappa"] - lambda
    theta <- x["theta"] * x["kappa"] / kappa
    hc <- heston_characteristic(initial_stock_price = u[1],
                                initial_volatility = x["v0"],
                                time_to_maturity = t,
                                alpha = alpha,
                                rho = x["rho"],
                                kappa = kappa,
                                theta = theta,
                                sigma = x["sigma"])

    unlist(map(y, function(x){ 
      re_p <- function(w){
        Re(exp(-1i  * x *w) * hc(w))
      }
      im_p <- function(w){
        Im(exp(-1i  *x * w) * hc(w))
      }
      i <- 1/(2*pi) * (integrate(re_p, -Inf, Inf, subdivisions = 2000)$value +
                         1i * integrate( im_p, -Inf, Inf, subdivisions = 2000)$value)
      return(i)
    }))

    
  }
  
   
  alpha <- 0.001268801 

  theta <- 0.04871543 

  kappa <- 4.001055 
  dens(0.005050189, 0.001268801 ,0.04871543,4.001055 )

  
  
  MASS::fitdistr(x = u, densfun = dens, start = list(lambda = .5), alpha = 0.001268801, 
                 lower = c(-4), upper = c(0))
  
  ggplot(data.frame(u)) +
    stat_density(aes(u),
                 geom = "line",
                 colour = 'steelblue')+
    stat_density(data = data.frame(u_heston),aes(u_heston),
                 geom = "line",
                 colour = 'darkred')+

  
  
  #   kappa <- x["kappa"] + 5.4883278229
  # theta <- x["theta"] * x["kappa"] / kappa
  # alpha <- c('alpha' = 0.4822917)
  # 
  # 
  # heston_riskaverse <- c(kappa, theta, alpha)
  # 
  # setwd("c:/Users/ATE/thesisDoc/data")
  # 
  # save(heston_riskaverse, file = "optimalHestonRiskaverse.RData")
  
  
  #XTABLE
  
  a <- AAPL[, c("date", "ticker", "open", "close", "low", "high")]
  a <- data.frame(date = as.character(a$date), a[, -1])

  print(xtable::xtable(a, caption = "Market stock data (AAPL)", 
                 label = "t:market:stock"),
  include.rownames = FALSE,
  include.colnames = FALSE)
  

  x