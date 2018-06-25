library(purrr) 
library(StockPriceSimulator)
library(ggplot2)  
library(Quandl)
library(dplyr)
library(fOptions)
library(tidyverse)
library(pracma)
library(FME)
###############################################################################
# calibration of the heston model 
###############################################################################
#######################################3
# Creation of the data table
#######################################3
setwd("c:/Users/ATE/thesisDoc/data")
load(file="option-quote.RData")

# 1.1. All the strike price available for all the maturity
avail_stikes <- map(AAPL_o, `$`, calls) %>%
  map(`$`, Strike) %>%
  unlist %>% unname %>% unique %>% sort

# 1.2 Covered maturities
maturity <- names(AAPL_o)

# 1.3 Construct the price surface
dfs_call <- map(AAPL_o, `$`, call) %>%
  map(~.x[.x$Strike >= 130 & .x$Strike <= 222.5, c("Strike", "Last", "Bid", "Ask")]) %>%
  Filter(f = Negate(is.null))

lst_call <- map(names(dfs_call), function(x){
  maturity <- as.Date(x, format = "%b.%d.%Y") - as.Date("2018-05-18")
  row.names(dfs_call[[x]]) <- NULL
  y <- dfs_call[[x]]
  structure(data.frame(maturity = rep(maturity, nrow(y)), y))
})


## GET A UNIQUE DATAFRAME CONTAINING OPTION PRICE
call <- data.frame("S0" = AAPL$Last, do.call("rbind", lst_call))

# 1.3 Riskless rate
m <- unique(call$maturity) %>% sort
r <- c(0.415, 1.25, 1.69, 1.79, 1.90, 1.97, 2.03, 2.09, 2.16, 2.40) / 100
continuous <- 365/m * log(1 + r * (m/365))
rate <- data.frame(maturity = m, riskless = r, continuous = continuous)

DATA <- plyr::join(call, rate)
DATA <- DATA[DATA$maturity >= 63 & DATA$maturity <= 399 & 
               DATA$Strike %in% c(130, 140, 150, 160, 170, 180, 190, 200, 210, 220), ]
rownames(DATA) <- 1:nrow(DATA)

cost <- function(x){
  cst <- vector("double", length = nrow(DATA)) 
  
  for(i in 1:nrow(DATA)){
    cst[i] <- DATA[i, 4] - call_heston(DATA[i, 1], 
                                       x[1],
                                       x[2],
                                       # (x[5] + x[3]^2) / (2 * x[2]),
                                       x[5],
                                       x[3],
                                       DATA[i, 8],
                                       x[4],
                                       DATA[i, 2] / 365,
                                       DATA[i, 3])
  }

  print(sum(cst^2))
  print(x)
  
  sum(cst^2)
  
}

# Initial valued vector
  # OK:
# GOOD
x0 <- c("v0" = .3, "theta" = .3, "sigma" = .4, "rho" = -.85, "kappa" = 3.) 
x0 <- c("v0" = .3, "theta" = .3, "sigma" = .5, "rho" = -.4, "kappa" = 1) 
# test

# Optimized result
detach("package:StockPriceSimulator", unload = T)
library(StockPriceSimulator)

x1 <- c("v0" = 0, "theta" = 0, "sigma" = .6, "rho" = -.8, "kappa" = 1) 
x2 <- c("v0" = .3, "theta" = .3, "sigma" = .5, "rho" = -.4, "kappa" = 1) 

v0 <- .3
theta <- .3
sigma <- seq(.5, .7, by = .1)
rho <- seq(-.4, -.9, by = -.1)
kappa <- seq(2, 4, by = 1)

xo <- as.data.frame(t(expand.grid(v0, theta, sigma, rho, kappa)))
# xo <- data.frame(x1, x2)

# tosave <- l

l2 <- map(xo , function(y){
  x0 <- c("v0" = y[1], "theta" = y[2], "sigma" = y[3], "rho" = y[4], "kappa" = y[5]) 
  
  x <- tryCatch(
    lsqnonlin(cost, x0, options = list(maxeval = 2000))$x,
    error = function(c) {
      message(c)
      return(c)
    }
  )
  if(inherits(x, "error")){
    x <- c(v0 = 0, theta = 0, sigma = 0, rho = 0, kappa = 0)
  }
  
  message(x)
  
  
  # Check optimized results accuracy   
  cst <- vector("double", length = nrow(DATA))
  for(i in 1:nrow(DATA)){
    cst[i] <- tryCatch(call_heston(DATA[i, 1], 
                                   x[1],
                                   x[2],
                                   # (x[5] + x[3]^2) / (2 * x[2]),
                                   x[5],
                                   x[3],
                                   DATA[i, 8],
                                   x[4],
                                   DATA[i, 2] / 365,
                                   DATA[i, 3]),
                       error = function(c) NA 
    )
    if(is.na(cst[i])){
      cst[i] <- 0
      next
    }
  }
  sum(cst - DATA$Last)
  
  
  # Gather both DATA and cst into a unique dataframe
  heston <- data.frame(DATA, 
                       heston = cst, 
                       diff = cst - DATA$Last, 
                       within.spread = (DATA$Ask - DATA$Bid) >= abs(cst - DATA$Last))
  
  
  list(check <- sum(heston$within.spread),
       feller <-  2 * x["kappa"] * x["theta"] - x["sigma"] ^2,
       x0,
       x)
  
})


# Measure the performance of the callibration
optim.1 <- l
optim.2 <- l2
within.spread.1 <- map_dbl(optim.1, ~ .x[[1]])

remove_negative_feller <- function(y){
  if(y[[2]] > 0.1){
    return(y[[1]])
  }else{
    return(0)
  }
}

within.spread.1 <- map_dbl(optim.1, .f = remove_negative_feller ) %>%
  unname 
within.spread.2 <- map_dbl(optim.2, .f = remove_negative_feller) %>%
  unname

# Keep the best fit
bstfit.optim.1 <-  map(which(within.spread.1 >= 20),
                       ~ optim.1[[.x]][[4]])
bstfit.optim.2 <-  map(which(within.spread.2 >= 20),
                       ~ optim.2[[.x]][[4]])

cost_bstfit <- function(x){
  cst <- vector("double", length = nrow(DATA)) 
  
  for(i in 1:nrow(DATA)){
    cst[i] <- DATA[i, 4] - call_heston(DATA[i, 1], 
                                       x[1],
                                       x[2],
                                       # (x[5] + x[3]^2) / (2 * x[2]),
                                       x[5],
                                       x[3],
                                       DATA[i, 8],
                                       x[4],
                                       DATA[i, 2] / 365,
                                       DATA[i, 3])
  }
  
  list(x,
       sum(cst ^ 2),
       sum(cst),
       cst)
}

summary.bstfit <- map(c(bstfit.optim.1, bstfit.optim.2), cost_bstfit)

cst1 <- map_dbl(summary.bstfit, ~ .x[[2]]) %>% 
  unname
min.cst1 <- which(cst1 == min(cst1))

cst2 <- map_dbl(summary.bstfit, ~ .x[[3]]) %>% 
  unname
min.cst2 <- which(abs(cst2) == min(abs(cst2)))


# Winner:
bstfit <- summary.bstfit[[min.cst1]]
bstfit.idx <- map_lgl(c(optim.1, optim.2), .f = function(x){
  sum(x[[4]] == bstfit[[1]]) == 5
}) %>% unname %>% which

c(optim.1, optim.2)[[bstfit.idx]]
  


# #### End measure Perf ###


x <- bstfit[[1]]

# Feller constraint
2 * x["kappa"] * x["theta"] - x["sigma"] ^2

h <- heston(initial_stock_price = AAPL$Last,
       initial_volatility = x["v0"],
       time_to_maturity = 1,
       seed = 1,
       scale = 365,
       alpha = 0.02,
       rho = x["rho"],
       kappa = x["kappa"],
       theta = x["theta"],
       sigma = x["sigma"])
























