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
setwd("/Users/anthony/workspace/thesis/thesis/data")
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

  sum(cst)^2
  
}

x0 <- c("v0" = .2, "theta" = .2, "sigma" = .4, "rho" = -.5, "kappa" = 1.2)
lower <- c(0,0,0,-1,0)
upper <- c(1,1,5,1,20)

lsqnonlin(cost, x0, options = list(maxeval = 2000))
modFit(cost, x0, lower = lower, upper = upper)


x <- c(0.02924656,  0.15179058,  0.20226292, -0.80011677,  0.99861074) 
x <- c(0.02187661,  0.15032430,  0.20198890, -0.50006493,  0.99838052)
v0       theta       sigma         rho       kappa 
0.02041248  0.26148421  0.20458438 -0.50043906  0.49515858
v0       theta       sigma         rho       kappa 
x <- c(0.03804248,  0.28543453,  0.20543522, -0.80042459,  0.19479137) 
x <- c(0.03843349,  0.17386345,  0.50712030, -0.80170592,  0.49718398)
x <- c(0.02794343,  0.10050789,  0.50505045, -0.80097830,  1.99891370)
x0 <- c("v0" = .2, "theta" = .2, "sigma" = .5, "rho" = -.5, "kappa" = 1):
x <- c(0.02508427,  0.14854600,  0.50480220, -0.50060892,  0.99788751) 

cst <- vector("double", length = nrow(DATA))
for(i in 1:nrow(DATA)){
cst[i] <- call_heston(DATA[i, 1], 
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

sum(cst - DATA$Last)

heston(186.31,
       x[1],
       399/365,
       1,
       365,
       0.40,
       x[4],
       x[5],
       x[2],
       x[3])

2 * x[5] * x[2] - x[3]^2


################################################################################
# # Use modFit
# ## initial "guess"
# parms <-  c(v0 = .02, theta = .2, sigma = .5, rho = -.5, kappa = 1)
# i <- nrow(DATA)
# # 
# # ## analytical solution
# model <- function(parms, i){
#   with(as.list(parms),
#   return(
#     call_heston(DATA[i, 1],
#                 v0,
#                 theta,
#                 (kappa + sigma ^2) / (2 * theta),
#                 #kappa,
#                 sigma,
#                 DATA[i, 8],
#                 rho,
#                 DATA[i, 2] / 365,
#                 DATA[i, 3])
#   ))
# }
# 
# # 
# # ## FITTING algorithm 1
# ModelCost <- function(P) {
#   out <- map_dbl(1:i, ~model(P, .x))
#   return(DATA$Last-out)  # residuals
# }
# 
# (Fita <- modFit(f = ModelCost, p = parms))
# ################################################################################





















