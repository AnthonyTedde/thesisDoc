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
DATA <- data.frame(DATA, price = (DATA$Ask + DATA$Bid) / 2)
DATA <- DATA[DATA$maturity >= 63 & DATA$maturity <= 399 & 
               DATA$Strike %in% c(130, 140, 150, 160, 170, 180, 190, 200, 210, 220), ]
rownames(DATA) <- 1:nrow(DATA)

cost <- function(x){
  cst <- vector("double", length = nrow(DATA)) 
  
  for(i in 1:nrow(DATA)){
    cst[i] <- DATA[i, 9] - call_heston(DATA[i, 1], 
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



# Optimized result

v0 <- .3
theta <- .3
sigma <- seq(.3, .7, by = .1)
rho <- seq(-.4, -.9, by = -.1)
kappa <- seq(2, 5, by = 1)

xo <- as.data.frame(t(expand.grid(v0, theta, sigma, rho, kappa)))
# xo <- data.frame(x1, x2)

# tosave <- l

l <- map(xo , function(y){
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
  sum(cst - DATA$price)
  
  
  # Gather both DATA and cst into a unique dataframe
  heston <- data.frame(DATA, 
                       heston = cst, 
                       diff = cst - DATA$price, 
                       within.spread = (DATA$Ask - DATA$Bid) >= abs(cst - DATA$price))
  
  
  list(check <- sum(heston$within.spread),
       feller <-  2 * x["kappa"] * x["theta"] - x["sigma"] ^2,
       x0,
       x)
  
})


# Measure the performance of the callibration
optim.1 <- l

remove_negative_feller <- function(y){
  if(y[[2]] > 0.1){
    return(y[[1]])
  }else{
    return(0)
  }
}

within.spread.1 <- map_dbl(optim.1, .f = remove_negative_feller ) %>%
  unname 

remove_negative_feller_fromlist <- function(y){
  if(y[[2]] > 0.1 & y[[4]][1] >0 & y[[1]] >= 42){
    return(y)
  }else{
    NA
  }
}
optim.2.logical <- map(optim.1, .f = remove_negative_feller_fromlist ) %>%
  is.na %>% `!`
optim.2 <- optim.1[optim.2.logical]

optim.toprint <- map(optim.2, `[[`, 4) %>% as.data.frame %>% t

xt <- xtable(optim.toprint,
             caption = "Best estimates for HSV call option model",
             digits=c(0,5,5,5,5,5))
print(xt,  include.rownames=FALSE)



# Keep the best fit
bstfit.optim.1 <-  map(which(within.spread.1 >= 50),
                       ~ optim.1[[.x]][[4]])
bstfit.optim.2 <-  map(which(within.spread.2 >= 50),
                       ~ optim.2[[.x]][[4]])
#####################
# BEST DEAl
#####################
bstfit <- optim.2[which(within.spread.2 == max(within.spread.2))]
x <- bstfit[[1]][[4]]
bstfit <- optim.1[which(within.spread.1 == max(within.spread.1))]
x <- bstfit[[1]][[4]]

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
bstfit <- summary.bstfit[[min.cst2]]
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


#############################################################################
##  Delta hedgin heston
############################################################################
detach("package:StockPriceSimulator", unload = T)
library(StockPriceSimulator)
tic()

initial_stock_price = AAPL$Last
initial_volatility = x["v0"]
time_to_maturity = 67/365
seed = 1
scale = 365
alpha = 0.02369057
r <- alpha
K = 180
strike <- K
rho = x["rho"]
kappa = x["kappa"]
theta = x["theta"]
sigma = x["sigma"]

## 2. Simulate 500 GRM that correspond to that date
GBM <- purrr::map(1:50, ~heston(initial_stock_price = initial_stock_price,
                                initial_volatility = initial_volatility,
                                time_to_maturity = time_to_maturity,
                                seed = .x,
                                scale = scale,
                                alpha = alpha,
                                rho = rho,
                                kappa = kappa,
                                theta = theta,
                                sigma = sigma))

# remove NAs (error due to negative CIR)
CIR.last <- map_dbl(GBM, ~ tail(.x$CIR, 1)) %>% unname
toremove <- which(is.na(CIR.last))
if(! is_empty(toremove))
  GBM <- GBM[-toremove]

# Option price at Time t = 0 
o <-  call_heston(initial_stock_price = initial_stock_price, 
                  initial_volatility = initial_volatility,
                  theta = theta,
                  kappa = kappa,
                  sigma = sigma,
                  alpha = alpha,
                  rho = rho,
                  time_to_maturity = time_to_maturity,
                  K = K)

# Option price for all Geometric BM at all times
O <- purrr::map(GBM, .f = function(x){
  tryCatch(purrr::map_dbl(as.data.frame(t(x)), .f = function(y){
    call_heston(initial_stock_price = y[2],
                initial_volatility = y[5],
                theta = theta,
                kappa = kappa,
                sigma = sigma,
                alpha = alpha,
                rho = rho,
                time_to_maturity = time_to_maturity - y[1],
                K = K)
  }), error = function(c) NA)
})


#  Computation of delta for each time series GBM
d <- purrr::map(GBM, .f = function(x){
  delta_heston(x$stock_price_path,
               x$CIR,
               theta,
               kappa,
               sigma,
               alpha,
               rho,
               time_to_maturity,
               K)
})


# Remove the NAs due to bad integrate computation
O.toremove <- which(is.na(map(O, ~ .x[1])))
d.toremove <- which(is.na(map(d, ~ .x[1])))
toremove <- unique(c(O.toremove, d.toremove))
if(!is_empty(toremove)){
  O <- O[-toremove]
  d <- d[-toremove]
  GBM <- GBM[-toremove]
}

# Get the delta of the delta for each time series.
diff_delta <- purrr::map(d,
                         ~ c(.x[1], diff(.x)))

# list of arrays used in functional pmap structure.
l <- list(diff_delta,
          d,
          GBM,
          O)

# Output a dataframe containing:
#   * The remaining time till maturity
#   * The delta of the delta. By convention the first item is the value of
#     delta at time 0
#   * The value of delta for each time
#   * The value of the stock for each time following a Geom. Brownian Motion
#   * The Total amount paid to get delta stock at time t
#   * The Total amout paid to get delta stock at time t, but value for T
u <- purrr::pmap(l, .f = function(dd, d, g, o){
  
  # time_remaining <- (time_to_maturity * scale + 1) : 1
  time_remaining <- seq(time_to_maturity, 0, length.out = (time_to_maturity * scale) + 1)
  amount_paid <- dd * g$stock_price_path #- c(o[1], diff(o))
  # amount_with_interest <- amount_paid * (1 + r / (scale)) ^ (time_remaining - 1)
  amount_with_interest <- amount_paid * exp(r * time_remaining)
  
  data.frame("time remaining" = time_remaining,
             "option price" = o,
             "diff delta" = dd,
             "delta" = d,
             "s" = g$stock_price_path,
             "amount paid" = amount_paid,
             "with interest" = amount_with_interest)
  
})


# i <- purrr::map_dbl(u,
#                     ~ sum(.x$with.interest)
#                     - .x$delta[time_to_maturity * scale + 1] * strike
#                     - o * (1 + r / (scale)) ^ (time_to_maturity * scale))

i <- purrr::map_dbl(u,
                    ~ sum(.x$with.interest)
                    - .x$delta[time_to_maturity * scale + 1] * strike
                    - o * exp(r * time_to_maturity))

hedging_cost <- purrr::map_dbl(u,
                               ~ sum(.x$with.interest) - .x$delta[time_to_maturity * scale + 1] * strike)
hedging_cost_sd <- sd(hedging_cost)
hedging_perf <- hedging_cost / (o * exp(r * time_to_maturity))

sum(i) / length(i)

toc() 
> i
[1]  3.7836358  1.1450726 -1.3733684  1.5018851 -0.6252957  3.1870493
[7]  0.3670511  2.3174390 -0.9525140 -1.3005366
> hedging_cost
[1] 15.53286 12.89430 10.37586 13.25111 11.12393 14.93627 12.11628
[8] 14.06666 10.79671 10.44869
> (o * exp(r * time_to_maturity))
[1] 11.74922


if(full)
  structure(list("delta" = i,
                 "perf" = hedging_perf,
                 "summary" = u))
else hedging_perf
}



#############################################################################
##  Delta - gamma hedgin heston
############################################################################

#############################################################################
##  Delta hedgin heston
############################################################################
detach("package:StockPriceSimulator", unload = T)
library(StockPriceSimulator)
tic()

initial_stock_price = AAPL$Last
initial_volatility = x["v0"]
time_to_maturity = 399/365
seed = 1
scale = 365
alpha = 0.02369057
r <- alpha
K = 130
K2 <- 140
strike <- K
rho = x["rho"]
kappa = x["kappa"]
theta = x["theta"]
sigma = x["sigma"]


## 2. Simulate 500 GRM that correspond to that date
GBM <- purrr::map(1:10, ~heston(initial_stock_price = initial_stock_price,
                                initial_volatility = initial_volatility,
                                time_to_maturity = time_to_maturity,
                                seed = .x,
                                scale = scale,
                                alpha = alpha,
                                rho = rho,
                                kappa = kappa,
                                theta = theta,
                                sigma = sigma))

# remove NAs (error due to negative CIR)
CIR.last <- map_dbl(GBM, ~ tail(.x$CIR, 1)) %>% unname
toremove <- which(is.na(CIR.last))
if(! is_empty(toremove))
  GBM <- GBM[-toremove]

# Option price at Time t = 0 
o <-  call_heston(initial_stock_price = initial_stock_price, 
                  initial_volatility = initial_volatility,
                  theta = theta,
                  kappa = kappa,
                  sigma = sigma,
                  alpha = alpha,
                  rho = rho,
                  time_to_maturity = time_to_maturity,
                  K = K)

# Option price for all Geometric BM at all times
O <- purrr::map(GBM, .f = function(x){
  tryCatch(purrr::map_dbl(as.data.frame(t(x)), .f = function(y){
    call_heston(initial_stock_price = y[2],
                initial_volatility = y[5],
                theta = theta,
                kappa = kappa,
                sigma = sigma,
                alpha = alpha,
                rho = rho,
                time_to_maturity = time_to_maturity - y[1],
                K = K)
  }), error = function(c) NA)
})
O2 <- purrr::map(GBM, .f = function(x){
  tryCatch(purrr::map_dbl(as.data.frame(t(x)), .f = function(y){
    call_heston(initial_stock_price = y[2],
                initial_volatility = y[5],
                theta = theta,
                kappa = kappa,
                sigma = sigma,
                alpha = alpha,
                rho = rho,
                time_to_maturity = time_to_maturity - y[1],
                K = K2)
  }), error = function(c) NA)
})



#  Computation of delta for each time series GBM
d <- purrr::map(GBM, .f = function(x){
  delta_heston(x$stock_price_path,
               x$CIR,
               theta,
               kappa,
               sigma,
               alpha,
               rho,
               time_to_maturity,
               K)
})
d2 <- purrr::map(GBM, .f = function(x){
  delta_heston(x$stock_price_path,
               x$CIR,
               theta,
               kappa,
               sigma,
               alpha,
               rho,
               time_to_maturity,
               K = K2)
})


g1 <- purrr::map(GBM, .f = function(x){
  gamma_heston(x$stock_price_path,
               x$CIR,
               theta,
               kappa,
               sigma,
               alpha,
               rho,
               time_to_maturity,
               K = K)
})

g2 <- purrr::map(GBM, .f = function(x){
  gamma_heston(x$stock_price_path,
               x$CIR,
               theta,
               kappa,
               sigma,
               alpha,
               rho,
               time_to_maturity,
               K = K2)
})
# Remove the NAs due to bad integrate computation
O.toremove <- which(is.na(map(O, ~ .x[1])))
O2.toremove <- which(is.na(map(O2, ~ .x[1])))
d.toremove <- which(is.na(map(d, ~ .x[1])))
d2.toremove <- which(is.na(map(d2, ~ .x[1])))
g1.toremove <- which(is.na(map(g1, ~ .x[1])))
g2.toremove <- which(is.na(map(g2, ~ .x[1])))
toremove <- unique(c(O.toremove, d.toremove, O2.toremove, d2.toremove))
if(!is_empty(toremove)){
  O <- O[-toremove]
  O2 <- O2[-toremove]
  d <- d[-toremove]
  d2 <- d2[-toremove]
  g1 <- g1[-toremove]
  g2 <- g2[-toremove]
  GBM <- GBM[-toremove]
}

alpha1 <- purrr::pmap(list(g1, g2), ~  ..1 / ..2)
alpha2 <- purrr::pmap(list(alpha1, d, d2), ~ ..2 - ..1 * ..3)

# Get the delta of the delta for each time series.
diff_alpha1 <- purrr::map(alpha1,
                         ~ c(.x[1], diff(.x)))

diff_alpha2 <- purrr::map(alpha2,
                         ~ c(.x[1], diff(.x)))
# list of arrays used in functional pmap structure.
l <- list(diff_alpha1,
          diff_alpha2,
          d,
          d2,
          GBM,
          O,
          O2)

# Output a dataframe containing:
#   * The remaining time till maturity
#   * The delta of the delta. By convention the first item is the value of
#     delta at time 0
#   * The value of delta for each time
#   * The value of the stock for each time following a Geom. Brownian Motion
#   * The Total amount paid to get delta stock at time t
#   * The Total amout paid to get delta stock at time t, but value for T
u <- purrr::pmap(l, .f = function(da1, da2, d1, d2, S, c1, c2){
  
  # time_remaining <- (time_to_maturity * scale + 1) : 1
  time_remaining <- seq(time_to_maturity, 0, length.out = (time_to_maturity * scale) + 1)
  amount_paid <- da1 * c2 + da2 * S$stock_price_path 
  # amount_with_interest <- amount_paid * (1 + r / (scale)) ^ (time_remaining - 1)
  amount_with_interest <- amount_paid * exp(r * time_remaining)
  
  data.frame("time remaining" = time_remaining,
             "option price" = o,
             "diff alpha 1" = da1,
             "diff alpha 2" = da2,
             "delta" = d1,
             "delta 2" = d2,
             "s" = S$stock_price_path,
             "amount paid" = amount_paid,
             "with interest" = amount_with_interest)
  
})


# i <- purrr::map_dbl(u,
#                     ~ sum(.x$with.interest)
#                     - .x$delta[time_to_maturity * scale + 1] * strike
#                     - o * (1 + r / (scale)) ^ (time_to_maturity * scale))

i <- purrr::map_dbl(u,
                    ~ sum(.x$with.interest)
                    - .x$delta[time_to_maturity * scale + 1] * strike
                    - o * exp(r * time_to_maturity))

hedging_cost <- purrr::map_dbl(u,
                               ~ sum(.x$with.interest) - .x$delta[time_to_maturity * scale + 1] * strike)
hedging_cost_sd <- sd(hedging_cost)
hedging_perf <- hedging_cost / (o * exp(r * time_to_maturity))

toc() 


# TEST
- sum(u[[1]]$with.interest) +
  sum(u[[1]]$diff.alpha.1) * (tail(u[[1]]$s, 1) - K2) * tail(u[[1]]$delta.2,1) +
  tail(u[[1]]$s, 1) * (sum(u[[1]]$diff.alpha.2) - tail(u[[1]]$delta, 1)) +
  tail(u[[1]]$delta, 1) * K +
  o * exp(r * time_to_maturity)

- sum(u[[2]]$with.interest) +
  sum(u[[2]]$diff.alpha.1) * (tail(u[[2]]$s, 1) - K2) * tail(u[[2]]$delta.2,1) + 
  tail(u[[2]]$s, 1) * (sum(u[[2]]$diff.alpha.2) - tail(u[[2]]$delta, 1)) +
  tail(u[[2]]$delta, 1) * K +
  o * exp(r * time_to_maturity)


if(full)
  structure(list("delta" = i,
                 "perf" = hedging_perf,
                 "summary" = u))
else hedging_perf
}


setwd("c:/Users/ATE/thesisDoc/data")
   
save(x, file = "optimalHestonCalibration.RData")
save(DATA, file = "DATA.RData")















