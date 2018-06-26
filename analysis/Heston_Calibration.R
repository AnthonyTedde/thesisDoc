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

################################################################################
# delta_hedging 
################################################################################

tic()

initial_stock_price = AAPL$Last
initial_volatility = x["v0"]
time_to_maturity = 399/365
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
  
    
  #  Computation of delta for each time series GBM
  d <- purrr::map(GBM, .f = function(x){
    tryCatch(purrr::map_dbl(as.data.frame(t(x)), .f = function(y){
      upper.st.price <- y[[2]] * 1.0000000001
      lower.st.price <- y[[2]] * 0.9999999999
      
      upper.c.price <- call_heston(initial_stock_price = upper.st.price,
                                   initial_volatility = y[5],
                                   theta = theta,
                                   kappa = kappa,
                                   sigma = sigma,
                                   alpha = alpha,
                                   rho = rho,
                                   time_to_maturity = time_to_maturity - y[1],
                                   K = K)
      
      lower.c.price <- call_heston(initial_stock_price = lower.st.price,
                  initial_volatility = y[5],
                  theta = theta,
                  kappa = kappa,
                  sigma = sigma,
                  alpha = alpha,
                  rho = rho,
                  time_to_maturity = time_to_maturity - y[1],
                  K = K)
      
      (upper.c.price - lower.c.price) / (upper.st.price - lower.st.price)
    }), error = function(c) NA)
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
  
  for(i in 1:length(d)){
    d[[i]][(time_to_maturity * scale) + 1] <-  round(d[[i]][time_to_maturity * scale])
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
    amount_paid <- dd * g$stock_price_path # - c(o[1], diff(o))
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
  hedging_perf <- hedging_cost / o
  
 toc() 
  
  
  
  if(full)
    structure(list("delta" = i,
                   "perf" = hedging_perf,
                   "summary" = u))
  else hedging_perf
}



################################################################################
# delta_Gamma_hedging 
################################################################################

tic()

initial_stock_price = AAPL$Last
initial_volatility = x["v0"]
time_to_maturity = 399/365
seed = 1
scale = 365
alpha = 0.02369057
r <- alpha
K = 180
K2 <- 150
strike <- K
rho = x["rho"]
kappa = x["kappa"]
theta = x["theta"]
sigma = x["sigma"]

## 2. Simulate 500 GRM that correspond to that date
GBM <- purrr::map(1:2, ~heston(initial_stock_price = initial_stock_price,
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
  tryCatch(purrr::map_dbl(as.data.frame(t(x)), .f = function(y){
    upper.st.price <- y[[2]] + 0.00000000000005
    lower.st.price <- y[[2]] - 0.00000000000005
    
    upper.c.price <- call_heston(initial_stock_price = upper.st.price,
                                 initial_volatility = y[5],
                                 theta = theta,
                                 kappa = kappa,
                                 sigma = sigma,
                                 alpha = alpha,
                                 rho = rho,
                                 time_to_maturity = time_to_maturity - y[1],
                                 K = K)
    
    lower.c.price <- call_heston(initial_stock_price = lower.st.price,
                                 initial_volatility = y[5],
                                 theta = theta,
                                 kappa = kappa,
                                 sigma = sigma,
                                 alpha = alpha,
                                 rho = rho,
                                 time_to_maturity = time_to_maturity - y[1],
                                 K = K)
    
    (upper.c.price - lower.c.price) / (upper.st.price - lower.st.price)
  }), error = function(c) NA)
})

#  Computation of delta for each time series GBM
dO2 <- purrr::map(GBM, .f = function(x){
  tryCatch(purrr::map_dbl(as.data.frame(t(x)), .f = function(y){
    upper.st.price <- y[[2]] + 0.00000000000005
    lower.st.price <- y[[2]] - 0.00000000000005
    
    upper.c.price <- call_heston(initial_stock_price = upper.st.price,
                                 initial_volatility = y[5],
                                 theta = theta,
                                 kappa = kappa,
                                 sigma = sigma,
                                 alpha = alpha,
                                 rho = rho,
                                 time_to_maturity = time_to_maturity - y[1],
                                 K = K2)
    
    lower.c.price <- call_heston(initial_stock_price = lower.st.price,
                                 initial_volatility = y[5],
                                 theta = theta,
                                 kappa = kappa,
                                 sigma = sigma,
                                 alpha = alpha,
                                 rho = rho,
                                 time_to_maturity = time_to_maturity - y[1],
                                 K = K2)
    
    (upper.c.price - lower.c.price) / (upper.st.price - lower.st.price)
  }), error = function(c) NA)
})

#  Computation of delta for each time series GBM
d2O <- purrr::pmap(list(GBM, O), .f = function(x, z){
  tryCatch(purrr::pmap_dbl(list(as.data.frame(t(x)), z), .f = function(y, k){
    upper.st.price <- y[[2]] * 1.1
    lower.st.price <- y[[2]] * 0.9
    
    remaining_time <- time_to_maturity - y[1]
    if(remaining_time == 0) remaining_time <- 1 / (365 * 24) # One hour
    
    upper.c.price <- call_heston(initial_stock_price = upper.st.price,
                                 initial_volatility = y[5],
                                 theta = theta,
                                 kappa = kappa,
                                 sigma = sigma,
                                 alpha = alpha,
                                 rho = rho,
                                 time_to_maturity = remaining_time,
                                 K = K)
    
    lower.c.price <- call_heston(initial_stock_price = lower.st.price,
                                 initial_volatility = y[5],
                                 theta = theta,
                                 kappa = kappa,
                                 sigma = sigma,
                                 alpha = alpha,
                                 rho = rho,
                                 time_to_maturity = remaining_time,
                                 K = K)
    
    (upper.c.price + lower.c.price -2 * k ) / ((upper.st.price - lower.st.price) / 2)^2
  }), error = function(c) NA)
})

#  Computation of delta for each time series GBM
d2O2 <- purrr::pmap(list(GBM, O2), .f = function(x, z){
  tryCatch(purrr::pmap_dbl(list(as.data.frame(t(x)), z), .f = function(y, k){
    upper.st.price <- y[[2]] * 1.1
    lower.st.price <- y[[2]] * 0.9
    
    upper.c.price <- call_heston(initial_stock_price = upper.st.price,
                                 initial_volatility = y[5],
                                 theta = theta,
                                 kappa = kappa,
                                 sigma = sigma,
                                 alpha = alpha,
                                 rho = rho,
                                 time_to_maturity = time_to_maturity - y[1],
                                 K = K2)
    
    lower.c.price <- call_heston(initial_stock_price = lower.st.price,
                                 initial_volatility = y[5],
                                 theta = theta,
                                 kappa = kappa,
                                 sigma = sigma,
                                 alpha = alpha,
                                 rho = rho,
                                 time_to_maturity = time_to_maturity - y[1],
                                 K = K2)
    
    (upper.c.price + lower.c.price -2 * k ) / ((upper.st.price - lower.st.price) / 2)^2
  }), error = function(c) NA)
})

# Remove the NAs due to bad integrate computation
O.toremove <- which(is.na(map(O, ~ .x[1])))
O2.toremove <- which(is.na(map(O2, ~ .x[1])))
d.toremove <- which(is.na(map(d, ~ .x[1])))
d2O.toremove <- which(is.na(map(d2O, ~ .x[1])))
d2O2.toremove <- which(is.na(map(d2O2, ~ .x[1])))
toremove <- unique(c(O.toremove, d.toremove, O2.toremove, d2O.toremove, d2O2.toremove))
if(!is_empty(toremove)){
  O <- O[-toremove]
  O2 <- O2[-toremove]
  d <- d[-toremove]
  d2O <- d2O[-toremove]
  d2O2 <- d2O2[-toremove]
  GBM <- GBM[-toremove]
}

# for(i in 1:length(d)){
#   d[[i]][(time_to_maturity * scale) + 1] <-  round(d[[i]][time_to_maturity * scale])
#   d2O[[i]][(time_to_maturity * scale) + 1] <-  0
#   d2O2[[i]][(time_to_maturity * scale) + 1] <-  0 
# }

delta_c2 <- purrr::pmap(list(d2O,d2O2), .f = function(x, y){
  delta <- x / y
  idx <- which(is.infinite(delta))
  delta[idx] <- 0
  delta
})

# # Maybe to remove
# for(i in 1:length(delta_c2)){
#   delta_c2[[i]][(time_to_maturity * scale) + 1] <-  0 
# }

delta_s <- purrr::pmap(list(d, delta_c2, dO2), .f = function(x, y, z){
  x - y * z
})


 

# Get the delta of the delta for each time series.
diff_delta_s <- purrr::map(delta_s,
                         ~ c(.x[1], diff(.x)))

diff_delta_c2 <- purrr::map(delta_c2,
                         ~ c(.x[1], diff(.x)))
# list of arrays used in functional pmap structure.
l <- list(delta_s,
          GBM,
          O,
          O2,
          delta_c2,
          diff_delta_s,
          diff_delta_c2)

# Output a dataframe containing:
#   * The remaining time till maturity
#   * The delta of the delta. By convention the first item is the value of
#     delta at time 0
#   * The value of delta for each time
#   * The value of the stock for each time following a Geom. Brownian Motion
#   * The Total amount paid to get delta stock at time t
#   * The Total amout paid to get delta stock at time t, but value for T
u <- purrr::pmap(l, .f = function(ds, s, c1, c2, dc, dds, ddc){
  
  # time_remaining <- (time_to_maturity * scale + 1) : 1
  time_remaining <- seq(time_to_maturity, 0, length.out = (time_to_maturity * scale) + 1)
  amount_paid <- ddc * c2 + dds * s$stock_price_path #- c(c1[1], diff(c1))
  # amount_with_interest <- amount_paid * (1 + r / (scale)) ^ (time_remaining - 1)
  amount_with_interest <- amount_paid * exp(r * time_remaining)
  
  data.frame("time remaining" = time_remaining,
             "option price" = c1,
             "option 2 price" = c2,
             "delta" = ds,
             "gamma" = dc,
             "s" = s$stock_price_path,
             "amount paid" = amount_paid,
             "with interest" = amount_with_interest)
  
})


# i <- purrr::map_dbl(u,
#                     ~ sum(.x$with.interest)
#                     - .x$delta[time_to_maturity * scale + 1] * strike
#                     - o * (1 + r / (scale)) ^ (time_to_maturity * scale))

i <- purrr::map_dbl(u,
                    ~ sum(.x$with.interest[-(time_to_maturity * scale + 1)])
                    - .x$delta[time_to_maturity * scale] * strike
                    - .x$gamma[time_to_maturity * scale] * .x$option.2.price[time_to_maturity * scale]
                    - o * exp(r * time_to_maturity))

hedging_cost <- purrr::map_dbl(u,
                               ~ sum(.x$with.interest[-(time_to_maturity * scale + 1)]) 
                               - .x$delta[time_to_maturity * scale] * .x$option.price[time_to_maturity * scale]
                               - .x$gamma[time_to_maturity * scale] * .x$option.2.price[time_to_maturity * scale])


hedging_cost <- purrr::map_dbl(u,
                               ~ sum(.x$with.interest))

hedging_cost_sd <- sd(hedging_cost)
hedging_perf <- hedging_cost / o

toc()

if(full)
  structure(list("delta" = i,
                 "perf" = hedging_perf,
                 "summary" = u))
else hedging_perf
}








##################3
### delta heston
##################

S <- GBM[[1]]$stock_price_path
initial_volatility
theta
kappa
sigma
alpha
rho
time_to_maturity
K <- 200
  remaining.time <- seq(time_to_maturity, 0, length.out = length(S))
  remaining.time <- c(remaining.time[-length(remaining.time)], (1 / (365 * 24)))
  
  hc <- purrr::pmap(list(S, remaining.time), ~ heston_characteristic(..1,
                                                initial_volatility,
                                                theta,
                                                kappa,
                                                sigma,
                                                alpha,
                                                rho,
                                                ..2))
  
  hc2 <- purrr::pmap(list(S, remaining.time), ~ heston_characteristic(log(..1),
                                              initial_volatility,
                                              theta,
                                              kappa,
                                              sigma,
                                              alpha,
                                              rho,
                                              ..2))
  
  pmap_dbl(list(hc, hc2, S), .f = function(hc, hc2, s){
    p1 <- function(w){
      a <- exp(-1i * w * log(K)) * hc(w -1i)
      Re(a / (1i * w * hc(-1i)))
    }
    
    
    f1 <- function(w){
      a <- exp(-1i * w * log(K)) * hc(w -1i)
      Re(a / (hc(-1i) * s))
    }
    
    f2 <- function(w){
      a <- exp(-1i * w * log(K)) * hc(w)
      Re(a / s)
    }
    
    
    
    pi1 <- 1/2 + 1/pi * integrate(p1, 0, Inf, subdivisions=2000, stop.on.error = F)$value
    fi1 <- integrate(f1, 0, Inf, subdivisions=2000, stop.on.error = F)$value
    fi2 <- integrate(f2, 0, Inf, subdivisions=2000, stop.on.error = F)$value
    
    
    pi1 + (s / pi) * fi1 - (K / pi) * fi2
  })
























