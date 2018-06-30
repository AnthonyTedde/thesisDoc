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
