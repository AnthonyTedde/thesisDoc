################################################################################
# Merton Hedgin performance
################################################################################

#################################
# LIBRARY
#################################
detach("package:StockPriceSimulator", unload = T)
library(StockPriceSimulator)

#################################
# VARIABLE
#################################
load(file = "optimalMertonCalibration.RData")
load(file = "optimalMertonRiskaverse.RData")
load(file = "DATA.RData")
n_delta_hedge <- 100
initial_stock_price = 186.31
period <- 67
time_to_maturity = 67/365
seed = 1
scale = 365
K <- strike <-  180
strike <- K
r <- 0.02144491

# Generalization



#################################
# TIME SERIES
#################################
tic()
args_merton <- as.list(merton_riskaverse)
args_merton$jumps_intensity_parameters <- list(mean = args_merton$mu,
                                               sd = args_merton$delta)
args_merton$delta <- args_merton$mu <- NA
args_merton <- args_merton[!is.na(args_merton)]


merton_ts <- map(1:n_delta_hedge, ~ sstock_jump(
  initial_stock_price = initial_stock_price,
  time_to_maturity = 399/365,
  seed = .x,
  scale = 365,
  sigma = args_merton$sigma,
  alpha = args_merton$alpha,
  lambda = args_merton$lambda,
  jumps_intensity_parameters = list(
    mean = args_merton$jumps_intensity_parameters$mean,
    sd = args_merton$jumps_intensity_parameters$sd)))

#################################
# OPTION PRICE
#################################
o_merton_t0 <-  call_merton(initial_stock_price = initial_stock_price,
                            time_to_maturity = time_to_maturity,
                            seed = 1,
                            scale = 365, # Daily measurement
                            sigma = x_merton['sigma'],
                            alpha = r,
                            lambda =  x_merton['lambda'],
                            jumps_intensity_parameters = list(mean = x_merton['mu'],
                                                              sd = x_merton['delta']),
                            K = K)

# # Option price for all Geometric BM at all times
O_merton <- purrr::map(merton_ts, .f = function(x){
  df <- data.frame(x$stock_price_path[1:(period+1)], x$time[1:(period+1)])
  purrr::map_dbl(as.data.frame(t(df)), .f = function(y){
    if(time_to_maturity - y[2] == 0)
      tau <- 1/365/24
    else
      tau <- time_to_maturity - y[2]
    call_merton(initial_stock_price = y[1],
                time_to_maturity = tau,
                seed = 1,
                scale = 365, # Daily measurement
                sigma = x_merton['sigma'],
                alpha = r,
                lambda =  x_merton['lambda'],
                jumps_intensity_parameters = list(mean = x_merton['mu'],
                                                  sd = x_merton['delta']),
                K = K)
  })
})

#################################
# Delta computation
#################################
d <- purrr::map(merton_ts, .f = function(x){
  delta_merton(x$stock_price_path[1:(period+1)],
               time_to_maturity = time_to_maturity,
               K = K,
               sigma = x_merton['sigma'],
               alpha = r,
               lambda = x_merton['lambda'],
               jumps_intensity_parameters  = list(mean = x_merton['mu'],
                                                  sd = x_merton['delta']))
})


# Get the delta of the delta for each time series.
diff_delta <- purrr::map(d,
                         ~ c(.x[1], diff(.x)))

# list of arrays used in functional pmap structure.
l <- list(diff_delta,
          d,
          merton_ts,
          O_merton)

# Output a dataframe containing:
#   * The remaining time till maturity
#   * The delta of the delta. By convention the first item is the value of
#     delta at time 0
#   * The value of delta for each time
#   * The value of the stock for each time following a Geom. Brownian Motion
#   * The Total amount paid to get delta stock at time t
#   * The Total amout paid to get delta stock at time t, but value for T
u <- purrr::pmap(l, .f = function(dd, d, g, O){
  
  # time_remaining <- (time_to_maturity * scale + 1) : 1
  time_remaining <- seq(time_to_maturity, 0, length.out = (time_to_maturity * scale) + 1)
  amount_paid <- dd * g$stock_price_path[1:(period +1)] #- c(o[1], diff(o))
  # amount_with_interest <- amount_paid * (1 + r / (scale)) ^ (time_remaining - 1)
  amount_with_interest <- amount_paid * exp(r * time_remaining)
  
  p <- o_merton_t0 - cumsum(dd  * g$stock_price_path[1:(period + 1)])
  #####################
  # test
  # o_merton_t0 - cumsum(diff_delta[[1]] * merton_ts[[1]]$stock_price_path[1:(period + 1)])
  # u[[1]]$delta * merton_ts[[1]]$stock_price_path[1:(period + 1)] # Ce que j' ai en plus!
  ########################
  
  data.frame("time remaining" = time_remaining,
             "time period" = time_remaining[1] - time_remaining,
             "diff delta" = dd,
             "delta" = d,
             "option" = O,
             "s" = g$stock_price_path[1:(period +1)],
             "amount paid" = amount_paid,
             "with interest" = amount_with_interest,
             "p" = p)
})
toc()  







# i <- purrr::map_dbl(u,
#                     ~ sum(.x$with.interest)
#                     - .x$delta[time_to_maturity * scale + 1] * strike
#                     - o * (1 + r / (scale)) ^ (time_to_maturity * scale))

i <- purrr::map_dbl(u,
                    ~ sum(.x$with.interest)
                    - .x$delta[time_to_maturity * scale + 1] * strike
                    - o_merton_t0 * exp(r * time_to_maturity))

pi <- purrr::map_dbl(u,
                    ~ .x$delta[time_to_maturity * scale + 1] 
                      * .x$s[time_to_maturity * scale + 1]
                    + (exp(r*time_to_maturity) * .x$p[time_to_maturity * scale + 1])
                    - .x$option[time_to_maturity * scale + 1])



pl <- exp(-r * time_to_maturity) * pi / o_merton_t0
                    
      

hedging_cost <- purrr::map_dbl(u,
                               ~ sum(.x$with.interest) - .x$delta[time_to_maturity * scale + 1] * strike)
hedging_cost_sd <- sd(hedging_cost)
hedging_perf <- hedging_cost / (o_merton_t0 * exp(r * time_to_maturity))

sum(i) / length(i)

           
                 
#################################
# Plot
#################################
ggplot(u[[4]]) +
  geom_line(aes(x = time.period, y = option),
            colour = 'blue')+
  geom_point(aes(x = time.period, y = delta* s + exp(r*time.period) * p),
             colour = ' red')

ggplot(data.frame(pl)) +
  stat_density(aes(pl) ,geom = "line",
               colour = 'steelblue')
                 
                 
             
                 
                 