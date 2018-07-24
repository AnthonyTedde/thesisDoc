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

# initial_stock_price = 186.31
# period <- 67
# time_to_maturity = 67/365
# seed = 1
# 
# K = 180
# strike <- K
# r <- 0.02144491

#################################
# GENERALIZATION
#################################
n_delta_hedge <- 100
maturities <- c(91, 182, 399)#unique(DATA$maturity)
strikes <- c(140, 160, initial_stock_price, 200,  230) #unique(DATA$Strike)

# maturities <- c(67)#unique(DATA$maturity)
# strikes <- c(180) #unique(DATA$Strike)

domain <- expand.grid(maturities, strikes)
names(domain) <- c('maturity', 'strike')

#? scale ??
scale = 365

tmp <- dplyr::select(DATA, maturity, continuous)
tmp <- tmp[!duplicated(tmp), ]
domain <- plyr::join(domain, tmp)
# domain$continuous <- 0.02144491


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
  time_to_maturity = max(maturity),
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
o_merton_t0 <-  purrr::map(data.frame(t(domain)), function(x){
  call_merton(initial_stock_price = initial_stock_price,
              time_to_maturity = x[1]/365,
              seed = 1,
              scale = 365, # Daily measurement
              sigma = x_merton['sigma'],
              alpha = x[3],
              lambda =  x_merton['lambda'],
              jumps_intensity_parameters = list(mean = x_merton['mu'],
                                                sd = x_merton['delta']),
              K = x[2])
})
  

# # Option price for all Geometric BM at all times
O_merton <- purrr::map(data.frame(t(domain)), .f = function(z){
  # print(z)
  purrr::map(merton_ts, .f = function(x){
    df <- data.frame(x$stock_price_path[1:(z[1]+1)], x$time[1:(z[1]+1)])
    purrr::map_dbl(as.data.frame(t(df)), .f = function(y){
      if(z[1] / 365 - y[2] == 0)
        tau <- 1/365/24
      else
        tau <- z[1] / 365 - y[2]
      call_merton(initial_stock_price = y[1],
                  time_to_maturity = tau,
                  seed = 1,
                  scale = 365, # Daily measurement
                  sigma = x_merton['sigma'],
                  alpha = z[3],
                  lambda =  x_merton['lambda'],
                  jumps_intensity_parameters = list(mean = x_merton['mu'],
                                                    sd = x_merton['delta']),
                  K = z[2])
    })
  })
})
# names(O_merton) <- paste(domain$maturity, domain$strike, sep='/')
# print('O_merton')
# O_merton <- purrr::map(merton_ts, .f = function(x){
#   df <- data.frame(x$stock_price_path[1:(period+1)], x$time[1:(period+1)])
#   purrr::map_dbl(as.data.frame(t(df)), .f = function(y){
#     if(time_to_maturity - y[2] == 0)
#       tau <- 1/365/24
#     else
#       tau <- time_to_maturity - y[2]
#     call_merton(initial_stock_price = y[1],
#                 time_to_maturity = tau,
#                 seed = 1,
#                 scale = 365, # Daily measurement
#                 sigma = x_merton['sigma'],
#                 alpha = r,
#                 lambda =  x_merton['lambda'],
#                 jumps_intensity_parameters = list(mean = x_merton['mu'],
#                                                   sd = x_merton['delta']),
#                 K = K)
#   })
# })

#################################
# Delta computation
#################################
d <- purrr::map(data.frame(t(domain)), .f = function(z){
  # print(z)
  purrr::map(merton_ts, .f = function(x){
    delta_merton(x$stock_price_path[1:(z[1] + 1)],
                 time_to_maturity = z[1] / 365,
                 K = z[2],
                 sigma = x_merton['sigma'],
                 alpha = z[3],
                 lambda = x_merton['lambda'],
                 jumps_intensity_parameters  = list(mean = x_merton['mu'],
                                                    sd = x_merton['delta']))
  })
})
# print('d')
# names(d) <- paste(domain$maturity, domain$strike, sep='/')


# Get the delta of the delta for each time series.
diff_delta <- purrr::map(d, function(z){
  map(z, ~ c(.x[1], diff(.x)))
})

#################################
# Delta BSM computation
#################################
merton_ts_bsm <- map(merton_ts, function(x){
  z <- x[, 1:2]
  names(z) <- c('stock_price_path','time_periods')
  z
  })

d_bsm <- purrr::map(data.frame(t(domain)), .f = function(z){
  # print(z)
  purrr::map(merton_ts_bsm, .f = function(x){
    delta_bsm(x[1:(z[1] + 1), ],
              sigma = x_merton['sigma'],
              strike = z[2],
              riskless_rate = z[3])
  })
})

# print('d')
# names(d) <- paste(domain$maturity, domain$strike, sep='/')


# Get the delta of the delta for each time series.
diff_delta_bsm <- purrr::map(d_bsm, function(z){
  map(z, ~ c(.x[1], diff(.x)))
})
                         

# list of arrays used in functional pmap structure.
# l <- map(1:nrow(domain), function(x){
#   ts <- map(merton_ts, ~.x[1:(domain[x, 1] + 1), ])
#   domn <- map(rep(x, length(d[[x]])), ~domain[.x, ])
#   list(diff_delta = diff_delta[[x]],
#     d = d[[x]],
#     merton_ts = ts,
#     O_merton = O_merton[[x]])#,
#     # domain = domn)
# })

l <- map(1:nrow(domain), function(x){
  ts <- map(merton_ts, ~.x[1:(domain[x, 1] + 1), ])
  domn <- map(rep(x, length(d[[x]])), ~domain[.x, ])
  list(diff_delta[[x]],
       d[[x]],
       ts,
       O_merton[[x]],
       domn,
       diff_delta_bsm[[x]],
       d_bsm[[x]])
})



# names(l) <- paste(domain$maturity, domain$strike, sep='/')


# Output a dataframe containing:
#   * The remaining time till maturity
#   * The delta of the delta. By convention the first item is the value of
#     delta at time 0
#   * The value of delta for each time
#   * The value of the stock for each time following a Geom. Brownian Motion
#   * The Total amount paid to get delta stock at time t
#   * The Total amout paid to get delta stock at time t, but value for T
u <- purrr::map(l, function(x){
  # print(x)
  purrr::pmap(x, function(dd, d, g, O, domn, dd_bsm, d_bsm){
    # print(d)
    # time_remaining <- (time_to_maturity * scale + 1) : 1
    time_remaining <- seq(domn$maturity/365, 0, length.out = (domn$maturity + 1))
    
    amount_paid <- dd * g$stock_price_path #- c(o[1], diff(o))
    amount_paid_bsm <- dd_bsm * g$stock_price_path
    # amount_with_interest <- amount_paid * (1 + r / (scale)) ^ (time_remaining - 1)
    amount_with_interest <- amount_paid * exp(domn$continuous * time_remaining)
    amount_with_interest_bsm <- amount_paid_bsm * exp(domn$continuous * time_remaining)
    
    # print(o_merton_t0[[paste0('X',row.names(domn))]])
    p <- o_merton_t0[[as.double(row.names(domn))]] - cumsum(dd  * g$stock_price_path)
    p_bsm <- o_merton_t0[[as.double(row.names(domn))]] - cumsum(dd_bsm  * g$stock_price_path)

    # print(p)
    #####################
    # test
    # o_merton_t0 - cumsum(diff_delta[[1]] * merton_ts[[1]]$stock_price_path[1:(period + 1)])
    # u[[1]]$delta * merton_ts[[1]]$stock_price_path[1:(period + 1)] # Ce que j' ai en plus!
    ########################
    
    print('---------------------------------')
    print(length(time_remaining))
    print(length(time_remaining[1] - time_remaining))
    print(length(dd))
    print(length(d))
    print(length(O))
    print(length(g$stock_price_path))
    print(length(amount_paid))
    print(length(amount_with_interest))
    print(length(p))
    print(length(domn$continuous))
    print(length(amount_paid_bsm))
    print(length(amount_with_interest_bsm))
    print(length(p_bsm))
    print('---------------------------------')
    
    data.frame("time remaining" = time_remaining,
               "time period" = time_remaining[1] - time_remaining,
               "diff delta" = dd,
               "delta" = d,
               "option" = O,
               "s" = g$stock_price_path,
               "amount paid" = amount_paid,
               "with interest" = amount_with_interest,
               "p" = p,
               'r' = domn$continuous,
               "amount paid bsm" = amount_paid_bsm,
               "with interest bsm" = amount_with_interest_bsm,
               "p bsm" = p_bsm,
               "delta bsm" = d_bsm,
               "diff delta bsm" = dd_bsm)
  })
})
toc()







# i <- purrr::map_dbl(u,
#                     ~ sum(.x$with.interest)
#                     - .x$delta[time_to_maturity * scale + 1] * strike
#                     - o * (1 + r / (scale)) ^ (time_to_maturity * scale))

# i <- purrr::map_dbl(u,
#                     ~ sum(.x$with.interest)
#                     - .x$delta[time_to_maturity * scale + 1] * strike
#                     - o_merton_t0 * exp(r * time_to_maturity))

pi <- purrr::map(u, function(x){
  purrr::map_dbl(x,
                 ~ dplyr::last(.x$delta) * dplyr::last(.x$s)
                 + exp(dplyr::first(.x$r) * dplyr::last(.x$time.period)) * dplyr::last(.x$p)
                 - dplyr::last(.x$option))
})

  
# pi2 <-   purrr::map_dbl(u[[1]],
#                  ~ .x$delta[time_to_maturity * scale + 1]
#                  * .x$s[time_to_maturity * scale + 1]
#                  + (exp(r*time_to_maturity) * .x$p[time_to_maturity * scale + 1])
#                  - .x$option[time_to_maturity * scale + 1])


pl <- pmap(list(u, pi), function(x, y){
  # exp(- dplyr::first(x[[1]]$r) * dplyr::last(x[[1]]$time.period)) * y / dplyr::first(x[[1]]$option)
  y / dplyr::first(x[[1]]$option)
})
  
                    
      

# hedging_cost <- purrr::map_dbl(u,
#                                ~ sum(.x$with.interest) - .x$delta[time_to_maturity * scale + 1] * strike)
# hedging_cost_sd <- sd(hedging_cost)
# hedging_perf <- hedging_cost / (o_merton_t0 * exp(r * time_to_maturity))
# 
# sum(i) / length(i)

           
                 
#################################
# Plot
#################################
ggplot(u[[1]][[9]]) +
  geom_line(aes(x = time.period, y = option),
            colour = 'blue')+
  geom_point(aes(x = time.period, y = delta* s + exp(domain[1, 3]*time.period) * p),
             colour = ' red')+
  geom_point(aes(x = time.period, y = delta.bsm* s + exp(domain[1, 3]*time.period) * p.bsm),
             colour = 'black')

ggplot(data.frame(pl = pl[[13]])) +
  stat_density(aes(pl) ,geom = "line",
               colour = 'steelblue')
                 
                 
                 
                 
                 