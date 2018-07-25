################################################################################
# bsm Hedgin performance
################################################################################

#################################
# LIBRARY
#################################
detach("package:StockPriceSimulator", unload = T)
library(StockPriceSimulator)
library(pracma)
library(ggplot2)
library(purrr)

#################################
# VARIABLE
#################################
setwd("c:/Users/ATE/thesisDoc/data")
rm(list = ls())
load(file = "DATA.RData")
load(file = "GBM.RData")


#################################
# GENERALIZATION
#################################

#############
# To change:
#############
n_delta_hedge <- 100
# Frequency per day
frequency <- 2
##############

# To no change:
maturities <- c(91, 182, 399)#unique(DATA$maturity)
initial_stock_price <- dplyr::first(DATA$S0)
strikes <- c(140, 160, initial_stock_price , 200,  230) #unique(DATA$Strike)


domain <- expand.grid(maturities, strikes)
names(domain) <- c('maturity', 'strike')

# To have reproducible and comparable experiments.
# DO NOT CHANGE VALUE OF max_frequency
max_frequency <- 2
scale = 365
step <- max_frequency / frequency
# Number of check per day


tmp <- dplyr::select(DATA, maturity, continuous)
tmp <- tmp[!duplicated(tmp), ]
domain <- plyr::join(domain, tmp)
# domain$continuous <- 0.02144491


#################################
# TIME SERIES
#################################
tic()
args_bsm <- as.list(GBM)

ts <- map(1:n_delta_hedge, 
          ~ sstock(
            initial_stock_price = initial_stock_price,
            time_to_maturity = max(domain$maturity) / 365,
            seed = .x,
            scale = scale * max_frequency,
            sigma = args_bsm$sd,
            alpha = args_bsm$mean)) 

#################################
# OPTION PRICE
#################################
# 
# Variable to change in order to change the rebalancing frequency.
# Daily frequency
bsm_ts <- map(ts, function(x){
 z <-  x[seq(1, (max(domain$maturity) * max_frequency + 1), by = step), ]
  rownames(z) <- 1:nrow(z)
  z
})


################################################################################
# # Option price for all Geometric BM at all times
################################################################################
O_bsm <- purrr::map(data.frame(t(domain)), .f = function(z){
  purrr::map(bsm_ts, .f = function(x){
    BSM(stock_path = x[1:(z[1] * frequency + 1), ],
        sigma = args_bsm$sd,
        r = z[3],
        k = z[2])
  })
})

o_bsm_t0 <- map(O_bsm, ~ .x[[1]]$option_price_path[1])


#################################
# Delta computation
#################################
d <- purrr::map(data.frame(t(domain)), .f = function(z){
  # print(z)
  purrr::map(bsm_ts, .f = function(x){
    delta_bsm(x[1:(z[1] * frequency + 1), ],
              sigma = args_bsm$sd,
              strike = z[2],
              riskless_rate = z[3])
  })
})

# print('d')
# names(d) <- paste(domain$maturity, domain$strike, sep='/')


# Get the delta of the delta for each time series.
diff_delta <- purrr::map(d, function(z){
  map(z, ~ c(.x[1], diff(.x)))
})



l <- map(1:nrow(domain), function(x){
  ts <- map(bsm_ts, ~.x[1:(domain[x, 1] * frequency + 1), ])
  domn <- map(rep(x, length(d[[x]])), ~domain[.x, ])
  list(diff_delta[[x]],
       d[[x]],
       ts,
       O_bsm[[x]],
       domn)
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
  purrr::pmap(x, function(dd, d, g, O, domn){
    # print(d)
    # time_remaining <- (time_to_maturity * scale + 1) : 1
    time_remaining <- seq(domn$maturity/365, 0, length.out = (domn$maturity * frequency + 1))
    
    amount_paid <- dd * g$stock_price_path #- c(o[1], diff(o))
    # amount_with_interest <- amount_paid * (1 + r / (scale)) ^ (time_remaining - 1)
    amount_with_interest <- amount_paid * exp(domn$continuous * time_remaining)
    
    # print(o_bsm_t0[[paste0('X',row.names(domn))]])
    p <- o_bsm_t0[[as.double(row.names(domn))]] - cumsum(dd  * g$stock_price_path)
    # O <- map(O, ~.x$option_price_path)
    
    # print(p)
    #####################
    # test
    # o_bsm_t0 - cumsum(diff_delta[[1]] * bsm_ts[[1]]$stock_price_path[1:(period + 1)])
    # u[[1]]$delta * bsm_ts[[1]]$stock_price_path[1:(period + 1)] # Ce que j' ai en plus!
    ########################
    
    # print('---------------------------------')
    # print(length(time_remaining))
    # print(length(time_remaining[1] - time_remaining))
    # print(length(dd))
    # print(length(d))
    # print(length(O))
    # print(length(g$stock_price_path))
    # print(length(amount_paid))
    # print(length(amount_with_interest))
    # print(length(p))
    # print(length(domn$continuous))
    # print(length(amount_paid_bsm))
    # print(length(amount_with_interest_bsm))
    # print(length(p_bsm))
    # print('---------------------------------')
    
    data.frame("time remaining" = time_remaining,
               "time period" = time_remaining[1] - time_remaining,
               "diff delta" = dd,
               "delta" = d,
               "option" = O,
               "s" = g$stock_price_path,
               "amount paid" = amount_paid,
               "with interest" = amount_with_interest,
               "p" = p,
               'r' = domn$continuous)
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
#                     - o_bsm_t0 * exp(r * time_to_maturity))

pi <- purrr::map(u, function(x){
  purrr::map_dbl(x,
                 ~ dplyr::last(.x$delta) * dplyr::last(.x$s)
                 + exp(dplyr::first(.x$r) * dplyr::last(.x$time.period)) * dplyr::last(.x$p)
                 - dplyr::last(.x$option.option_price_path))
})

# pi2 <-   purrr::map_dbl(u[[1]],
#                  ~ .x$delta[time_to_maturity * scale + 1]
#                  * .x$s[time_to_maturity * scale + 1]
#                  + (exp(r*time_to_maturity) * .x$p[time_to_maturity * scale + 1])
#                  - .x$option[time_to_maturity * scale + 1])


pl <- pmap(list(u, pi), function(x, y){
  # exp(- dplyr::first(x[[1]]$r) * dplyr::last(x[[1]]$time.period)) * y / dplyr::first(x[[1]]$option)
  y / dplyr::first(x[[1]]$option.option_price_path)
})




# hedging_cost <- purrr::map_dbl(u,
#                                ~ sum(.x$with.interest) - .x$delta[time_to_maturity * scale + 1] * strike)
# hedging_cost_sd <- sd(hedging_cost)
# hedging_perf <- hedging_cost / (o_bsm_t0 * exp(r * time_to_maturity))
# 
# sum(i) / length(i)



#################################
# Plot
#################################
ggplot(u[[10]][[1]]) +
  geom_line(aes(x = time.period, y = option.option_price_path),
            colour = 'blue')+
  geom_point(aes(x = time.period, y = delta* s + exp(domain[1, 3]*time.period) * p),
             colour = ' red')

ggplot(data.frame(pl = pl[[1]])) +
  stat_density(aes(pl) ,geom = "line",
               colour = 'steelblue') +
  xlim(c(-0.5, 0.5))


#################
# Plot
#################
ggplot2::ggplot(dplyr::bind_rows(bsm_ts, .id = "uniqueID"), 
                ggplot2::aes(x = time, 
                             y = stock_price_path, 
                             group = uniqueID)) + 
  ggplot2::geom_line(ggplot2::aes(alpha = 0.5)) + 
  theme(legend.position = 'none') +
  ggplot2::labs( x = 'Time period',
                 y = 'Price')



# setwd("c:/Users/ATE/thesisDoc/data")
# u_bsm_2 <- u
# save(u_bsm_2 , file = "u_bsm_2.RData")

