################################################################################
# Heston Hedgin performance
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
rm(list = ls())
load(file = "DATA.RData")
load(file = "GBM.RData")


#################################
# GENERALIZATION
#################################
# To change:
n_delta_hedge <- 1
# Frequency per day
frequency <- 2

delta_hedging_analysis <- function(n_delta_hedge, frequency){
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
  args_bsm <- as.list(GBM)
  
  ts <- map(1:n_delta_hedge, ~ sstock(
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
      # print(paste(length(x[1:(z[1] * frequency + 1), 1]), ':', z[3]))
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
      time_elapsed <- dplyr::first(time_remaining) - time_remaining
     
      index <- map(1:(domn$maturity * frequency + 1), ~ 1:.x)
      # print(index)
      # print('----2222222222222------------------------')
      cum_delta_stock <- map_dbl(index, function(x){
        sum(dd[x]  * g$stock_price_path[x] * exp(rev(time_elapsed[x]) * domn$continuous))
      })
      
      # print(cum_delta_stock)
      # 
      p <- exp(time_elapsed * domn$continuous) * o_bsm_t0[[as.double(row.names(domn))]] -
      cum_delta_stock

      
      data.frame("time remaining" = time_remaining,
                 "time period" = time_remaining[1] - time_remaining,
                 "diff delta" = dd,
                 "delta" = d,
                 "option" = O$option_price_path,
                 "s" = g$stock_price_path,
                 "p" = p,
                 'r' = domn$continuous,
                 "ds" = d * g$stock_price_path,
                 'cum' = cum_delta_stock,
                 'h' = O - (d * g$stock_price_path + p))
    })
  })
  print(paste('for frequency' , frequency, 'and n hedge of', n_delta_hedge))
  return(u)
}


# To change:
n_delta_hedge <- 100
# Frequency per day
frequency <- 2
tic()
U_bsm <- map(c(2, 1, 1/7), ~ delta_hedging_analysis(100, .x))
toc()
U_bsm <- map(1, ~ delta_hedging_analysis(1, .x))





# i <- purrr::map_dbl(u,
#                     ~ sum(.x$with.interest)
#                     - .x$delta[time_to_maturity * scale + 1] * strike
#                     - o * (1 + r / (scale)) ^ (time_to_maturity * scale))

# i <- purrr::map_dbl(u,
#                     ~ sum(.x$with.interest)
#                     - .x$delta[time_to_maturity * scale + 1] * strike

pi <- purrr::map(u, function(x){
  purrr::map_dbl(x,
                 ~ dplyr::last(.x$delta) * dplyr::last(.x$s)
                 + dplyr::last(.x$p)
                 - dplyr::last(.x$option))
})



pl <- pmap(list(u, pi), function(x, y){
  # exp(- dplyr::first(x[[1]]$r) * dplyr::last(x[[1]]$time.period)) * y / dplyr::first(x[[1]]$option)
  y / dplyr::first(x[[1]]$option)
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
ggplot(u[[13]][[1]]) +
  geom_line(aes(x = time.period, y = option),
            colour = 'blue')+
  geom_point(aes(x = time.period, y = delta* s +  p),
             colour = ' red') 


ggplot(U_bsm[[1]][[13]][[9]]) +
  geom_line(aes(x = time.period, y = option.option_price_path),
            colour = 'blue')+
  geom_point(aes(x = time.period, y = delta* s +  p),
             colour = ' red')


U_bsm[[1]][[14]][[1]][120:140, c("option", "s", "delta", "diff.delta", "p", "ds", 'h')]

ggplot(data.frame(pl = pl[[2]])) +
  stat_density(aes(pl) ,geom = "line",
               colour = 'steelblue')+
  stat_density(data = data.frame(pl_bsm = pl_bsm[[2]]), aes(pl_bsm) ,geom = "line",
               colour = 'darkred') +
  xlim(c(-0.005, 0.02))


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
# save(U_bsm , file = "u_bsm.RData")


u <- U_bsm[[1]]
u

pi <- purrr::map(u, function(x){
  purrr::map_dbl(x,
                 ~ dplyr::last(.x$delta) * dplyr::last(.x$s)
                 + exp(dplyr::first(.x$r) * dplyr::last(.x$time.period)) * dplyr::last(.x$p)
                 - dplyr::last(.x$option))
})
pi_bsm <- purrr::map(u, function(x){
  purrr::map_dbl(x,
                 ~ dplyr::last(.x$delta.bsm) * dplyr::last(.x$s)
                 + exp(dplyr::first(.x$r) * dplyr::last(.x$time.period)) * dplyr::last(.x$p.bsm)
                 - dplyr::last(.x$option))
})