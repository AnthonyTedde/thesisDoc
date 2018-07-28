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
load(file = "optimalHestonCalibration.RData")
load(file = "optimalHestonRiskaverse.RData")
load(file = "DATA.RData")


#################################
# GENERALIZATION
#################################
# To change:
n_delta_hedge <- 2
# Frequency per day
frequency <- 1

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
  args_heston <- as.list(heston_riskaverse)
  args_heston_riskneutral <- as.list(x)
  
  ts <- map(1:n_delta_hedge, ~ heston(
    initial_stock_price = initial_stock_price,
    initial_volatility = args_heston_riskneutral$v0,
    time_to_maturity = max(domain$maturity) / 365,
    seed = .x,
    scale = scale * max_frequency,
    alpha = args_heston$alpha,
    rho = args_heston_riskneutral$rho,
    kappa = args_heston$kappa,
    theta = args_heston$theta,
    sigma = args_heston_riskneutral$sigma
  ))
  # remove NA
  CIR.last <- map_dbl(ts, ~ tail(.x$CIR, 1)) %>% unname
  toremove <- which(is.na(CIR.last))
  if(! is_empty(toremove))
    ts <- ts[-toremove]
  
  #################################
  # OPTION PRICE
  #################################
  # 
  # Variable to change in order to change the rebalancing frequency.
  # Daily frequency
  heston_ts <- map(ts, function(x){
    z <-  x[seq(1, (max(domain$maturity) * max_frequency + 1), by = step), ]
    rownames(z) <- 1:nrow(z)
    z
  })
  
  
  
  o_heston_t0 <-  purrr::map(data.frame(t(domain)), function(x){
    call_heston(initial_stock_price = initial_stock_price,
                initial_volatility = args_heston_riskneutral$v0,
                time_to_maturity = x[1]/365,
                theta = args_heston_riskneutral$theta,
                kappa = args_heston_riskneutral$kappa,
                sigma = args_heston_riskneutral$sigma,
                alpha = x[3],
                rho = args_heston_riskneutral$rho,
                K = x[2])
  })
  
  
  ################################################################################
  # # Option price for all Geometric BM at all times
  ################################################################################
  O_heston <- purrr::map(data.frame(t(domain)), .f = function(z){
    purrr::map(heston_ts, .f = function(x){
      df <- data.frame(x$stock_price_path[1:(z[1] * frequency + 1)], x$time[1:(z[1] * frequency + 1)])
      purrr::map_dbl(as.data.frame(t(df)), .f = function(y){
        if(z[1] / 365 - y[2] == 0){
          tau <- 1/365/24
        }else{
          tau <- z[1] / 365 - y[2]
        }
        call_heston(initial_stock_price = y[1],
                    initial_volatility = args_heston_riskneutral$v0,
                    time_to_maturity = tau,
                    alpha = z[3],
                    theta = args_heston_riskneutral$theta,
                    kappa = args_heston_riskneutral$kappa,
                    sigma = args_heston_riskneutral$sigma,
                    rho = args_heston_riskneutral$rho,
                    K = z[2])
      })
    })
  })
  
  
  #################################
  # Delta computation
  #################################
  d <- purrr::map(data.frame(t(domain)), .f = function(z){
    # print(z)
    purrr::map(heston_ts, .f = function(x){
      delta_heston(x$stock_price_path[1:(z[1] * frequency + 1)],
                   V = x$CIR[1:(z[1] * frequency + 1)],
                   time_to_maturity = z[1] / 365,
                   K = z[2],
                   alpha = z[3],
                   theta = args_heston_riskneutral$theta,
                   kappa = args_heston_riskneutral$kappa,
                   sigma = args_heston_riskneutral$sigma,
                   rho = args_heston_riskneutral$rho
      )
    })
  })
  
  
  diff_delta <- purrr::map(d, function(z){
    map(z, ~ c(.x[1], diff(.x)))
  })
  
  #################################
  # Delta BSM computation
  #################################
  heston_ts_bsm <- map(heston_ts, function(x){
    z <- x[, c(1:2, 5)]
    names(z) <- c('time_periods', 'stock_price_path', 'CIR')
    z
  })
  
  d_bsm <- purrr::map(data.frame(t(domain)), .f = function(z){
    # print(z)
    purrr::map(heston_ts_bsm, .f = function(x){
      # print(x$CIR[1:(z[1] * frequency + 1)])
      delta_bsm(x[1:(z[1] * frequency + 1), 1:2],
                sigma = x$CIR[1:(z[1] * frequency + 1)],
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
  
  
  
  l <- map(1:nrow(domain), function(x){
    ts <- map(heston_ts, ~.x[1:(domain[x, 1] * frequency + 1), ])
    domn <- map(rep(x, length(d[[x]])), ~domain[.x, ])
    list(diff_delta[[x]],
         d[[x]],
         ts,
         O_heston[[x]],
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
      time_remaining <- seq(domn$maturity/365, 0, length.out = (domn$maturity * frequency + 1))
      time_elapsed <- dplyr::first(time_remaining) - time_remaining
      
      amount_paid <- dd * g$stock_price_path #- c(o[1], diff(o))
      amount_paid_bsm <- dd_bsm * g$stock_price_path
      # amount_with_interest <- amount_paid * (1 + r / (scale)) ^ (time_remaining - 1)
      amount_with_interest <- amount_paid * exp(domn$continuous * time_remaining)
      amount_with_interest_bsm <- amount_paid_bsm * exp(domn$continuous * time_remaining)
      
      # print(o_heston_t0[[paste0('X',row.names(domn))]])
      # print('---11111111111-------------------------')
      # print((domn$maturity * frequency + 1))
      index <- map(1:(domn$maturity * frequency + 1), ~ 1:.x)
      # print(index)
      # print('----2222222222222------------------------')
      cum_delta_stock <- map_dbl(index, function(x){
        sum(dd[x]  * g$stock_price_path[x] * exp(rev(time_elapsed[x]) * domn$continuous))
      })
      cum_delta_bsm_stock <- map_dbl(index, function(x){
        sum(dd_bsm[x]  * g$stock_price_path[x] * exp(rev(time_elapsed[x]) * domn$continuous))
      })
      # print(cum_delta_stock)
      # 
      p <- exp(time_elapsed * domn$continuous) * o_heston_t0[[as.double(row.names(domn))]] - 
        cum_delta_stock
      p_bsm <- exp(time_elapsed * domn$continuous) * o_heston_t0[[as.double(row.names(domn))]] -
        cum_delta_bsm_stock
      
      
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
                 "diff delta bsm" = dd_bsm,
                 "ds" = d * g$stock_price_path,
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
U_heston <- map(c(2, 1, 1/7), ~ delta_hedging_analysis(100, .x))
toc()
U_heston <- map(1, ~ delta_hedging_analysis(1, .x))





# i <- purrr::map_dbl(u,
#                     ~ sum(.x$with.interest)
#                     - .x$delta[time_to_maturity * scale + 1] * strike
#                     - o * (1 + r / (scale)) ^ (time_to_maturity * scale))

# i <- purrr::map_dbl(u,
#                     ~ sum(.x$with.interest)
#                     - .x$delta[time_to_maturity * scale + 1] * strike
#                     - o_heston_t0 * exp(r * time_to_maturity))
u <- U_heston[[]]
pi <- purrr::map(u, function(x){
  purrr::map_dbl(x,
                 ~ dplyr::last(.x$delta) * dplyr::last(.x$s)
                 + dplyr::last(.x$p)
                 - dplyr::last(.x$option))
})
pi_bsm <- purrr::map(u, function(x){
  purrr::map_dbl(x,
                 ~ dplyr::last(.x$delta.bsm) * dplyr::last(.x$s)
                 + dplyr::last(.x$p.bsm)
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
pl_bsm <- pmap(list(u, pi_bsm), function(x, y){
  # exp(- dplyr::first(x[[1]]$r) * dplyr::last(x[[1]]$time.period)) * y / dplyr::first(x[[1]]$option)
  y / dplyr::first(x[[1]]$option)
})




# hedging_cost <- purrr::map_dbl(u,
#                                ~ sum(.x$with.interest) - .x$delta[time_to_maturity * scale + 1] * strike)
# hedging_cost_sd <- sd(hedging_cost)
# hedging_perf <- hedging_cost / (o_heston_t0 * exp(r * time_to_maturity))
# 
# sum(i) / length(i)



#################################
# Plot
#################################
ggplot(u[[12]][[1]]) +
  geom_line(aes(x = time.period, y = option),
            colour = 'blue')+
  geom_point(aes(x = time.period, y = delta* s +  p),
             colour = ' red')+
  geom_point(aes(x = time.period, y = delta.bsm* s +  p.bsm),
             colour = 'black')


U_heston[[1]][[14]][[1]][120:140, c("option", "s", "delta", "diff.delta", "p", "ds", 'h')]

ggplot(data.frame(pl = pl[[2]])) +
  stat_density(aes(pl) ,geom = "line",
               colour = 'steelblue')+
  stat_density(data = data.frame(pl_bsm = pl_bsm[[2]]), aes(pl_bsm) ,geom = "line",
               colour = 'darkred') +
  xlim(c(-0.005, 0.02))


#################
# Plot
#################
ggplot2::ggplot(dplyr::bind_rows(heston_ts, .id = "uniqueID"), 
                ggplot2::aes(x = time, 
                             y = stock_price_path, 
                             group = uniqueID)) + 
  ggplot2::geom_line(ggplot2::aes(alpha = 0.5)) + 
  theme(legend.position = 'none') +
  ggplot2::labs( x = 'Time period',
                 y = 'Price')



# setwd("c:/Users/ATE/thesisDoc/data")
# save(U_heston , file = "u_heston.RData")
# load(file = "u_heston.RData")


u <- U_heston[[1]]
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



# dist <- map_dbl(U_heston[[1]][[9]], ~dplyr::last(.x$s))
# ggplot(data.frame(dist)) +
#   stat_density(aes(dist))