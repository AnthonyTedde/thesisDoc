library(purrr) 
library(StockPriceSimulator)
library(ggplot2)  
library(Quandl)
library(dplyr)
library(fOptions)

###############
## Variables ##
###############
# Convention ACT360 used
t <- 1 / 365
## day represents the number of days from the start date where the data
## should be downladed
day <-  365 
dt <- day ** -1
date_sequence <- seq(ISOdate(2017, 5, 18), ISOdate(2018, 5, 18), by = 'day')

days <- seq(ISOdate(2017,01,01), by = "day", length.out = day) %>%
  format("%F") %>%
  paste(collapse = ",")

###################
## Download data ##
###################
Quandl.api_key("VZSMB9uWUzm_VBrugWy5") 
AAPL <- Quandl.datatable('WIKI/PRICES', date = days, ticker = "AAPL")

##################
## Extract data ##
##################
quote <- (AAPL$high + AAPL$low)/2
day <- AAPL$date
days <- length(day)
interval <- day[-1] - day[-days]

quotes <- length(quote)
quotediff <- quote[-1] / quote[-quotes]

df <- data.frame(quotediff, interval)
u <- log(filter(df, interval ==1 )$quotediff)


## u <- log(quote[-1] / quote[-quotes])
ubar <- mean(u)

s <- sqrt(1/(length(u) - 1) * sum((u - ubar)^2)) 

#
# Sigma and mean will be used inside the GBM simulation
#
sigma <- s / sqrt(t)
mean <- ubar / t + sigma ^2 / 2

######################################################################
## Simulate a Geometric Brownian Motion based on the collected data ##
######################################################################
GBM <- sstock(initial_stock_price = quote[1],
              seed = 1,
              alpha = mean,
              sigma = sigma,
              time_to_maturity = 1,
              scale = 365)
# and compute the log-return
GBM_price <- GBM$stock_price_path
GBM_prices <- length(GBM_price)
GBM_logreturn <- log(GBM_price[-1] / GBM_price[-GBM_prices])

  
ggplot(data.frame(u)) +
  stat_density(aes(u),
               geom = "line",
               colour = 'steelblue') +
  stat_density(data = data.frame(GBM_logreturn), aes(GBM_logreturn),
               geom = "line",
               colour = 'darkred') +
  stat_function(fun = dnorm,
                colour = "black",
                args = list(
                  mean = ubar,
                  sd = sigma * sqrt(t) ))

########################################################################
##  Hedging strategy using GBM 
########################################################################
# Hedging performance in a perfectly log-normal world
#
# 1. Get all the maturity and all the strike prices
#
load(file="option-quote.RData")

# 1.1. All the strike price available for all the maturity
avail_stikes <- map(AAPL_o, `$`, calls) %>%
  map(`$`, Strike) %>%
  unlist %>% unname %>% unique %>% sort

# 1.2 Covered maturities
maturity <- names(AAPL_o)

# 1.3 Construct the price surface
dfs_call <- map(AAPL_o, `$`, call) %>%
  map(~.x[, 1:2]) %>%
  Filter(f = Negate(is.null))

price_surface <- dfs_call[[1]]
for(i in names(dfs_call[-1])){
  print(i)
  price_surface <- merge(price_surface, dfs_call[[i]], by = "Strike", all = T)
}
colnames(price_surface) <- c("Strike", names(dfs_call)) 
# First subsetting of the price surface
price_surface <- structure(price_surface[23:57, -4])
rownames(price_surface) <- 1:nrow(price_surface)

# 1.4 Convert the maturity date based on the price surface
maturity <- names(price_surface)[-1] %>%
  as.Date(format = "%b.%d.%Y") - as.Date("2018-05-18")

# 1.5 Compute the volatility surface
volatility_surface <- vector("list", length(price_surface))
volatility_surface[[1]] <- price_surface[1]
for(i in seq_along(names(price_surface)[-1])){
  a <-  rbind(price_surface[[i + 1]],
              maturity[i] / 365,
              price_surface$Strike,
              AAPL$Last)
  a <- map(as.data.frame(a), .f = function(x){
    structure(as.list(x),
              names = c("price", "Time", "X", "S" ))
  })
  b <- a %>%
    map(c,TypeFlag = "c", r = .05, b= 0) %>%
    map(.f = function (x){
      if(!is.na(x$price))
        tryCatch(do.call(GBSVolatility, x),
                 error = function(c) NA)
      else NA
    })
  volatility_surface[[i + 1]] <- unname(unlist(b))
}

volatility_surface <- as.data.frame(volatility_surface) %>% round(digit = 4)
names(volatility_surface) <- names(price_surface)
volatility_surface


maturity <- as.double(maturity)
strike <- volatility_surface$Strike


# 1. BSM price at time zero
# sigma, mean, 
# time to maturity: 7  21  35  63  91 126 154 182 245 399
hedging <- function(maturity, strike){
  o <- GBSOption(TypeFlag = "c",
                 S = AAPL$Last,
                 X = strike,
                 Time = maturity / 365,
                 r = 0.05,
                 b = 0,
                 sigma = sigma)
  
  # 2. Simulate 500 GRM that correspond to that date
  GBM <- map(1:500, ~sstock(initial_stock_price = AAPL$Last,
                     seed = .x,
                     alpha = mean,
                     sigma = sigma,
                     time_to_maturity = 399/365,
                     scale = 365))
   
  # 3. Compute the delta for all step
  # NEW
  d <- map(GBM, ~.x[1:(maturity + 1), ]) %>%
    map(.f = function(x){
      x$time_periods <- rev(x$time_periods)
      x
    }) %>%
    map(~GBSGreeks(Selection = "Delta",
                  TypeFlag = "c",
                  S = .x$stock_price_path,
                  X = strike,
                  Time = .x$time_periods,
                  r = 0.05,
                  b = 0,
                  sigma = sigma)) 
  
  # OLD
  # d <- vector("list", 500)
  # S <- vector("list", 500)
  diff_delta <- vector("list", 500)
  share_diff_cost <- vector("list", 500)
  interest <- vector("list", 500)
  cum_cost <- vector("list", 500)
  reminder <- vector("numeric", 500)
  # for(j in 1:500){
  #   d[[j]] <- vector("numeric", maturity + 1)
  #   S[[j]] <- vector("numeric", maturity + 1)
  #   for(i in 1:(maturity + 1)){
  #     S[[j]][i] <- GBM[[j]]$stock_price_path[i]
  #     d[[j]][i] <- GBSGreeks(Selection = "Delta",
  #                            TypeFlag = "c",
  #                            S = S[[j]][i],
  #                            X = strike,
  #                            Time = ((maturity + 1) - i)/365,
  #                            r = 0.05,
  #                            b = 0,
  #                            sigma = sigma)
  #     
  #     # Cost of share purchase
  #     
  #     #Compute the interest to paid
  #     # interest[i] <- 5
  #   }
    # Share purchase
  diff_delta <- map(d, ~c(.x[1], diff(.x)))
    # diff_delta[[j]] <- c(d[[j]][1], diff(d[[j]]))
    # Cost of share purchase
  share_diff_cost <- map(1:500, .f = function(x){
    GBM[[x]]$stock_price_path[1:(maturity + 1)] * diff_delta[[x]]})
    # share_diff_cost[[j]] <- S[[j]] * diff_delta[[j]]
    # Interest
  interest <- map(share_diff_cost, ~cumsum(.x * 0.05 / 365))
    # interest[[j]] <- cumsum(share_diff_cost[[j]]) * 0.05 / 365
    # Cumulativa cost including interest
  cum_cost <- map(1:500, .f = function(x){
    cumsum(share_diff_cost[[x]])[-1] + interest[[x]][-length(interest[[x]])]})
  
    # cum_cost[[j]] <- cumsum(share_diff_cost[[j]])[-1] + interest[[j]][-length(interest[[j]])]
    # at the end:
  reminder <- map_dbl(1:500, .f = function(x){
    tail(cum_cost[[x]], 1) - o@price * (1 + 0.05 / 365 )^maturity - d[[x]][maturity + 1 ] * strike
  })
    # reminder[j] <- tail(cum_cost[[j]], 1) - o@price * (1 + 0.05 / 365 )^maturity - d[[j]][maturity + 1 ] * K
  # }
  reminder
}

strike <- 186
maturity <- 7
hedging(maturity = maturity, strike = strike)

0.02610991
0.03329432




















