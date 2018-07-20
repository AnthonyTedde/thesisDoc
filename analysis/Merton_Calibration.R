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
    cst[i] <- DATA[i, 9] - 
      call_merton(initial_stock_price = DATA[i, 1], 
                  time_to_maturity = DATA[i, 2] / 365,
                  sigma = x[4],
                  alpha = DATA[i, 8],
                  lambda = x[1],
                  jumps_intensity_parameters = list(mean = x[2],
                                                    sd = x[3]),
                  K = DATA[i, 3]
                  )
  }
  
  # print(sum(cst^2))
  # print(x)
  
  sum(cst^2)
  
}



# Optimized result

lambda <- seq(.1, .3, by = .1)
mu <- seq(-.1, .4, by = .1)
delta<- seq(.1, .4, by = .1)
sigma <- .119459


xo <- as.data.frame(t(expand.grid(lambda, mu, delta, sigma)))
# xo <- data.frame(x1, x2)

# tosave <- l

l2 <- map(xo , function(y){
  x0 <- c("lambda" = y[1], 
          "mu" = y[2], 
          "delta" = y[3], 
          "sigma" = y[4]) 
  
  x <- tryCatch(
    lsqnonlin(cost, x0, options = list(maxeval = 2000))$x,
    error = function(c) {
      message(c)
      return(c)
    }
  )
  if(inherits(x, "error")){
    x <- c("lambda" = 0, 
           "mu" = 0, 
           "delta" = 0, 
           "sigma" = 0)
  }
  
  message(x)
  
  
  # Check optimized results accuracy   
  cst <- vector("double", length = nrow(DATA))
  for(i in 1:nrow(DATA)){
    cst[i] <- tryCatch(call_merton(initial_stock_price = DATA[i, 1], 
                                   time_to_maturity = DATA[i, 2] / 365,
                                   sigma = x[4],
                                   alpha = DATA[i, 8],
                                   lambda = x[1],
                                   jumps_intensity_parameters = list(mean = x[2],
                                                                     sd = x[3]),
                                   K = DATA[i, 3]),
                       error = function(c) NA 
    )
    if(is.na(cst[i])){
      cst[i] <- 0
      next
    }
  }
  sum(cst - DATA$price)
  
  
  # Gather both DATA and cst into a unique dataframe
  merton <- data.frame(DATA, 
                       merton = cst, 
                       diff = cst - DATA$price, 
                       within.spread = (DATA$Ask - DATA$Bid) >= abs(cst - DATA$price))
  
  
  list(check <- sum(merton$within.spread),
       x0,
       x)
  
})


# Measure the performance of the callibration
optim.1 <- l2

remove_negative_feller <- function(y){
    return(y[[1]])
}

within.spread.1 <- map_dbl(optim.1, .f = remove_negative_feller ) %>%
  unname 

# Subset optim.1 with the best values
optim.bst <- optim.1[
  !map(optim.1, function(x){
  if(x[[1]] == 45)
    return(x)
  else return(NA)
}) %>% is.na
]
map(optim.bst, `[[`, 3)

# 
# X merton to keep
# 
x_merton <- optim.bst$V21[[3]]


remove_negative_feller_fromlist <- function(y){
  if(y[[2]] > 0.1 & y[[4]][1] >0 & y[[1]] >= 42){
    return(y)
  }else{
    NA
  }
}

x_merton <- unlist(l[[13]][3])
setwd("c:/Users/ATE/thesisDoc/data")

save(x_merton, file = "optimalMertonCalibration.RData")