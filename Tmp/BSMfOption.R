library(fOptions)
library(quantmod)
library(purrr)
library(reshape)


#######################################################
## (I)                                               ##
## Get historical data (AAPL) to construct the hedge ##
## Download the data: 20 May                         ##
#######################################################

## 0. prerequisites
rm(list = ls())
setwd("/Users/anthony/workspace/thesis/thesis/data")

## 1. Load the data
## Option Datalist 18 May 16h00 (FRIDAY)
##

## (Down)Load the data
if(file.exists("option-quote.RData")){
    load(file="option-quote.RData")
}else{
    AAPL_o <- getOptionChain("AAPL", "2018/2019")
    AAPL <- getQuote("AAPL")
    save(list = ls(), file="option-quote.RData")
}

## avail_strikes: All the strikes available for the dowloaded option quotes
avail_stikes <- map(AAPL_o, `$`, calls) %>%
  map(`$`, Strike) %>%
  unlist %>% unname %>% unique %>% sort

## maturity: All the maturity available for the downloaded option quotes
## maturity is still a character vector, it will be subsequently transformed
## into a date vector
maturity <- names(AAPL_o)

## dfs_call: temporary data used to construct the price surface
## It is a list containing all the option price  for all strike and maturity.
dfs_call <- map(AAPL_o, `$`, call) %>%
  map(~.x[, 1:2]) %>%
  Filter(f = Negate(is.null))

price_surface <- dfs_call[[1]]
for(i in names(dfs_call[-1])){
  print(i)
  price_surface <- merge(price_surface, dfs_call[[i]], by = "Strike", all = T)
}
colnames(price_surface) <- c("Strike", names(dfs_call))

# Find the implied volatility
# 1. Delta date
maturity <- names(price_surface)[-1] %>%
  as.Date(format = "%b.%d.%Y") - as.Date("2018-05-18")
  
for(i in seq_along(names(price_surface)[-1])){
  print(price_surface[[i + 1]])
  print(maturity[i])
  print(price_surface$Strike)
  print(AAPL$Last)
}


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

q <- as.data.frame(volatility_surface) %>% round(digit = 4)
names(q) <- names(price_surface)
q

b <- map(as.data.frame(a), .f = function(x){
  structure(as.list(x),
            names = c("price", "Time", "X", "S" ))
})  %>%
  map(c,TypeFlag = "c", r = .05, b= 0) %>%
  map(.f = function (x){
    print(x)
    if(!is.na(x$price))
      do.call(GBSVolatility, x)
    else NA
    })


130, 59.13, 186.31
dt <- as.double(ISOdate(2018, 05, 25) - ISOdate(2018, 05, 18))
GBSVolatility(price = (59.25 + 58.15)/2,
              TypeFlag = 'c',
              S = (187.81 + 186.13) / 2,
              X = 130,
              Time = (dt) / 250,
              r = .05,
              b = 0)

GBSVolatility(price = 187.49,
              TypeFlag = 'c',
              S = 186.31,
              X = 2.5,
              Time = 0.4219,
              r = .05,
              b = 0.0)

dt <- as.double(ISOdate(2018, 06, 08) - ISOdate(2018, 05, 18))
GBSVolatility(price = 44.85,
              TypeFlag = 'c',
              S = 186.31,
              X = 145,
              Time = (dt) / 365,
              r = .05,
              b = 0.0)









dt <-  as.double(ISOdate(2018, 11, 16) - Sys.time())

o[[6]][[1]][10, ]

GBSVolatility(price = 36.5,
              TypeFlag = "c",
              S = 186.4,
              X = 155,
              Time = dt / 365,
              r = 0.03,
              b = 0)
         

GBSOption(TypeFlag = "c",
          S = 186.4,
          X = 155,
          Time = dt / 365,
          r = 0.03,
          b = 0,
          sigma = .3495)


1. Quelle volatilité à utiliser pour calculer le prix des calls ? implied ou historique ? 
Si implied -> elle sera stochastique et dépendra du prix de l'action et du temps. Dès lors, il ne s'agit pas de la stricte application du modèle BSM.

2. Quelle volatilité à utiliser pour simuler les series 
