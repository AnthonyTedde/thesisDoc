library(fOptions)
library(quantmod)
library(purrr)
library(reshape)


#
# Download the data: 20 May
#

# 1. Option Datalist 18 May 16h00 (FRIDAY)
# remove all data from workspace before saving
rm(list = ls())
TSLA_o <- getOptionChain("TSLA", "2018/2019")
get
AAPL_o <- getOptionChain("AAPL", "2018/2019")
AAPL <- getQuote("AAPL")
TSLA <- getQuote("TSLA")
# Save the data to deal with 
setwd("/Users/anthony/workspace/thesis/thesis/data")
save(list = ls(), file="option-quote.RData")

# Deal with option AAPL
# 1. Construct the volatility surface
# 1.1 Covered strike price
avail_stikes <- map(AAPL_o, `$`, calls) %>%
  map(`$`, Strike) %>%
  unlist %>% unname %>% unique %>% sort
# 1.2 Covered maturities
maturity <- names(AAPL_o)

# 1.3 Construct the data.frame
# Remove period with no value (NULL in list)
dfs_call <- map(AAPL_o, `$`, call) %>%
  map(~.x[, 1:2]) %>%
  Filter(f = Negate(is.null))

a <- dfs_call[[1]]
for(i in names(dfs_call[-1])){
  print(i)
  a <- merge(a, dfs_call[[i]], by = "Strike", all = T)
}
colnames(a) <- names(c("Strike", dfs_call))

merge_recurse(dfs_call, by = "Strike")

merge(AAPL_o[[1]]$calls[, 1:2], AAPL_o[[3]]$calls[, 1:2], by = "Strike", all = T)

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
