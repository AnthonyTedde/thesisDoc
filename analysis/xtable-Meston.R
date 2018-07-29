################################################################################
# Analysis table + plots
################################################################################

#################################
# LIBRARY
#################################
detach("package:StockPriceSimulator", unload = T)
library(StockPriceSimulator)
library(pracma)
library(ggplot2)
library(purrr)

setwd("c:/Users/ATE/thesisDoc/data")
rm(list = ls())
load(file = "u_heston.RData")
load(file = "DOMAIN.RData")

#################################
# Compute the profit and loss
#################################

pi <- map(U_heston, function(u){ 
  purrr::map(u, function(x){
    purrr::map_dbl(x,
                   ~ dplyr::last(.x$delta) * dplyr::last(.x$s)
                   + dplyr::last(.x$p)
                   - dplyr::last(.x$option))
  })
})

pi_bsm <- map(U_heston, function(u){ 
  purrr::map(u, function(x){
    purrr::map_dbl(x,
                   ~ dplyr::last(.x$delta.bsm) * dplyr::last(.x$s)
                   + dplyr::last(.x$p.bsm)
                   - dplyr::last(.x$option))
  })
})

pl <- pmap(list(U_heston, pi), function(u, pi){ 
  pmap(list(u, pi), function(x, y){
    # exp(- dplyr::first(x[[1]]$r) * dplyr::last(x[[1]]$time.period)) * y / dplyr::first(x[[1]]$option)
    y / dplyr::first(x[[1]]$option)
  })
})

pl_bsm <- pmap(list(U_heston, pi_bsm), function(u, pi_bsm){ 
  pmap(list(u, pi_bsm), function(x, y){
    # exp(- dplyr::first(x[[1]]$r) * dplyr::last(x[[1]]$time.period)) * y / dplyr::first(x[[1]]$option)
    y / dplyr::first(x[[1]]$option)
  })
})

#################################
# create the table
#################################
l <- map(1:3, function(x){
  map(pl, function(y){
    map_dbl(y[seq(x, 15, by = 3)], mean) %>% round(3)
  }) %>%
    pmap(c) %>% unlist
}) 
l_bsm <- map(1:3, function(x){
  map(pl_bsm, function(y){
    map_dbl(y[seq(x, 15, by = 3)], mean) %>% round(3)
  }) %>%
    pmap(c) %>% unlist
}) 
xt <- pmap(list(l, l_bsm), list) %>% as.data.frame
colnames(xt) <- 1:6

xt <- data.frame(strike = as.integer(domain$strike), 
                 frequency = rep(c('intraday', 'daily', 'weekly'), 
                                 length(unique(domain$strike))),
                 xt,
                 stringsAsFactors = F) 
xt <- rbind(c(rep('', 2), map_chr(c(91, 91, 182, 182, 399, 399)
                                  , paste, 'days before maturity')),
            c(rep('', 2),
              rep(c('dddhst', 'dddbsm'), 3)),
            xt)
print(xtable::xtable(xt,
                     align = "lllllllll",  # align and put a vertical line (first "l" again represents column of row numbers)
                     caption = "Hedging with HSV: Relative P\\&L", 
                     label = "t:analysis:heston:pl"),
      include.rownames = FALSE,
      include.colnames = FALSE)


#####################################
# TEST
#####################################
# l <- map(1:3, function(x){
#   map(pl, function(y){
#     map_dbl(y[seq(x, 15, by = 3)], mean)
#   }) %>%
#     pmap(c) %>% unlist
# }) 
# l_bsm <- map(1:3, function(x){
#   map(pl_bsm, function(y){
#     map_dbl(y[seq(x, 15, by = 3)], mean)
#   }) %>%
#     pmap(c) %>% unlist
# }) 
# xt <- pmap(list(l, l_bsm), list) %>% as.data.frame
# colnames(xt) <- 1:6
#####################################

x <- 1 # intraday
y <- 3 # weekly
ggplot(U_heston[[x]][[14]][[14]]) +
  geom_line(aes(x = time.period, y = option),
            colour = 'blue')+
  geom_point(aes(x = time.period, y = delta* s +  p),
             colour = ' red')+
  geom_point(data = U_heston[[y]][[14]][[14]], aes(x = time.period, y = delta* s +  p),
             colour = 'black')

pl[[1]][[14]]



#####################################
# stock price path
#####################################
S <- map(U_heston[[2]][[3]], ~ data.frame(stock = .x$s, time = .x$time.period))

setwd("c:/Users/ATE/thesisDoc")
tikzDevice::tikz(file = "figures/analysis.hsv.stocks.tex", width = 4, height = 2)
ggplot2::ggplot(dplyr::bind_rows(S, .id = "uniqueID"), 
                ggplot2::aes(x= time, 
                             y = stock, 
                             group = uniqueID)) + 
  ggplot2::geom_line(ggplot2::aes(alpha = 0.5)) + 
  theme(legend.position = 'none') +
  ggplot2::labs( x = 'Time period',
                 y = 'Stock price')
dev.off()
setwd("c:/Users/ATE/thesisDoc/data")









