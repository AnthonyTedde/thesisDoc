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
load(file = "u_merton.RData")
load(file = "DOMAIN.RData")

#################################
# Compute the profit and loss
#################################

pi <- map(U_merton, function(u){ 
  purrr::map(u, function(x){
    purrr::map_dbl(x,
                   ~ dplyr::last(.x$delta) * dplyr::last(.x$s)
                   + dplyr::last(.x$p)
                   - dplyr::last(.x$option))
  })
})

pi_bsm <- map(U_merton, function(u){ 
  purrr::map(u, function(x){
    purrr::map_dbl(x,
                   ~ dplyr::last(.x$delta.bsm) * dplyr::last(.x$s)
                   + dplyr::last(.x$p.bsm)
                   - dplyr::last(.x$option))
  })
})

pl <- pmap(list(U_merton, pi), function(u, pi){ 
  pmap(list(u, pi), function(x, y){
    # exp(- dplyr::first(x[[1]]$r) * dplyr::last(x[[1]]$time.period)) * y / dplyr::first(x[[1]]$option)
    y / dplyr::first(x[[1]]$option)
  })
})

pl_bsm <- pmap(list(U_merton, pi_bsm), function(u, pi_bsm){ 
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
    rep(c('$\\Delta_{mrt}$', '$\\Delta_{bsm}$'), 3)),
  xt)
print(xtable::xtable(xt,
                     align = "lllllllll",  # align and put a vertical line (first "l" again represents column of row numbers)
                     caption = "Hedging with MJD: Relative P&L", 
                     label = "t:analysis:merton:pl"),
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



#####################################
# stock price path
#####################################
S <- map(U_merton[[2]][[3]], ~ data.frame(stock = .x$s, time = .x$time.period))

setwd("c:/Users/ATE/thesisDoc")
tikzDevice::tikz(file = "figures/analysis.mjd.stocks.tex", width = 4, height = 2)
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



#####################################
# P&L distrib
#####################################
#####################################
# K140
#####################################
setwd("c:/Users/ATE/thesisDoc")
xx <- 1
yy <- 2
zz <- 3
k <- 140
tikzDevice::tikz(file = paste0("figures/p.analysis.merton.pl.dist.",k,".tex"), width = 6, height = 2)



p1 <- ggplot(data.frame(pl = pl[[1]][[xx]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) + 
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  # xlim(-0.0023, .025) +
  scale_x_continuous(lim = c(-0.0023, .025), breaks=c(0,0.01, .02)) +
  labs( title = paste0("(A",k,")"),
    x = 'Relative profit and loss',
       y = 'Density')

#####################################################

p2 <- ggplot(data.frame(pl = pl[[1]][[yy]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
theme(legend.position = 'none', 
      axis.title = element_text(size = rel(0.8)),
      axis.text = element_text(size = rel(0.7)),
      plot.title = element_text(size = rel(0.8))) +
  scale_x_continuous(lim = c(-0.0023, .025), breaks=c(0,0.01, .02)) +
  labs(title =  paste0("(B",k,")"),
    x = 'Relative profit and loss',
       y = 'Density')

#####################################################

p3 <- ggplot(data.frame(pl = pl[[1]][[zz]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
theme(legend.position = 'none', 
      axis.title = element_text(size = rel(0.8)),
      axis.text = element_text(size = rel(0.7)),
      plot.title = element_text(size = rel(0.8))) +
  scale_x_continuous(lim = c(-0.0023, .025), breaks=c(0,0.01, .02)) +
  labs(title =  paste0("(C",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')

#####################################################

gridExtra::grid.arrange(p1, p2, p3, ncol = 3)

dev.off()
setwd("c:/Users/ATE/thesisDoc/data")





################################################################################
#####################################
# K160
#####################################
setwd("c:/Users/ATE/thesisDoc")
xx <- 4
yy <- 5
zz <- 6
k <- 160
tikzDevice::tikz(file = paste0("figures/p.analysis.merton.pl.dist.",k,".tex"), width = 6, height = 2)



p1 <- ggplot(data.frame(pl = pl[[1]][[xx]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) + 
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-0.03, .06) +
  # scale_x_continuous(lim = c(-0.0023, .01), breaks=c(0,0.005,0.01)) +
  labs( title = paste0("(A",k,")"),
        x = 'Relative profit and loss',
        y = 'Density')

#####################################################

p2 <- ggplot(data.frame(pl = pl[[1]][[yy]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-0.03, .06) +
  labs(title =  paste0("(B",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')

#####################################################

p3 <- ggplot(data.frame(pl = pl[[1]][[zz]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-0.03, .06) +
  labs(title =  paste0("(C",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')

#####################################################

gridExtra::grid.arrange(p1, p2, p3, ncol = 3)

dev.off()
setwd("c:/Users/ATE/thesisDoc/data")







################################################################################
#####################################
# K186
#####################################
setwd("c:/Users/ATE/thesisDoc")
xx <- 7
yy <- 8
zz <- 9
k <- 186
tikzDevice::tikz(file = paste0("figures/p.analysis.merton.pl.dist.",k,".tex"), width = 6, height = 2)



p1 <- ggplot(data.frame(pl = pl[[1]][[xx]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) + 
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-0.3, .4) +
  # scale_x_continuous(lim = c(-0.0023, .01), breaks=c(0,0.005,0.01)) +
  labs( title = paste0("(A",k,")"),
        x = 'Relative profit and loss',
        y = 'Density')

#####################################################

p2 <- ggplot(data.frame(pl = pl[[1]][[yy]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-0.3, .4) +
  labs(title =  paste0("(B",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')

#####################################################

p3 <- ggplot(data.frame(pl = pl[[1]][[zz]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-0.3, .4) +
  labs(title =  paste0("(C",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')

#####################################################

gridExtra::grid.arrange(p1, p2, p3, ncol = 3)

dev.off()
setwd("c:/Users/ATE/thesisDoc/data")








################################################################################
#####################################
# K200
#####################################
setwd("c:/Users/ATE/thesisDoc")
xx <- 10
yy <- 11
zz <- 12
k <- 200
tikzDevice::tikz(file = paste0("figures/p.analysis.merton.pl.dist.",k,".tex"), width = 6, height = 2)



p1 <- ggplot(data.frame(pl = pl[[1]][[xx]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) + 
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-1,  1.5) +
  # scale_x_continuous(lim = c(-0.0023, .01), breaks=c(0,0.005,0.01)) +
  labs( title = paste0("(A",k,")"),
        x = 'Relative profit and loss',
        y = 'Density')

#####################################################

p2 <- ggplot(data.frame(pl = pl[[1]][[yy]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-1,  1.5) +
  labs(title =  paste0("(B",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')

#####################################################

p3 <- ggplot(data.frame(pl = pl[[1]][[zz]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-1,  1.5) +
  labs(title =  paste0("(C",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')

#####################################################

gridExtra::grid.arrange(p1, p2, p3, ncol = 3)

dev.off()
setwd("c:/Users/ATE/thesisDoc/data")










################################################################################
#####################################
# K230
#####################################
setwd("c:/Users/ATE/thesisDoc")
xx <- 13
yy <- 14
zz <- 15
k <- 230
tikzDevice::tikz(file = paste0("figures/p.analysis.merton.pl.dist.",k,".tex"), width = 6, height = 2)



p1 <- ggplot(data.frame(pl = pl[[1]][[xx]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) + 
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-5.51878,  5.50962) +
  # scale_x_continuous(lim = c(-0.0023, .01), breaks=c(0,0.005,0.01)) +
  labs( title = paste0("(A",k,")"),
        x = 'Relative profit and loss',
        y = 'Density')

#####################################################

p2 <- ggplot(data.frame(pl = pl[[1]][[yy]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-5.51878,  5.50962) +
  labs(title =  paste0("(B",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')

#####################################################

p3 <- ggplot(data.frame(pl = pl[[1]][[zz]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-5.51878,  5.50962) +
  labs(title =  paste0("(C",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')

#####################################################

gridExtra::grid.arrange(p1, p2, p3, ncol = 3)

dev.off()
setwd("c:/Users/ATE/thesisDoc/data")
































#####################################
# P&L distrib
#####################################
#####################################
# K140
#####################################
setwd("c:/Users/ATE/thesisDoc")
xx <- 1
yy <- 2
zz <- 3
k <- 140
tikzDevice::tikz(file = paste0("figures/p.analysis.merton.pl.dist.big.tex"), width = 6, height = 2)



p11 <- ggplot(data.frame(pl = pl[[1]][[xx]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) + 
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  # xlim(-0.0023, .025) +
  scale_x_continuous(lim = c(-0.0023, .025), breaks=c(0,0.01, .02)) +
  labs( title = paste0("(A",k,")"),
    x = 'Relative profit and loss',
       y = 'Density')

#####################################################

p12 <- ggplot(data.frame(pl = pl[[1]][[yy]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
theme(legend.position = 'none', 
      axis.title = element_text(size = rel(0.8)),
      axis.text = element_text(size = rel(0.7)),
      plot.title = element_text(size = rel(0.8))) +
  scale_x_continuous(lim = c(-0.0023, .025), breaks=c(0,0.01, .02)) +
  labs(title =  paste0("(B",k,")"),
    x = 'Relative profit and loss',
       y = 'Density')

#####################################################

p13 <- ggplot(data.frame(pl = pl[[1]][[zz]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
theme(legend.position = 'none', 
      axis.title = element_text(size = rel(0.8)),
      axis.text = element_text(size = rel(0.7)),
      plot.title = element_text(size = rel(0.8))) +
  scale_x_continuous(lim = c(-0.0023, .025), breaks=c(0,0.01, .02)) +
  labs(title =  paste0("(C",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')

#####################################################






################################################################################
#####################################
# K160
#####################################

xx <- 4
yy <- 5
zz <- 6
k <- 160



p21 <- ggplot(data.frame(pl = pl[[1]][[xx]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) + 
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-0.03, .06) +
  # scale_x_continuous(lim = c(-0.0023, .01), breaks=c(0,0.005,0.01)) +
  labs( title = paste0("(A",k,")"),
        x = 'Relative profit and loss',
        y = 'Density')

#####################################################

p22 <- ggplot(data.frame(pl = pl[[1]][[yy]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-0.03, .06) +
  labs(title =  paste0("(B",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')

#####################################################

p23 <- ggplot(data.frame(pl = pl[[1]][[zz]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-0.03, .06) +
  labs(title =  paste0("(C",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')

#####################################################









################################################################################
#####################################
# K186
#####################################

xx <- 7
yy <- 8
zz <- 9
k <- 186



p31 <- ggplot(data.frame(pl = pl[[1]][[xx]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) + 
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-0.3, .4) +
  # scale_x_continuous(lim = c(-0.0023, .01), breaks=c(0,0.005,0.01)) +
  labs( title = paste0("(A",k,")"),
        x = 'Relative profit and loss',
        y = 'Density')

#####################################################

p32 <- ggplot(data.frame(pl = pl[[1]][[yy]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-0.3, .4) +
  labs(title =  paste0("(B",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')

#####################################################

p33 <- ggplot(data.frame(pl = pl[[1]][[zz]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-0.3, .4) +
  labs(title =  paste0("(C",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')

#####################################################










################################################################################
#####################################
# K200
#####################################

xx <- 10
yy <- 11
zz <- 12
k <- 200



p41 <- ggplot(data.frame(pl = pl[[1]][[xx]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) + 
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-1,  1.5) +
  # scale_x_continuous(lim = c(-0.0023, .01), breaks=c(0,0.005,0.01)) +
  labs( title = paste0("(A",k,")"),
        x = 'Relative profit and loss',
        y = 'Density')

#####################################################

p42 <- ggplot(data.frame(pl = pl[[1]][[yy]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-1,  1.5) +
  labs(title =  paste0("(B",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')

#####################################################

p43 <- ggplot(data.frame(pl = pl[[1]][[zz]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-1,  1.5) +
  labs(title =  paste0("(C",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')












################################################################################
#####################################
# K230
#####################################

xx <- 13
yy <- 14
zz <- 15
k <- 230



p51 <- ggplot(data.frame(pl = pl[[1]][[xx]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) + 
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-5.51878,  5.50962) +
  # scale_x_continuous(lim = c(-0.0023, .01), breaks=c(0,0.005,0.01)) +
  labs( title = paste0("(A",k,")"),
        x = 'Relative profit and loss',
        y = 'Density')

#####################################################

p52 <- ggplot(data.frame(pl = pl[[1]][[yy]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-5.51878,  5.50962) +
  labs(title =  paste0("(B",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')

#####################################################

p53 <- ggplot(data.frame(pl = pl[[1]][[zz]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-5.51878,  5.50962) +
  labs(title =  paste0("(C",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')

#####################################################

gridExtra::grid.arrange(p11, p12, p13, 
                        p21, p22, p23, 
                        p31, p32, p33, 
                        p41, p42, p43, 
                        p51, p52, p53, 
                        ncol = 3)

dev.off()
setwd("c:/Users/ATE/thesisDoc/data")













#####################################
# P&L distrib
#####################################
#####################################
# K140
#####################################
setwd("c:/Users/ATE/thesisDoc")
xx <- 1
yy <- 2
zz <- 3
k <- 140
tikzDevice::tikz(file = paste0("figures/p.analysis.merton.pl.dist.big.tex"))



p11 <- ggplot(data.frame(pl = pl[[1]][[xx]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) + 
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  # xlim(-0.0023, .025) +
  xlim(-3,  3) +
  labs( title = paste0("(A",k,")"),
    x = 'Relative profit and loss',
       y = 'Density')

#####################################################

p12 <- ggplot(data.frame(pl = pl[[1]][[yy]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
theme(legend.position = 'none', 
      axis.title = element_text(size = rel(0.8)),
      axis.text = element_text(size = rel(0.7)),
      plot.title = element_text(size = rel(0.8))) +
  xlim(-3,  3) +
  labs(title =  paste0("(B",k,")"),
    x = 'Relative profit and loss',
       y = 'Density')

#####################################################

p13 <- ggplot(data.frame(pl = pl[[1]][[zz]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
theme(legend.position = 'none', 
      axis.title = element_text(size = rel(0.8)),
      axis.text = element_text(size = rel(0.7)),
      plot.title = element_text(size = rel(0.8))) +
  xlim(-3,  3) +
  labs(title =  paste0("(C",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')

#####################################################






################################################################################
#####################################
# K160
#####################################

xx <- 4
yy <- 5
zz <- 6
k <- 160



p21 <- ggplot(data.frame(pl = pl[[1]][[xx]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) + 
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-3,  3) +
  # scale_x_continuous(lim = c(-0.0023, .01), breaks=c(0,0.005,0.01)) +
  labs( title = paste0("(A",k,")"),
        x = 'Relative profit and loss',
        y = 'Density')

#####################################################

p22 <- ggplot(data.frame(pl = pl[[1]][[yy]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-3,  3) +
  labs(title =  paste0("(B",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')

#####################################################

p23 <- ggplot(data.frame(pl = pl[[1]][[zz]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-3,  3)+
  labs(title =  paste0("(C",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')

#####################################################









################################################################################
#####################################
# K186
#####################################

xx <- 7
yy <- 8
zz <- 9
k <- 186



p31 <- ggplot(data.frame(pl = pl[[1]][[xx]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) + 
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-3,  3) +
  # scale_x_continuous(lim = c(-0.0023, .01), breaks=c(0,0.005,0.01)) +
  labs( title = paste0("(A",k,")"),
        x = 'Relative profit and loss',
        y = 'Density')

#####################################################

p32 <- ggplot(data.frame(pl = pl[[1]][[yy]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-3,  3) +
  labs(title =  paste0("(B",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')

#####################################################

p33 <- ggplot(data.frame(pl = pl[[1]][[zz]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-3,  3) +
  labs(title =  paste0("(C",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')

#####################################################










################################################################################
#####################################
# K200
#####################################

xx <- 10
yy <- 11
zz <- 12
k <- 200



p41 <- ggplot(data.frame(pl = pl[[1]][[xx]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) + 
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-3,  3) +
  # scale_x_continuous(lim = c(-0.0023, .01), breaks=c(0,0.005,0.01)) +
  labs( title = paste0("(A",k,")"),
        x = 'Relative profit and loss',
        y = 'Density')

#####################################################

p42 <- ggplot(data.frame(pl = pl[[1]][[yy]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-3,  3) +
  labs(title =  paste0("(B",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')

#####################################################

p43 <- ggplot(data.frame(pl = pl[[1]][[zz]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-3,  3)+
  labs(title =  paste0("(C",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')












################################################################################
#####################################
# K230
#####################################

xx <- 13
yy <- 14
zz <- 15
k <- 230



p51 <- ggplot(data.frame(pl = pl[[1]][[xx]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[xx]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) + 
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-3,  3) +
  # scale_x_continuous(lim = c(-0.0023, .01), breaks=c(0,0.005,0.01)) +
  labs( title = paste0("(A",k,")"),
        x = 'Relative profit and loss',
        y = 'Density')

#####################################################

p52 <- ggplot(data.frame(pl = pl[[1]][[yy]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[yy]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-3,  3) +
  labs(title =  paste0("(B",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')

#####################################################

p53 <- ggplot(data.frame(pl = pl[[1]][[zz]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[zz]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  xlim(-3,  3) +
  labs(title =  paste0("(C",k,")"),
       x = 'Relative profit and loss',
       y = 'Density')

#####################################################

gridExtra::grid.arrange(p11, p12, p13, 
                        p21, p22, p23, 
                        p31, p32, p33, 
                        p41, p42, p43, 
                        p51, p52, p53, 
                        ncol = 3)

dev.off()
setwd("c:/Users/ATE/thesisDoc/data")


#########
domain
pepe <- map(1:nrow(domain), function(x){
  list(pl[[2]][[x]], rep(paste(domain$maturity[x],domain$strike[x], sep = '-'),
                         length(pl[[1]][[x]])))
}) %>% map(function(x){
  data.frame(pl = x[[1]], pivot = x[[2]])
}) %>%
do.call(what = "rbind")  
  

setwd("c:/Users/ATE/thesisDoc")
# tikzDevice::tikz(file = "figures/appl.impliedvol.heston.tex", width = 6, height = 3)
ggplot(pepe) + 
  stat_density(aes(pl),
               # geom =  'line',
               fill = 'steelblue',
               # color = 'darkred'
               alpha = .7
  ) + 
  xlab("Relative profit and loss") + ylab("Density")+
  xlim(-3,  3)+
  # ylim(0, 5) + 
  facet_wrap( ~ pivot, ncol = 3, scales = "free_y")
dev.off()
setwd("c:/Users/ATE/thesisDoc/data")













