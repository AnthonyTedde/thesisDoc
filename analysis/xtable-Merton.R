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
S <- map(U_merton[[2]][[3]], ~ data.frame(stock = .x$s, time = .x$time.period))[1:50]

setwd("c:/Users/ATE/thesisDoc")
tikzDevice::tikz(file = "figures/analysis.mjd.stocks.tex", width = 4, height = 2)
ggplot2::ggplot(dplyr::bind_rows(S, .id = "uniqueID"), 
                ggplot2::aes(x= time:w, 
                             y = stock, 
                             group = uniqueID)) + 
  ggplot2::geom_line(ggplot2::aes(alpha = 0.5)) + 
  theme(legend.position = 'none') +
  ggplot2::labs( x = 'Time period',
                 y = 'Stock price')
dev.off()
setwd("c:/Users/ATE/thesisDoc/data")


#####################################
# P&L distrib BIG
#####################################
#########
domain
ppl1 <- map(1:nrow(domain), function(x){
  list(pl[[1]][[x]], rep(paste0("K = ",domain$strike[x]," - dbm = ", domain$maturity[x]),
                         length(pl[[1]][[x]])))
}) %>% map(function(x){
  data.frame(ppl1 = x[[1]], pivot = x[[2]])
}) %>%
  do.call(what = "rbind")  
ppl2 <- map(1:nrow(domain), function(x){
  list(pl[[2]][[x]], rep(paste0("K = ",domain$strike[x]," - dbm = ", domain$maturity[x]),
                         length(pl[[2]][[x]])))
}) %>% map(function(x){
  data.frame(ppl2 = x[[1]], pivot = x[[2]])
}) %>%
  do.call(what = "rbind")  
ppl3 <- map(1:nrow(domain), function(x){
  list(pl[[3]][[x]], rep(paste0("K = ",domain$strike[x]," - dbm = ", domain$maturity[x]),
                         length(pl[[3]][[x]])))
}) %>% map(function(x){
  data.frame(ppl3 = x[[1]], pivot = x[[2]])
}) %>%
  do.call(what = "rbind")  


setwd("c:/Users/ATE/thesisDoc")
tikzDevice::tikz(file = "figures/p.analysis.merton.pl.dist.big.tex", width = 6, height = 5)
ggplot(ppl1) + 
  stat_density(aes(ppl1),
               # geom =  'line',
               fill = 'seagreen4',
               # color = 'darkred'
               alpha = .7
  ) + 
  stat_density(data = ppl2, aes(ppl2),
               # geom =  'line',
               fill = 'steelblue',
               # color = 'darkred'
               alpha = .7
  ) +
  stat_density(data = ppl3, aes(ppl3),
               # geom =  'line',
               fill = 'darkred',
               # color = 'darkred'
               alpha = .7
  ) +
  xlab("Relative profit and loss") + ylab("Density")+
  xlim(-3,  3)+
  # ylim(0, 5) + 
  facet_wrap( ~ pivot, ncol = 3, scales = "free_y")
dev.off()
setwd("c:/Users/ATE/thesisDoc/data")




#####################################
# P&L distrib IN
#####################################


domain
ppl1 <- map(1:6, function(x){
  list(pl[[1]][[x]], rep(paste0("K = ",domain$strike[x]," - dbm = ", domain$maturity[x]),
                         length(pl[[1]][[x]])))
}) %>% map(function(x){
  data.frame(ppl1 = x[[1]], pivot = x[[2]])
}) %>%
  do.call(what = "rbind")  
ppl2 <- map(1:6, function(x){
  list(pl[[2]][[x]], rep(paste0("K = ",domain$strike[x]," - dbm = ", domain$maturity[x]),
                         length(pl[[2]][[x]])))
}) %>% map(function(x){
  data.frame(ppl2 = x[[1]], pivot = x[[2]])
}) %>%
  do.call(what = "rbind")  
ppl3 <- map(1:6, function(x){
  list(pl[[3]][[x]], rep(paste0("K = ",domain$strike[x]," - dbm = ", domain$maturity[x]),
                         length(pl[[3]][[x]])))
}) %>% map(function(x){
  data.frame(ppl3 = x[[1]], pivot = x[[2]])
}) %>%
  do.call(what = "rbind")  


setwd("c:/Users/ATE/thesisDoc")
tikzDevice::tikz(file = "figures/p.analysis.merton.pl.dist.in.tex", width = 6, height = 4)
ggplot(ppl1) + 
  stat_density(aes(ppl1),
               # geom =  'line',
               fill = 'seagreen4',
               # color = 'darkred'
               alpha = .7
  ) + 
  stat_density(data = ppl2, aes(ppl2),
               # geom =  'line',
               fill = 'steelblue',
               # color = 'darkred'
               alpha = .7
  ) +
  stat_density(data = ppl3, aes(ppl3),
               # geom =  'line',
               fill = 'darkred',
               # color = 'darkred'
               alpha = .7
  ) +
  xlab("Relative profit and loss") + ylab("Density")+
  xlim(-.025,  .05)+
  # ylim(0, 5) + 
  facet_wrap( ~ pivot, ncol = 3, scales = "free_y")
dev.off()
setwd("c:/Users/ATE/thesisDoc/data")

#####################################
# distrib of s
#####################################

domain
186.3100
u_91 <- map_dbl(U_merton[[3]][[1]], function(x){
  dplyr::last(x$s)
})
u_182 <- map_dbl(U_merton[[3]][[2]], function(x){
  dplyr::last(x$s)
})
u_399 <- map_dbl(U_merton[[3]][[3]], function(x){
  dplyr::last(x$s)
})
df <- data.frame(u_91, u_182, u_399)


setwd("c:/Users/ATE/thesisDoc")
# tikzDevice::tikz(file = "figures/p.analysis.merton.pl.dist.in.tex", width = 6, height = 3)
ggplot(df) + 
  stat_density(aes(u_91),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) + 
  stat_density(aes(u_182),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(aes(u_399),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) +
  xlab("asset price") + ylab("Density")
# xlim(-.025,  .05)+
# ylim(0, 5) + 
# facet_wrap( ~ pivot, ncol = 3, scales = "free_y")
dev.off()
setwd("c:/Users/ATE/thesisDoc/data")



#####################################
# Delta vs delta merton
#####################################


domain
ppl1 <- map(1:nrow(domain), function(x){
  list(pl[[1]][[x]], rep(paste0("K = ",domain$strike[x]," - dbm = ", domain$maturity[x]),
                         length(pl[[1]][[x]])))
}) %>% map(function(x){
  data.frame(ppl1 = x[[1]], pivot = x[[2]])
}) %>%
  do.call(what = "rbind")  
ppl2 <- map(1:nrow(domain), function(x){
  list(pl_bsm[[1]][[x]], rep(paste0("K = ",domain$strike[x]," - dbm = ", domain$maturity[x]),
                             length(pl_bsm[[1]][[x]])))
}) %>% map(function(x){
  data.frame(ppl2 = x[[1]], pivot = x[[2]])
}) %>%
  do.call(what = "rbind")  



setwd("c:/Users/ATE/thesisDoc")
tikzDevice::tikz(file = "figures/p.analysis.merton.pl.dist.deltas.tex", width = 6, height = 5)
ggplot(ppl1) + 
  stat_density(aes(ppl1),
               # geom =  'line',
               fill = 'seagreen4',
               # color = 'darkred'
               alpha = .7
  ) + 
  stat_density(data = ppl2, aes(ppl2),
               # geom =  'line',
               fill = 'darkred',
               # color = 'darkred'
               alpha = .7
  ) +
  xlab("Relative profit and loss") + ylab("Density")+
  xlim(-3,  3)+
  facet_wrap( ~ pivot, ncol = 3, scales = "free_y")
dev.off()
setwd("c:/Users/ATE/thesisDoc/data")






#####################################
# Delta hedging test
#####################################


domain
ppl1 <- map(seq(2, 15, by = 3), function(x){
  U_merton[[1]][[x]][c(14, 2, 56,77, 34)]
}) %>% map(function(x){
  map(x, ~.x[seq(1, nrow(.x), by = 10), ])
})



p1 <- ggplot(dplyr::bind_rows(ppl1[[1]], .id = "uniqueID")) +
  geom_line(aes(x = time.period, y = option, 
                group = uniqueID),
            colour = 'black')+ 
  geom_point(aes(x = time.period, y = delta.bsm* s +  p.bsm, 
                 group = uniqueID),
             color = 'darkred', size = .75) +
  geom_point(aes(x = time.period, y = delta* s +  p, 
                 group = uniqueID),
             colour = 'steelblue', size = .45) + 
  theme(legend.position = 'none', 
        title = element_text(size = rel(0.8))) +
  labs(title = "(K = 140, dbm = 182)",
       x = 'Time period',
       y = 'Option value')

p2 <- ggplot(dplyr::bind_rows(ppl1[[2]], .id = "uniqueID")) +
  geom_line(aes(x = time.period, y = option, 
                group = uniqueID),
            colour = 'black')+ 
  geom_point(aes(x = time.period, y = delta.bsm* s +  p.bsm, 
                 group = uniqueID),
             color = 'darkred', size = .75) +
  geom_point(aes(x = time.period, y = delta* s +  p, 
                 group = uniqueID),
             colour = 'steelblue', size = .45) + 
  theme(legend.position = 'none', 
        title = element_text(size = rel(0.8))) +
  labs(title = "(K = 160, dbm = 182)",
       x = 'Time period',
       y = 'Option value')

p3 <- ggplot(dplyr::bind_rows(ppl1[[3]], .id = "uniqueID")) +
  geom_line(aes(x = time.period, y = option, 
                group = uniqueID),
            colour = 'black')+ 
  geom_point(aes(x = time.period, y = delta.bsm* s +  p.bsm, 
                 group = uniqueID),
             color = 'darkred', size = .75) +
  geom_point(aes(x = time.period, y = delta* s +  p, 
                 group = uniqueID),
             colour = 'steelblue', size = .45) + 
  theme(legend.position = 'none', 
        title = element_text(size = rel(0.8))) +
  labs(title = "(K = 186, dbm = 182)",
       x = 'Time period',
       y = 'Option value')

p4 <- ggplot(dplyr::bind_rows(ppl1[[4]], .id = "uniqueID")) +
  geom_line(aes(x = time.period, y = option, 
                group = uniqueID),
            colour = 'black')+ 
  geom_point(aes(x = time.period, y = delta.bsm* s +  p.bsm, 
                 group = uniqueID),
             color = 'darkred', size = .75) +
  geom_point(aes(x = time.period, y = delta* s +  p, 
                 group = uniqueID),
             colour = 'steelblue', size = .45) + 
  theme(legend.position = 'none', 
        title = element_text(size = rel(0.8))) +
  labs(title = "(K = 200, dbm = 182)",
       x = 'Time period',
       y = 'Option value')

p5 <- ggplot(dplyr::bind_rows(ppl1[[5]], .id = "uniqueID")) +
  geom_line(aes(x = time.period, y = option, 
                group = uniqueID),
            colour = 'black')+ 
  geom_point(aes(x = time.period, y = delta.bsm* s +  p.bsm, 
                 group = uniqueID),
             color = 'darkred', size = .75) +
  geom_point(aes(x = time.period, y = delta* s +  p, 
                 group = uniqueID),
             colour = 'steelblue', size = .45) + 
  theme(legend.position = 'none', 
        title = element_text(size = rel(0.8))) +
  labs(title = "(K = 230, dbm = 182)",
       x = 'Time period',
       y = 'Option value')


setwd("c:/Users/ATE/thesisDoc")
tikzDevice::tikz(file = "figures/p.analysis.merton.hedge.deltas.tex", width = 4.5, height = 3.2)
grid.arrange(p1, p2, p3, p4, p5)
dev.off()
setwd("c:/Users/ATE/thesisDoc/data")





library(ggplot2)
p1 <- ggplot(U_merton[[1]][[13]][[3]]) +
  geom_line(aes(x = time.period, y = option),
            colour = 'chocolate4')+
  geom_point(aes(x = time.period, y = delta* s +  p),
             colour = ' red')
# geom_point(aes(x = time.period, y = delta.bsm* s +  p.bsm),
#            colour = 'black')





k <- 7

p1_i <- ggplot(U_merton[[1]][[13]][[k]]) +
  geom_line(aes(x = time.period, y = option),
            colour = 'chocolate4')+
  geom_point(aes(x = time.period, y = delta* s +  p),
             colour = 'darkred',
             size = .5)+
  labs(x = 'Time period',
       y = 'Option value')

p2_i <- ggplot(U_merton[[1]][[13]][[k]]) +
  geom_line(aes(x = time.period, y = s),
            color = 'chocolate4') +
  geom_line(aes(x = time.period, y = 230),
            color = 'steelblue')+
  geom_vline(xintercept = U_bsm[[3]][[13]][[k]]$time.period, alpha = 0.5,
             linetype = "dotdash")+
  labs(x = 'Time period',
       y = 'Asset price')

setwd("c:/Users/ATE/thesisDoc")
tikzDevice::tikz(file = "figures/p.analysis.merton.hedge.gamma.intra.tex", width = 4, height = 3)
gridExtra::grid.arrange(p1_i, p2_i)
dev.off()
setwd("c:/Users/ATE/thesisDoc/data")


p1_d <- ggplot(U_merton[[1]][[13]][[k]]) +
  geom_line(aes(x = time.period, y = option),
            colour = 'chocolate4')+
  geom_point(data = U_merton[[2]][[13]][[k]],
             aes(x = time.period, y = delta* s +  p),
             colour = 'darkred',
             size = .5)+
  labs(x = 'Time period',
       y = 'Option value')

p2_d <- ggplot(U_merton[[1]][[13]][[k]]) +
  geom_line(aes(x = time.period, y = s),
            color = 'chocolate4') +
  geom_line(aes(x = time.period, y = 230),
            color = 'steelblue')+
  geom_vline(xintercept = U_bsm[[3]][[13]][[k]]$time.period, alpha = 0.5,
             linetype = "dotdash")+
  labs(x = 'Time period',
       y = 'Asset price')

setwd("c:/Users/ATE/thesisDoc")
tikzDevice::tikz(file = "figures/p.analysis.merton.hedge.gamma.day.tex", width = 4, height = 3)
gridExtra::grid.arrange(p1_d, p2_d)
dev.off()
setwd("c:/Users/ATE/thesisDoc/data")




p1_w <- ggplot(U_merton[[1]][[13]][[k]]) +
  geom_line(aes(x = time.period, y = option),
            colour = 'chocolate4')+
  geom_point(data = U_merton[[3]][[13]][[k]],
             aes(x = time.period, y = delta* s +  p),
             colour = 'darkred',
             size = .5)+
  labs(x = 'Time period',
       y = 'Option value')

p2_w <- ggplot(U_merton[[1]][[13]][[k]]) +
  geom_line(aes(x = time.period, y = s),
            color = 'chocolate4') +
  geom_line(aes(x = time.period, y = 230),
            color = 'steelblue')+
  geom_vline(xintercept = U_bsm[[3]][[13]][[k]]$time.period, alpha = 0.5,
             linetype = "dotdash")+
  labs(x = 'Time period',
       y = 'Asset price')

setwd("c:/Users/ATE/thesisDoc")
tikzDevice::tikz(file = "figures/p.analysis.merton.hedge.gamma.week.tex", width = 4, height = 3)
gridExtra::grid.arrange(p1_w, p2_w)
dev.off()
setwd("c:/Users/ATE/thesisDoc/data")













