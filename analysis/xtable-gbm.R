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
load(file = "u_bsm.RData")
load(file = "GBM.RData")
load(file = "DOMAIN.RData")

#################################
# Compute the profit and loss
#################################

pi <- map(U_bsm, function(u){ 
  purrr::map(u, function(x){
    purrr::map_dbl(x,
                   ~ dplyr::last(.x$delta) * dplyr::last(.x$s)
                   + dplyr::last(.x$p)
                   - dplyr::last(.x$option))
  })
})



pl <- pmap(list(U_bsm, pi), function(u, pi){ 
  pmap(list(u, pi), function(x, y){
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

xt <- l %>% as.data.frame
colnames(xt) <- 1:3

xt <- data.frame(strike = as.integer(domain$strike), 
                 frequency = rep(c('intraday', 'daily', 'weekly'), 
                                 length(unique(domain$strike))),
                 xt,
                 stringsAsFactors = F) 
xt <- rbind(c(rep('', 2), map_chr(c(91, 182,399)
                                  , paste, 'dbe')),
            c(rep('', 2),
              rep(c('dddd'), 3)),
            xt)
print(xtable::xtable(xt,
                     align = "llllll",  # align and put a vertical line (first "l" again represents column of row numbers)
                     caption = "Hedging with BSM: Relative P&L", 
                     label = "t:analysis:bsm:pl"),
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
ggplot(U_bsm[[x]][[13]][[94]]) +
  geom_line(aes(x = time.period, y = option),
            colour = 'blue')+
  geom_point(aes(x = time.period, y = delta* s +  p),
             colour = ' red')+
  geom_point(data = U_bsm[[y]][[13]][[64]],
             aes(x = time.period, y = delta* s +  p),
             colour = 'black') +
  geom_point(data = U_bsm[[2]][[13]][[64]], 
             aes(x = time.period, y = delta* s +  p),
             colour = 'green')

pl[[3]][[13]]
U_bsm[[3]][[13]][[94]]






data.frame(
  gammaa = round(U_bsm[[1]][[13]][[64]]$gamma , 4),
  thetaa = round(U_bsm[[1]][[13]][[64]]$theta, 4),
  option = U_bsm[[1]][[13]][[64]]$option,
  oo = U_bsm[[1]][[13]][[64]]$theta / U_bsm[[1]][[13]][[64]]$r+ 
    .5 * GBM['sd']^2 * U_bsm[[1]][[13]][[64]]$s^2 * U_bsm[[1]][[13]][[64]]$gamma / U_bsm[[1]][[13]][[64]]$r +
    U_bsm[[1]][[13]][[64]]$delta * U_bsm[[1]][[13]][[64]]$s
)

mean(U_bsm[[1]][[13]][[64]]$gamma, na.rm = T)
max(U_bsm[[1]][[13]][[64]]$gamma, na.rm = T)


#####################################
# gamma distrib
#####################################

gamma_230_91_w <- map(U_bsm[[1]][[13]], ~.x$gamma[-length(.x$gamma)]) %>% 
  unlist
gamma_230_91_d <- map(U_bsm[[2]][[13]], ~.x$gamma[-length(.x$gamma)]) %>% 
  unlist
gamma_140_91_w <- map(U_bsm[[1]][[1]], ~.x$gamma[-length(.x$gamma)]) %>% 
  unlist
df <- data.frame(gamma_230_91_w,
                 gamma_230_91_d,
                 gamma_140_91_w)

ggplot(df) +
  stat_density(aes(gamma_230_91_w),
               # geom =  'line',
               fill = 'steelblue',
               alpha = .7
               ) +
  stat_density(aes(gamma_140_91_w),
               # geom =  'line',
               fill = 'darkred',
               alpha = .5
               ) +
  xlim(0, 0.04) + ylim(0, 300)

> U_bsm[[3]][[13]][[3]]$theta
[1] -1.8250165078 -2.3577427091 -3.3332632679 -6.1008332345 -1.8718114089 -1.9243347535
[7] -2.6796883916 -1.2605775463 -0.2300142793 -0.7318687969 -0.1206997202 -0.0365084345
[13] -0.0002176138           NaN
> U_bsm[[3]][[13]][[94]]$theta


#####################################
# theta distrib
#####################################

theta_230_91_w <- map(U_bsm[[3]][[13]], ~.x$theta[-length(.x$theta)]) %>% 
  unlist
# theta_230_91_d <- map(U_bsm[[2]][[13]], ~.x$theta[-length(.x$theta)]) %>% 
#   unlist
theta_140_91_w <- map(U_bsm[[3]][[14]], ~.x$theta[-length(.x$theta)]) %>% 
  unlist
df1 <- data.frame(theta_230_91_w)
                 # theta_230_91_d,
df2 <- data.frame(theta_140_91_w)

ggplot(df1) +
  stat_density(aes(theta_230_91_w),
               # geom =  'line',
               fill = 'steelblue',
               alpha = .7
  ) +
  stat_density(data = df2, aes(theta_140_91_w),
               # geom =  'line',
               fill = 'darkred',
               alpha = .5
  ) +
  xlim(-3, 0.2) + ylim(0, 3.5)

#####################################
# theta / gamma distrib
#####################################

theta_230_91_w <- map(U_bsm[[1]][[13]], ~.x$theta[-length(.x$theta)] / 
                        .x$gamma[-length(.x$gamma)]) %>% 
  unlist
theta_230_91_d <- map(U_bsm[[2]][[13]], ~.x$theta[-length(.x$theta)]/ 
                        .x$gamma[-length(.x$gamma)]) %>% 
  unlist
theta_140_91_w <- map(U_bsm[[1]][[1]], ~.x$theta[-length(.x$theta)]/ 
                        .x$gamma[-length(.x$gamma)]) %>%  
  unlist
theta_140_91_w <- theta_140_91_w[theta_140_91_w != -Inf]
theta_140_91_w  <- theta_140_91_w[theta_140_91_w > - 10e7]

df <- data.frame(theta_230_91_w = abs(theta_230_91_w[(1:10057)]),
                 theta_140_91_w = abs(theta_140_91_w))

ggplot(df) +
  stat_density(aes(theta_230_91_w),
               # geom =  'line',
               fill = 'steelblue',
               alpha = .7
  ) +
  stat_density(aes(theta_140_91_w),
               # geom =  'line',
               fill = 'darkred',
               alpha = .5
  ) +
  xlim(400, 2000) + ylim(0, 0.0055)






pl[[1]][[14]]


names(U_bsm[[1]][[3]][[1]])



S <- map(U_bsm[[2]][[3]], ~ data.frame(stock = .x$s, time = .x$time.period))[1:50]

setwd("c:/Users/ATE/thesisDoc")
tikzDevice::tikz(file = "figures/analysis.bsm.stocks.tex", width = 4, height = 2)
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
setwd("c:/Users/ATE/thesisDoc")
tikzDevice::tikz(file = "figures/p.analysis.gbm.pl.better.tex", width = 4, height = 2)
ggplot(data.frame(pl = pl[[1]][[13]])) +
  stat_density(aes(pl),
               geom =  'line',
               # fill = 'seagreen4',
               color = 'seagreen4'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[2]][[13]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'steelblue',
               color = 'steelblue'
               # alpha = .7
  ) +
  stat_density(data = data.frame(pl2 = pl[[3]][[13]]) 
               ,aes(pl2),
               geom =  'line',
               # fill = 'darkred',
               color = 'darkred'
               # alpha = .7
  ) + xlim(-1, 3) +
  labs(x = 'Relative profit and loss',
       y = 'Density')
dev.off()
setwd("c:/Users/ATE/thesisDoc/data")



#####################################
# p:analysis:gbm:hedges
#####################################


setwd("c:/Users/ATE/thesisDoc")
tikzDevice::tikz(file = "figures/p.analysis.gbm.hedges.tex", width = 4, height = 2)

strouk <- 7

ggplot(U_bsm[[1]][[strouk]][[1]]) +
  geom_line(aes(x = time.period, y = option),
            colour = 'chocolate4')+
  geom_point(aes(x = time.period, y = delta* s +  p),
             colour = 'seagreen4')+
  geom_point(data = U_bsm[[2]][[strouk]][[1]],
             aes(x = time.period, y = delta* s +  p),
             colour = 'steelblue') +
  geom_point(data = U_bsm[[3]][[strouk]][[1]], 
             aes(x = time.period, y = delta* s +  p),
             colour = 'darkred')+
  labs(x = 'Time period',
       y = 'Option value')

dev.off()
setwd("c:/Users/ATE/thesisDoc/data")



#####################################
# TAB BAD PL
#####################################


print(xtable::xtable(matrix(pl[[3]][[13]], nrow = 10, byrow = T),
                     align = "lllllllllll",  # align and put a vertical line (first "l" again represents column of row numbers)
                     caption = "Worst relative P&L for BSM", 
                     label = "t:analysis:bsm:pl:worst"),
      include.rownames = FALSE,
      include.colnames = FALSE)


#######################
# theta for 60
###
print(xtable::xtable(matrix(c(round(c(U_bsm[[3]][[13]][[60]]$theta[-length(U_bsm[[3]][[13]][[60]]$theta)], Inf), 2),
                              round(U_bsm[[3]][[13]][[60]]$time.period, 2)),
                            byrow = T, nrow = 4),
                     align = "lllllllll",  # align and put a vertical line (first "l" again represents column of row numbers)
                     caption = "Worst relative P&L for BSM", 
                     label = "t:analysis:bsm:pl:worst"),
      include.rownames = FALSE,
      include.colnames = FALSE)
round(c(U_bsm[[3]][[13]][[60]]$theta[-length(U_bsm[[3]][[13]][[60]]$theta)], Inf), 2)
round(U_bsm[[3]][[13]][[60]]$time.remaining, 2)




#######################
# Its course
###
setwd("c:/Users/ATE/thesisDoc")
tikzDevice::tikz(file = "figures/p.analysis.gbm.theta.high.tex", width = 4, height = 4)

strouk <- 13
samp <- 60

p1 <- ggplot(U_bsm[[1]][[strouk]][[samp]]) +
  geom_line(aes(x = time.period, y = option),
            colour = 'chocolate4')+
  geom_point(data = U_bsm[[3]][[strouk]][[samp]], 
             aes(x = time.period, y = delta* s +  p),
             colour = 'darkred')+
  labs(x = 'Time period',
       y = 'Option value')

p2 <- ggplot(U_bsm[[1]][[strouk]][[samp]]) +
  geom_line(aes(x = time.period, y = s),
            color = 'chocolate4') +
  geom_line(aes(x = time.period, y = 230),
            color = 'steelblue')+
  geom_vline(xintercept = U_bsm[[3]][[strouk]][[samp]]$time.period, alpha = 0.5,
             linetype = "dotdash")+
  labs(x = 'Time period',
       y = 'Asset price')
  
gridExtra::grid.arrange(p1, p2)

dev.off()
setwd("c:/Users/ATE/thesisDoc/data")
















