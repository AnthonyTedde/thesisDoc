detach("package:StockPriceSimulator", unload = T)
library(StockPriceSimulator)
library(ggplot2)
library(purrr)


h <- heston(initial_stock_price = 50,
            initial_volatility = .7,
            time_to_maturity = 1,
            seed = 1,
            scale = 8760,
            alpha = 0,
            rho = 0,
            kappa = 2,
            theta = .3)

hest <- h$stock_price_path
df <- data.frame(return = diff(hest) / hest[-length(hest)])

ggplot(df, aes(return)) + geom_histogram()

ggplot(h, aes(time_periods, stock_price_path)) + 
  geom_line()

hs <- h$stock_price_path
return <- hs[-1] / hs[-length(hs)]
logreturn <- log(return)



# Annualized:1
h <- rep(0., 1000)
for(i in 1:1000){
  h[i] <- heston(initial_stock_price = 50,
                 initial_volatility = 0.2,
                 time_to_maturity = 1,
                 seed = i,
                 scale = 365,
                 alpha = 0,
                 rho = 0,
                 kappa = 2,
                 theta = 0.2,
                 sigma = .5)$stock_price_path[366]
}
# return <- h / 50
# logreturn <- log(return)
logreturn <- (h - 50) / 50


hm05 <- rep(0., 1000)
for(i in 1:1000){
  hm05[i] <- heston(initial_stock_price = 50,
                    initial_volatility = .2,
                    time_to_maturity = 1,
                    seed = i,
                    scale = 365,
                    alpha = 0,
                    rho = -0.8,
                    kappa = 2,
                    theta = .2,
                    sigma = .5)$stock_price_path[366]
}
# returnm05 <- hm05 / 50
# logreturnm05 <- log(returnm05)
logreturnm05 <- (hm05 - 50) / 50

hp05 <- rep(0., 1000)
for(i in 1:1000){
  hp05[i] <- heston(initial_stock_price = 50,
                    initial_volatility = .2,
                    time_to_maturity = 1,
                    seed = i,
                    scale = 365,
                    alpha = 0,
                    rho = 0.8,
                    kappa = 2,
                    theta = .2,
                    sigma = .5)$stock_price_path[366]
}
# returnp05 <- hp05 / 50
# logreturnp05 <- log(returnp05)
logreturnp05 <- (hp05 - 50) / 50

r <- data.frame(logreturn,
                logreturnp05,
                logreturnm05)



ggplot(r) +
  stat_density(aes(logreturn), 
               geom = "line",
               alpha = 1, 
               colour = "seagreen4") +
  stat_density(aes(logreturnm05),
               geom = "line",
               alpha = 1,
               colour = "darkred") +
  stat_density(aes(logreturnp05),
               geom = "line",
               alpha = 1,
               colour = "steelblue")


# Mean value of RandomWalk
#OK !
bms <- RandomWalk::sbmotionGenerator(time_to_maturity = 1,
                                     scale = 365,
                                     seed = 1,
                                     n = 10000)

mean(unname(map(bms, tail, 1) %>%
  map_dbl(`[[`, 2)))



w <- RandomWalk::sbmotion(time_to_maturity = 100, scale = 365)
mean(diff(w$brownian_motion_path))



s <- rep(0., 1000)
for(i in 1:1000){
  s[i] <- sstock(initial_stock_price = 50,
                 time_to_maturity = 1,
                 seed = i,
                 scale = 365,
                 alpha = 0,
                 sigma = 0.9486833)$stock_price_path[366]
}
return <- s / 50
logreturn <- log(return)








# 
# ]
# 
# 


h <- heston(initial_stock_price = 50,
            initial_volatility = .6,
            time_to_maturity = 1,
            seed = 1,
            scale = 8760,
            alpha = 0,
            rho = 0,
            kappa = 2,
            theta = .6,
            sigma = 1)
hm <- heston(initial_stock_price = 50,
            initial_volatility = .6,
            time_to_maturity = 1,
            seed = 1,
            scale = 8760,
            alpha = 0,
            rho = -.5,
            kappa = 2,
            theta = .6,
            sigma = 1)
hp <- heston(initial_stock_price = 50,
            initial_volatility = .6,
            time_to_maturity = 1,
            seed = 1,
            scale = 8760,
            alpha = 0,
            rho = .5,
            kappa = 2,
            theta = .6,
            sigma = 1)

hest <- h$stock_price_path
hestm <- hm$stock_price_path
hestp <- hp$stock_price_path
df <- data.frame(return = diff(hest) / hest[-length(hest)],
                 returnm = diff(hestm) / hestm[-length(hestm)],
                 returnp = diff(hestp) / hestp[-length(hestp)])

ggplot(df) +
  stat_density(aes(return), 
               geom = 'line',
               alpha = 1, 
               colour = "seagreen4") +
  stat_density(aes(returnp),
               geom = "line",
               alpha = 1,
               colour = "darkred") +
  stat_density(aes(returnm),
               geom = "line",
               alpha = 1,
               colour = "steelblue") + 
  stat_function(fun = dnorm,
                color = "black",
                args = list(mean = 0, 
                            sd = sqrt(0.6) * sqrt(1/8760)))






















