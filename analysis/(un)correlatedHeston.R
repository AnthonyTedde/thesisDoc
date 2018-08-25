detach("package:StockPriceSimulator", unload = T)
library(StockPriceSimulator)
library(ggplot2)
library(purrr)
library(dplyr)
library(grid)
library(gridExtra)
library(purrr)

# Anti-correlation
args <- c(50, 0.2, 5, 1, 365, 0, -1, 0.5, 0.2, 0.1)
names(args) <- names(formals(heston))
args_neg_cor <- as.list(args)

h <- do.call(what = heston, args_neg_cor)

pB1 <- ggplot(h) +
  geom_line(aes(time_periods, B1),
            color = "steelblue") +
  labs(x = "Time period",
       y = "Brownian Motion B1")
pB2 <- ggplot(h) +
  geom_line(aes(time_periods, B2),
            color = "darkred") +
  labs(x = "Time period",
       y = "Brownian Motion B2")
pS <- ggplot(h) +
  geom_line(aes(time_periods, stock_price_path),
            color = "steelblue") +
  labs(x = "Time period",
       y = "Stock Price Path")
pV <- ggplot(h) +
  geom_line(aes(time_periods, CIR),
            color = "darkred") +
  labs(x = "Time period",
       y = "Volatility")

tikzDevice::tikz(file = "figures/UncorrelatHeston.tex", width = 6, height = 4) 
grid.arrange(pS, pB1, pV, pB2)
dev.off()


# full correlation
args <- c(50, 0.2, 5, 1, 365, 0, 1, 0.5, 0.2, 0.1)
names(args) <- names(formals(heston))
args_neg_cor <- as.list(args)

h <- do.call(what = heston, args_neg_cor)

pB1 <- ggplot(h) +
  geom_line(aes(time_periods, B1),
            color = "steelblue") +
  labs(x = "Time period",
       y = "Brownian Motion B1")
pB2 <- ggplot(h) +
  geom_line(aes(time_periods, B2),
            color = "darkred") +
  labs(x = "Time period",
       y = "Brownian Motion B2")
pS <- ggplot(h) +
  geom_line(aes(time_periods, stock_price_path),
            color = "steelblue") +
  labs(x = "Time period",
       y = "Stock Price Path")
pV <- ggplot(h) +
  geom_line(aes(time_periods, CIR),
            color = "darkred") +
  labs(x = "Time period",
       y = "Volatility")

tikzDevice::tikz(file = "figures/CorrelatHeston.tex", width = 6, height = 4) 
grid.arrange(pS, pB1, pV, pB2)
dev.off()




################################################################################
# Denstity
################################################################################

# Identical param
sigma <-  0.1
alpha <-  0
kappa <-  2
theta <-  v <-  0.01
Ti <- 1
scale <- 10000

# negative-correlation
args<- list("NegCorrelation" = list(50, v, Ti, 1, scale, alpha, -.5, kappa, theta, sigma),
            "Uncorrelated" = list(50, v, Ti, 1, scale, alpha, 0, kappa, theta, sigma),
            "PosCorrelation" = list(50,v , Ti, 1, scale, alpha, .5, kappa, theta, sigma))

args <- purrr::map(args, structure, "names" = names(formals(heston)))

heston_list <- purrr::map(args, do.call, what = heston)

return_list <- purrr::map(heston_list, .f = function(x){
  price <- x$stock_price_path
  diff <- diff(price)
  diff / price[-length(price)]
  log(price[-1] / price[-length(price)]) 
})


# find mean of CIR uncorrelated
EV <- exp(-kappa * Ti) * v + theta * (1 - exp(-kappa * Ti))
mean(heston_list$Uncorrelated$CIR)
mean(heston_list$NegCorrelation$CIR)
mean(heston_list$PosCorrelation$CIR)

ggplot(as.data.frame(return_list)) + 
  stat_density(aes(NegCorrelation),
               geom = "line",
               colour = "darkred") + 
  stat_density(aes(Uncorrelated),
               geom = "line",
               colour = "steelblue") + 
  stat_density(aes(PosCorrelation),
               geom = "line",
               colour = "seagreen4") +
  stat_function(fun = dnorm,
                colour = "black",
                args = list(
                  mean = 0,
                  sd = sqrt(1 / scale) * sqrt(v)
                ))


# ################################################################################
# ################################################################################
# # Spot return

# Identical param
sigma <-  0.1
alpha <-  0
kappa <-  2
theta <-  v <-  0.01
Ti <- 1
scale <- 10000

# negative-correlation
hm <- rep(0., 10000)
for(i in 1:10000){
  hm[i] <- heston(initial_stock_price = 50,
                  initial_volatility = v,
                  time_to_maturity = 1,
                  seed = i,
                  scale = 500,
                  rho = -.5,
                  kappa = 2,
                  theta = theta,
                  sigma = sigma)$stock_price_path[501]
}

returnm <- log(hm / 50)


hp <- rep(0., 10000)
for(i in 1:10000){
  hp[i] <- heston(initial_stock_price = 50,
                  initial_volatility = v,
                  time_to_maturity = 1,
                  seed = i,
                  scale = 500,
                  rho = .5,
                  kappa = 2,
                  theta = theta,
                  sigma = sigma)$stock_price_path[501]
}

returnp <- log(hp / 50) 

h <- rep(0., 10000)
for(i in 1:10000){
  h[i] <- heston(initial_stock_price = 50,
                 initial_volatility = v,
                 time_to_maturity = 1,
                 seed = i,
                 scale = 500,
                 rho = 0,
                 kappa = 2,
                 theta = theta,
                 sigma = sigma)$stock_price_path[501]
}

return <- log(h / 50) 

tikzDevice::tikz(file = "figures/skewnessHeston.tex", width = 6, height = 3) 
ggplot(data.frame(returnm, returnp)) +
  stat_density(aes(returnm),
               geom = "line",
               colour = "darkred") +
  stat_density(aes(returnp),
               geom = "line",
               colour = "seagreen4") + 
  stat_density(aes(return),
               geom = "line",
               colour = "steelblue")+
  stat_function(fun = dnorm,
                colour = "black",
                args = list(
                  mean = 0 - sqrt(theta) ^2 / 2,
                  sd = sqrt(theta)))+
  labs(x = "log-return",
       y = "Density")
dev.off()








tikzDevice::tikz(file = "figures/skewnessHeston2.tex", width = 4, height = 2) 
ggplot(data.frame(returnm, returnp)) +
  stat_density(aes(returnm),
               geom = "line",
               colour = "darkred") +
  stat_density(aes(returnp),
               geom = "line",
               colour = "seagreen4") + 
  stat_density(aes(return),
               geom = "line",
               colour = "steelblue")+
  stat_function(fun = dnorm,
                colour = "black",
                args = list(
                  mean = 0 - sqrt(theta) ^2 / 2,
                  sd = sqrt(theta)))+
  labs(x = "Log-return",
       y = "Density")
dev.off()






# ################################################################################
# ################################################################################
# # Spot return

# Identical param
sigma <-  0.1
alpha <-  0
kappa <-  2
theta <-  v <-  0.01
Ti <- 1
scale <- 10000

# negative-correlation
hm1 <- rep(0., 10000)
for(i in 1:10000){
  hm1[i] <- heston(initial_stock_price = 50,
                   initial_volatility = v,
                   time_to_maturity = 1,
                   seed = i,
                   scale = 500,
                   rho = 0,
                   kappa = 2,
                   theta = theta,
                   sigma = .3)$stock_price_path[501]
}

returnm <- log(hm1 / 50)


hp1 <- rep(0., 10000)
for(i in 1:10000){
  hp1[i] <- heston(initial_stock_price = 50,
                   initial_volatility = v,
                   time_to_maturity = 1,
                   seed = i,
                   scale = 500,
                   rho = .0,
                   kappa = 2,
                   theta = theta,
                   sigma = .2)$stock_price_path[501]
}

returnp <- log(hp1 / 50) 

h1 <- rep(0., 10000)
for(i in 1:10000){
  h1[i] <- heston(initial_stock_price = 50,
                  initial_volatility = v,
                  time_to_maturity = 1,
                  seed = i,
                  scale = 500,
                  rho = 0,
                  kappa = 2,
                  theta = theta,
                  sigma = 0)$stock_price_path[501]
}

return <- log(h1 / 50) 







# tikzDevice::tikz(file = "figures/density.heston.kurtosis2.tex", width = 4, height = 2) 
ggplot(data.frame(returnm, returnp, return)) +
  stat_density(aes(returnm),
               geom = "line",
               colour = "darkred") +
  stat_density(aes(returnp),
               geom = "line",
               colour = "steelblue") + 
  stat_density(aes(return),
               geom = "line",
               colour = "seagreen4")+
  stat_function(fun = dnorm,
                colour = "black",
                args = list(
                  mean = 0 - sqrt(theta) ^2 / 2,
                  sd = sqrt(theta)))+
  labs(x = "Log-return",
       y = "Density")
# dev.off()









