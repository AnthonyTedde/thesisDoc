library(RandomWalk)
library(StockPriceSimulator)
library(ggplot2)
library(grid)
library(gridExtra)


scale <- 10000
return <- sstock_return_ito(scale = scale, sigma = 0.2)

rand_walk_t <- trwalkGenerator(time_to_maturity = 4,
                               scale = 100)

interval <- 1/scale

p <-  ggplot(data.frame(Return = return)) + 
  stat_density(aes(x = Return),
               geom = "area",
               alpha = 0.5,
               fill = "steelblue") + 
  ggplot2::stat_function(fun = dnorm,
                         color = "black",
                         args = list(mean = 0, 
                                     sd = 0.2 * sqrt(interval))) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
   labs( x = 'Daily Stock Return',
                     y = 'Density')


tikzDevice::tikz(file = "figures/stock_return_density.tex", width = 6, height = 3) 
p
dev.off()


################################################################################
# Log---return
################################################################################
library(RandomWalk)
library(StockPriceSimulator)
library(ggplot2)
library(grid)
library(gridExtra)
library(purrr)


scale <- 500
n <- 1:5000
t <- 1
s0 <- 50
sigma <- 0.3
a <- 0 # alpha
stock <- unlist(map(n, ~sstock(initial_stock_price = s0, time_to_maturity = t,
                               sigma = sigma,
                               seed = .x, 
                               scale = scale)$stock_price_path[scale * t + 1]))
log_return <- log(stock / s0)
 

p <-  ggplot(data.frame(return = log_return)) + 
  stat_density(aes(x = return),
               geom = "area",
               alpha = 0.5,
               fill = "steelblue") + 
  ggplot2::stat_function(fun = dnorm,
                         color = "black",
                         args = list(mean = -sigma ^2 / 2, 
                                     sd = sigma * sqrt(t))) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
   labs( x = 'Log-return',
                     y = 'Density')
p

tikzDevice::tikz(file = "figures/stock_logreturn_density.tex", width = 6, height = 3)
p
dev.off()

tikzDevice::tikz(file = "figures/stock_logreturn_density2.tex", width = 4, height = 2)
p
dev.off()






scale <- 365
n <- 1:5000
t <- 1
s0 <- 115.08

a <- alpha
stock <- unlist(map(n, ~sstock(initial_stock_price = s0, time_to_maturity = t,
                               sigma = sigma,
                               seed = .x, 
                               scale = scale)$stock_price_path[scale * t + 1]))
log_return <- log(stock / s0)



p <-  ggplot(data.frame(return = log_return)) + 
  stat_density(aes(x = return),
               geom = "line",
               alpha = 0.5,
               colour = "darkred") + 
  stat_density(data = data.frame(u), aes(u),
               geom = "line",
               colour = 'steelblue')+
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  labs( x = 'Log-return',
        y = 'Density')


tikzDevice::tikz(file = "figures/realvsempirical2.tex", width = 4, height = 2)
p
dev.off()



################################################################################
# log--normal stock return return
################################################################################
delta <- s0 ^2 * exp(2 * a * t) * (exp(sigma ^2 * t) - 1)
mu <- log(50) + (a - sigma ^2 / 2) * t
p <-  ggplot(data.frame(stock = stock)) + 
  stat_density(aes(x = stock),
               geom = "area",
               alpha = 0.5,
               fill = "steelblue") + 
  ggplot2::stat_function(fun = dlnorm,
                         color = "black",
                         args = list(meanlog = mu, 
                                     sdlog = sigma)) +
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
   labs( x = 'Daily Stock Return',
                     y = 'Density')

tikzDevice::tikz(file = "figures/stock_log_density.tex", width = 6, height = 3)
p
dev.off()




