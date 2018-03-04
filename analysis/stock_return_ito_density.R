library(RandomWalk)
library(StockPriceSimulator)
library(ggplot2)
library(grid)
library(gridExtra)


scale <- 360
return <- sstock_return_ito(scale = scale, sigma = 0.2)

rand_walk_t <- trwalkGenerator(time_to_maturity = 4,
                               scale = 100)

interval <- 1/scale

p <-  ggplot(data.frame(Return = return)) + 
  geom_histogram(aes(x = Return, 
                     y = (..density..) ),
                 bins = 31,
                 color = "steelblue", fill = 'steelblue') + 
  ggplot2::stat_function(fun = dnorm,
                         color = "darkred",
                         args = list(mean = 0, 
                                     sd = 0.2 * sqrt(interval))) +
   labs( x = 'Daily Stock Return',
                     y = 'Density')


tikzDevice::tikz(file = "figures/stock_return_density.tex", width = 6, height = 3) 
p
dev.off()