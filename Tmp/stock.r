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

ggplot(data.frame(Return = return)) + 
  geom_histogram(aes(x = Return, 
                     y = (..density..) ),
                 bins = 31) + 
  ggplot2::stat_function(fun = dnorm,
                         color = "blue",
                         args = list(mean = 0, 
                                     sd = 0.2 * sqrt(interval)))



