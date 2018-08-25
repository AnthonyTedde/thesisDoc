library(StockPriceSimulator)
library(ggplot2)
library(purrr)
setwd("c:/Users/ATE/thesisDoc/data")
################################################################################
# Only one path
################################################################################
S <- sstock_jump(initial_stock_price = 50,
                 time_to_maturity = 1,
                 seed = 1,
                 scale = 365,
                 sigma = 0.2,
                 alpha = .5,
                 lambda = 2,
                 jumps_intensity_parameters = list(
                   mean = 0.05,
                   sd = 0.1
                 ))

tikzDevice::tikz(file = "figures/merton.path.tex", width = 6, height = 3) 
ggplot(S, aes(x = time, y = stock_price_path)) +
  geom_line(aes(group = grp)) +
  xlab("Time periods") + ylab("Stock price")
dev.off()


setwd("c:/Users/ATE/thesisDoc")
tikzDevice::tikz(file = "figures/merton.path2.tex", width = 4, height = 1.7) 
ggplot(S, aes(x = time, y = stock_price_path)) +
  geom_line(aes(group = grp)) +
  xlab("Time periods") + ylab("Stock price")
dev.off()