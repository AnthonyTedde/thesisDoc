library(StockPriceSimulator)
library(ggplot2)
s <- sstock()

ggplot(s, aes(x = time_periods, y = stock_price_path)) + 
  geom_line() 

c <- BSM()

ggplot(c, aes(x = time_periods, y = option_price_path)) + 
  geom_line(colour = 'steelblue') +
  geom_line(data = s, aes(x = time_periods, y = stock_price_path),
            colour = 'darkred') +
  labs(x = 'Time period', y = 'price')


