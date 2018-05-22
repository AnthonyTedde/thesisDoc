library(StockPriceSimulator)
library(ggplot2)
library(purrr)

################################################################################
# Only one path
################################################################################
S <- sstock_jump(initial_stock_price = 50,
                 time_to_maturity = 4,
                 seed = 26,
                 scale = 365,
                 sigma = 0.2,
                 alpha = .5,
                 lambda = 1,
                 jumps_intensity_parameters = list(
                   mean = 0.5,
                   sd = 0.1
                 ))

ggplot(S, aes(x = time, y = stock_price_path)) +
  geom_line(aes(group = grp))



################################################################################
# Distribution
################################################################################
Sm05 <- rep(0, 1000)
S <- rep(0, 1000)
Sp05 <- rep(0, 1000)

# computation
for(i in 1:1000){
Sm05[i] <- sstock_jump(initial_stock_price = 50,
                 time_to_maturity = .25,
                 seed = i * pi,
                 scale = 365,
                 sigma = 0.2,
                 alpha = .03,
                 lambda = 1,
                 jumps_intensity_parameters = list(
                   mean = -0.5,
                   sd = 0.1
                 ))$stock_price_path[92]
}

for(i in 1:1000){
S[i] <- sstock_jump(initial_stock_price = 50,
                 time_to_maturity = .25,
                 seed = i * pi,
                 scale = 365,
                 sigma = 0.2,
                 alpha = .03,
                 lambda = 1,
                 jumps_intensity_parameters = list(
                   mean = 0,
                   sd = 0.1
                 ))$stock_price_path[92]
}

for(i in 1:1000){
Sp05[i] <- sstock_jump(initial_stock_price = 50,
                 time_to_maturity = .25,
                 seed = i * pi,
                 scale = 365,
                 sigma = 0.2,
                 alpha = .03,
                 lambda = 1,
                 jumps_intensity_parameters = list(
                   mean = 0.5,
                   sd = 0.1
                 ))$stock_price_path[92]
}

returnm05 <- log(Sm05 / 50)
return <- log(S / 50)
returnp05 <- log(Sp05 / 50)
df <- data.frame(returnm05, return, returnp05)

# plot
ggplot(df) + 
  stat_density(aes(x = return), 
               geom = "line", alpha = 1, colour = "seagreen4") +
 stat_density(aes(x = returnm05), 
               geom = "line", alpha = 1, colour = "steelblue") +
 stat_density(aes(x = returnp05), 
               geom = "line", alpha = 1, colour = "darkred")


return <- log(S$stock_price_path[-1] / S$stock_price_path[-length(S$stock_price_path)])

ggplot(data.frame(Return = return)) + 
  geom_histogram(aes(x = Return, 
                     y = (..density..) ),
                 bins = 100,
                 color = "steelblue", fill = 'steelblue') 

+ 
  ggplot2::stat_function(fun = dnorm,
                         color = "darkred",
                         args = list(mean = 0, 
                                     sd = 0.2 * sqrt(interval))) +
   labs( x = 'Daily Stock Return',
                     y = 'Density')