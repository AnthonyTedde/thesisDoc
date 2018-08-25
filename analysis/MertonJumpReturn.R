library(StockPriceSimulator)
library(ggplot2)
library(purrr)
################################################################################
# Distribution
################################################################################
Sm05 <- rep(0, 5000)
S <- rep(0, 5000)
Sp05 <- rep(0, 5000)

# computation
for(i in 1:5000){
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

for(i in 1:5000){
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

for(i in 1:5000){
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

tikzDevice::tikz(file = "figures/MertonJumpReturn.tex", width = 6, height = 3) 
ggplot(df) + 
  stat_density(aes(x = return), 
               geom = "line", alpha = 1, colour = "seagreen4") +
  stat_density(aes(x = returnm05), 
               geom = "line", alpha = 1, colour = "steelblue") +
  stat_density(aes(x = returnp05), 
               geom = "line", alpha = 1, colour = "darkred") +
  stat_function(fun = dnorm,
                color = "black",
                args = list(mean = 0, 
                            sd = 0.2 * sqrt(0.25)))
dev.off()


tikzDevice::tikz(file = "figures/MertonJumpReturn2.tex", width = 4, height = 2) 
ggplot(df) + 
  stat_density(aes(x = return), 
               geom = "line", alpha = 1, colour = "seagreen4") +
  stat_density(aes(x = returnm05), 
               geom = "line", alpha = 1, colour = "steelblue") +
  stat_density(aes(x = returnp05), 
               geom = "line", alpha = 1, colour = "darkred") +
  stat_function(fun = dnorm,
                color = "black",
                args = list(mean = 0, 
                            sd = 0.2 * sqrt(0.25)))
dev.off()




#####
#
# LAMBDA
#
#
#####

################################################################################
# Distribution
################################################################################
Sm05 <- rep(0, 5000)
S <- rep(0, 5000)
Sp05 <- rep(0, 5000)

# computation
for(i in 1:5000){
  Sm05[i] <- sstock_jump(initial_stock_price = 50,
                         time_to_maturity = .25,
                         seed = i * pi,
                         scale = 365,
                         sigma = 0.2,
                         alpha = 0,
                         lambda = 1,
                         jumps_intensity_parameters = list(
                           mean = 0,
                           sd = 0.1
                         ))$stock_price_path[92]
}

for(i in 1:5000){
  S[i] <- sstock_jump(initial_stock_price = 50,
                      time_to_maturity = .25,
                      seed = i * pi,
                      scale = 365,
                      sigma = 0.2,
                      alpha = 0,
                      lambda = 3,
                      jumps_intensity_parameters = list(
                        mean = 0,
                        sd = 0.1
                      ))$stock_price_path[92]
}

for(i in 1:5000){
  Sp05[i] <- sstock_jump(initial_stock_price = 50,
                         time_to_maturity = .25,
                         seed = i * pi,
                         scale = 365,
                         sigma = 0.2,
                         alpha = 0,
                         lambda = 5,
                         jumps_intensity_parameters = list(
                           mean = 0,
                           sd = 0.1
                         ))$stock_price_path[92]
}

returnm05 <- log(Sm05 / 50)
return <- log(S / 50)
returnp05 <- log(Sp05 / 50)
df <- data.frame(returnm05, return, returnp05)

# plot   

tikzDevice::tikz(file = "figures/MertonJumpReturnTails.tex", width = 6, height = 3)
ggplot(df) + 
  stat_density(aes(x = return), 
               geom = "line", alpha = 1, colour = "seagreen4") +
  stat_density(aes(x = returnm05), 
               geom = "line", alpha = 1, colour = "steelblue") +
  stat_density(aes(x = returnp05), 
               geom = "line", alpha = 1, colour = "darkred") +
  stat_function(fun = dnorm,
                color = "black",
                args = list(mean = 0, 
                            sd = sqrt(.25) * .2))
dev.off()




tikzDevice::tikz(file = "figures/MertonJumpReturnTails2.tex", width = 4, height = 2)
ggplot(df) + 
  stat_density(aes(x = return), 
               geom = "line", alpha = 1, colour = "seagreen4") +
  stat_density(aes(x = returnm05), 
               geom = "line", alpha = 1, colour = "steelblue") +
  stat_density(aes(x = returnp05), 
               geom = "line", alpha = 1, colour = "darkred") +
  stat_function(fun = dnorm,
                color = "black",
                args = list(mean = 0, 
                            sd = sqrt(.25) * .2))
dev.off()