library(ggplot2)
library(StockPriceSimulator)
################################################################################
# Heston model
################################################################################ 
###############
## Arguments ##
###############

initial_stock_price = 50
initial_volatility = 0.3
time_to_maturity = 5
seed = 1
scale = 365 # Daily measuremen
alpha = 0
rho = 1
kappa = 2
theta = 0.3
sigma = .6


##############################################
## Create the correlated BM                 ##
## Todo: add function to package RandomWalk ##
##############################################

bms <- RandomWalk::sbmotionGenerator(time_to_maturity = time_to_maturity,
                                     scale = scale,
                                     seed = seed,
                                     n = 2)

W1 <- bms$'1'$brownian_motion_path
W2 <- bms$'2'$brownian_motion_path

dB1 <- diff(W1)
dB2 <- diff(rho * W1 + sqrt( 1 - rho ^2) * W2)

t <- seq(0, time_to_maturity,
         length.out = time_to_maturity * scale + 1)
dt <- diff(t)

##################################
## Create the volatility vector ##
##################################
# CIR model

# time frame
d2 <- apply(rbind(dt,dB2),2,as.pairlist)


# random_volatility: vector that takes in the volatility to be applied
# in Heston model.
v <- Reduce(
  function(acc, x){
    acc + kappa * (theta - acc) * x$dt + sigma * x$dB2 * sqrt(acc)
  },
  d2, initial_volatility, accumulate = T)



# PLOT
tikzDevice::tikz(file = "figures/CIRVolatility.tex", width = 6, height = 3) 
ggplot(data.frame(v, t), aes(t, v)) + 
  geom_line(colour = "steelblue")+
  labs(x = "Time (years)",
       y = "CIR stochastic volatility")
dev.off()