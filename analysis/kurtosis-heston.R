detach("package:StockPriceSimulator", unload = T)
library(StockPriceSimulator)
library(ggplot2)
library(purrr)
library(dplyr)
library(grid)
library(gridExtra)
library(purrr)
################################################################################
# Denstity
################################################################################

# Identical param
alpha <-  0
kappa <-  2
theta <-  v <-  0.5
Ti <- 1
scale <- 10000
rho <- 1

 # negative-correlation
args<- list("NegCorrelation" = list(50, v, Ti, 1, scale, alpha, rho, kappa, theta, .4),
            "Uncorrelated" = list(50, v, Ti, 1, scale, alpha, rho, kappa, theta, .2),
            "PosCorrelation" = list(50,v , Ti, 1, scale, alpha, rho, kappa, theta, 0))

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

p <- ggplot(as.data.frame(return_list)) + 
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

tikzDevice::tikz(file = "figures/density.heston.kurtosis.tex", width = 6, height = 3) 
print(p)
dev.off()