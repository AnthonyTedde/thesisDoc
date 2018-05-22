library(RandomWalk)
library(StockPriceSimulator)
library(ggplot2)
library(grid)
library(gridExtra)
library(purrr)

# Concise way to achieve

tikzDevice::tikz(file = "figures/ItoAccuracy.tex", width = 6, height = 4) 

# Quadrant's name
q <- as.list(paste0('(Q', LETTERS[1:4], ')'))
c(expand.grid(c(360, 4), c(.2, 1)) %>%
  transpose() %>%
  map(~setNames(.x, c('scale', 'sigma'))) %>% 
  mapply(q, FUN = list, SIMPLIFY = F) %>%
  map(.f = function(x){
    list(do.call(what = sstock, args = x[[1]]), 
         do.call(what = sstock_ito, args = x[[1]]),
         name = x[[2]])
  }) %>%
  map(.f = function(x){
    ggplot(x[[1]],
                    aes(time_periods, stock_price_path)) +
      geom_line(color = 'darkred') +
      geom_line(data = x[[2]],
                         aes(time_periods, stock_price_path),
                         color = 'steelblue') +
      theme(legend.position = 'none', 
            axis.title = element_text(size = rel(0.8)),
            axis.text = element_text(size = rel(0.7)),
            plot.title = element_text(size = rel(0.8))) +
      labs( title = x[[3]],
            x = 'Time period',
            y = 'Brownian motion') 
  }), ncol = 2) %>% 
  do.call(what = grid.arrange)

dev.off()


# Practical way
