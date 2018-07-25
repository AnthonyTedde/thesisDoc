library(RandomWalk)
library(ggplot2)
library(purrr)
library(grid)
library(gridExtra)
# The following line set the variable @sampled with a list of 20
# data.frame of Adapted random walk.
# s <- sbmotionGenerator(time_to_maturity = 4, scale = 1, n = 2)
Sampled_100 <- sbmotionGenerator(time_to_maturity = 5, scale = 100, n = 100)
class(Sampled_100) <- "list"

Sampled <- sbmotionGenerator(time_to_maturity = 5, scale = 100, n = 100000)
class(Sampled) <- "list"
last <- data.frame(last = unname(map(Sampled, `$`, 'brownian_motion_path') %>%
                                   map_dbl(`[`,(501))))



# tikzDevice::tikz(file = "figures/plot_test.tex", width = 6, height = 3)
p_along <- ggplot2::ggplot(dplyr::bind_rows(Sampled_100, .id = "uniqueID"), 
                           ggplot2::aes(x = time_periods, 
                                        y = brownian_motion_path, 
                                        group = uniqueID)) + 
  ggplot2::geom_line(ggplot2::aes(alpha = 0.5)) + 
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  ggplot2::labs( title = "(QA)",
                 x = 'Time period',
                 y = 'Brownian motion')
# dev.off()

p_density <- ggplot(last) +
  stat_density(aes(last),
               # geom =  'line',
               alpha = 0.5,
               fill = 'steelblue',
               colour = 'steelblue') + 
  stat_function(fun = dnorm,
                color = "black",
                args = list(mean = 0, 
                            sd = sqrt(5))) + 
  theme(legend.position = 'none', 
        axis.title = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.8))) +
  labs(title = "(QB)",
       x = 'Brownian Motion Distribution',
       y = 'Density')

setwd("c:/Users/ATE/thesisDoc")
tikzDevice::tikz(file = "figures/brownianmotion.tex", width = 6, height = 3)
grid.arrange(p_along, p_density, ncol = 2)
dev.off()
setwd("c:/Users/ATE/thesisDoc/data")
