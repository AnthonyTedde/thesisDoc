library(RandomWalk)
library(ggplot)
library(purrr)
# The following line set the variable @sampled with a list of 20
# data.frame of Adapted random walk.
# s <- sbmotionGenerator(time_to_maturity = 4, scale = 1, n = 2)
s <- sbmotionGenerator(time_to_maturity = 5, scale = 365, n = 100)
class(s) <- "list"
categorical_sample <- bind_rows(s)
class(Sampled) <- 'list'

Sampled <- sbmotionGenerator(time_to_maturity = 5, scale = 100, n = 100)
class(Sampled) <- "list"

tikzDevice::tikz(file = "figures/plot_test.tex", width = 6, height = 3)
ggplot2::ggplot(dplyr::bind_rows(Sampled, .id = "uniqueID"), 
                ggplot2::aes(x = time_periods, 
                             y = brownian_motion_path, 
                             group = uniqueID)) + 
  ggplot2::geom_line(ggplot2::aes(alpha = 0.5)) + 
  ggplot2::theme(legend.position = 'none') +
  ggplot2::labs( x = 'Time period',
                y = 'Brownian motion')
dev.off()



Sampled <- sbmotionGenerator(time_to_maturity = 5, scale = 100, n = 100000)
class(Sampled) <- "list"
last <- data.frame(last = unname(map(Sampled, `$`, 'brownian_motion_path') %>%
                                   map_dbl(`[`,(501))))


tikzDevice::tikz(file = "figures/brownianmotiondensity.tex", width = 6, height = 3)
ggplot(last) +
  stat_density(aes(last),
               # geom =  'line',
               alpha = 0.5,
               fill = 'steelblue',
               colour = 'steelblue') + 
  stat_function(fun = dnorm,
                color = "black",
                args = list(mean = 0, 
                            sd = sqrt(5))) +
  labs(x = 'Brownian Motion Distribution',
       y = 'Density')
dev.off()

# ggplot2::ggplot() + 
#    # geom_point(alpha = 0) +
#                   geom_smooth(data = categorical_sample, aes(x = time_periods, 
#                              y = brownian_motion_path, group = time_periods))
# 
#   ggplot2::theme(legend.position = 'none') +
#   ggplot2::labs( x = 'Time period',
#                 y = 'Brownian motion')