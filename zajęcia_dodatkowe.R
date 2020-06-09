# libraries ---------------------------------------------------------------

library(tidyverse)
library(plotly)
library(dslabs)
library(EnvStats)

# load data ---------------------------------------------------------------
data(heights)

heights <- heights %>% mutate(height = height * 0.0254)

heights %>% select(sex, height) %>% head()

# select data -------------------------------------------------------------

woman_h <- heights %>% filter(sex=="Female") %>% select(height) %>% pull()
woman_n <- length(woman_h)

man_h <- heights %>% filter(sex=="Male") %>% select(height) %>% pull() %>% sample(woman_n)

man_and_woman_h <- c(man_h, woman_h)

# plot histogram ----------------------------------------------------------
title <- "height normalized histogram"
bin_size = 0.05
hist_norm_type <- "probability density" # "value" 

histogram <- 
  plot_ly() %>%
  add_histogram(x = man_and_woman_h, 
                name = "woman&man",
                histnorm = hist_norm_type,
                xbins = list(size=bin_size),
                marker = list(color = "grey")) %>% 
  add_histogram(x = man_h, 
                name = "man", 
                histnorm = hist_norm_type,
                xbins = list(size=bin_size),
                marker = list(color = "lightblue")) %>%
  add_histogram(x = woman_h, 
                name = "woman", 
                histnorm = hist_norm_type,
                xbins = list(size=bin_size),
                marker = list(color = "pink"))


histogram_layout <- layout(
  histogram,
  title = title,
  legend = list(orientation = 'u'),
  barmode = "overlay",
  xaxis = list(
    dtick = 2*bin_size, 
    tick0 = 0.0, 
    tickmode = "linear",
    title = "height [m]",
    zeroline = FALSE
  ),
  yaxis = list(
    title = "density"
  )
)

histogram_layout # show
htmlwidgets::saveWidget(histogram_layout, paste(title, ".html", sep="")) #save


# fit kernel density ------------------------------------------------------

man_h_kernel_density <- density(man_h)
woman_h_kernel_density <- density(woman_h)
man_and_woman_h_kernel_density <- density(man_and_woman_h)


# add kernel density to histogram -----------------------------------------

title <- "height normalized histogram and kernel density"

histogram_and_kernel_density <- 
  histogram %>% 
  add_trace(x = man_h_kernel_density$x, 
            y = man_h_kernel_density$y, 
            mode = "lines", 
            fill = "tozeroy", 
            yaxis = "y2", 
            name = "man_kernel_density") %>% 
  add_trace(x = woman_h_kernel_density$x, 
            y = woman_h_kernel_density$y, 
            mode = "lines", 
            fill = "tozeroy", 
            yaxis = "y2", 
            name = "woman_kernel_density") %>% 
  add_trace(x = man_and_woman_h_kernel_density$x, 
            y = man_and_woman_h_kernel_density$y, 
            mode = "lines", 
            fill = "tozeroy", 
            yaxis = "y2", 
            name = "woman&man_kernel_density")

histogram_and_kernel_density_layout <- layout(
  histogram_and_kernel_density,
  title = title,
  legend = list(orientation = 'h'),
  barmode = "overlay",
  xaxis = list(
    dtick = 2*bin_size, 
    tick0 = 0.0, 
    tickmode = "linear",
    title = "height [m]",
    zeroline = FALSE
  ),
  yaxis = list(
    title = "density"
  ),
  yaxis2 = list(overlaying = "y", side = "right", title = "density")
)

histogram_and_kernel_density_layout
htmlwidgets::saveWidget(histogram_and_kernel_density_layout, paste(title, ".html", sep="")) #save


# estimate theoretical density --------------------------------------------

man_and_woman_h_estimated_theoretical_density_normal <- EnvStats::enorm(man_and_woman_h)
man_h_estimated_theoretical_density_normal <- EnvStats::enorm(man_h)
woman_h_estimated_theoretical_density_normal <- EnvStats::enorm(woman_h)

man_and_woman_h_estimated_theoretical_density_uniform <- EnvStats::eunif(man_and_woman_h)

man_and_woman_h_estimated_theoretical_density_exponential <- EnvStats::eexp(man_and_woman_h)

x = seq(1, 3, length.out = 200)

# add theoretical distributions to histogram ------------------------------
man_and_woman_h_estimated_density_normal <- dnorm(x,
                                                  mean = man_and_woman_h_estimated_theoretical_density_normal$parameters[1],
                                                  sd = man_and_woman_h_estimated_theoretical_density_normal$parameters[2])
man_h_estimated_density_normal <- dnorm(x,
                                        mean = man_h_estimated_theoretical_density_normal$parameters[1],
                                        sd = man_h_estimated_theoretical_density_normal$parameters[2])
woman_h_estimated_density_normal <- dnorm(x,
                                          mean = woman_h_estimated_theoretical_density_normal$parameters[1],
                                          sd = woman_h_estimated_theoretical_density_normal$parameters[2])



man_and_woman_h_theoretical_density_uniform <- dunif(x, 
                                                     min = man_and_woman_h_estimated_theoretical_density_uniform$parameters[1], 
                                                     max = man_and_woman_h_estimated_theoretical_density_uniform$parameters[2])

man_and_woman_h_theoretical_density_exponential <- dexp(x, 
                                                        rate = man_and_woman_h_estimated_theoretical_density_exponential$parameters[1])


title <- "height normalized histogram and theoretical density"

histogram_and_theoretical_density <- 
  histogram %>% 
  add_trace(x = x, 
            y = man_and_woman_h_theoretical_density_uniform, 
            mode = "lines", 
            fill = "tozeroy",
            name = "man_and_woman_h_theoretical_density_uniform") %>% 
  add_trace(x = x, 
            y = man_and_woman_h_estimated_density_normal, 
            mode = "lines", 
            fill = "tozeroy", 
            name = "man_and_woman_h_estimated_density_normal") %>% 
  add_trace(x = x, 
            y = man_and_woman_h_theoretical_density_exponential, 
            mode = "lines", 
            fill = "tozeroy",  
            name = "man_and_woman_h_theoretical_density_exponential") %>% 
  add_trace(x = x, 
            y = man_h_estimated_density_normal, 
            mode = "lines", 
            fill = "tozeroy",  
            name = "man_h_estimated_density_normal") %>% 
  add_trace(x = x, 
            y = woman_h_estimated_density_normal, 
            mode = "lines", 
            fill = "tozeroy",  
            name = "woman_h_estimated_density_normal")

histogram_and_theoretical_density_layout <- layout(
  histogram_and_theoretical_density,
  title = title,
  barmode = "overlay",
  xaxis = list(
    dtick = 2*bin_size, 
    tick0 = 0.0, 
    tickmode = "linear",
    title = "height [m]",
    zeroline = FALSE,
    range = c(0.9,3.2)
  ),
  yaxis = list(
    title = "density"
  )
)

histogram_and_theoretical_density_layout
htmlwidgets::saveWidget(histogram_and_theoretical_density_layout, paste(title, ".html", sep="")) #save


