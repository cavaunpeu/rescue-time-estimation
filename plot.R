source("http://peterhaschke.com/Code/multiplot.R")

lapply(c("dplyr", "ggplot2", "reshape2", "rethinking", "gridExtra", "latex2exp"), require, character.only = TRUE)

castPredictionsToLong <- function(predictions) {
  simulated.predictions.long <- melt( as.matrix(predictions) )
  simulated.predictions.long <- rename(simulated.predictions.long, n = Var1, productivity_level = Var2)
  return( simulated.predictions.long )
}

preparePredictionsForDonutPlot <- function(predictions) {
  simulated.predictions.summary <- castPredictionsToLong(predictions) %>%
    group_by(productivity_level) %>%
    summarise(mean = round( mean(value), 3)) %>%
    arrange(mean) %>%
    mutate(ymax = cumsum(mean)) %>%
    mutate(ymin = c(0, head(ymax, n=-1)))
  return( simulated.predictions.summary )
}

generateDonutPlot <- function(predictions) {
  "
  I'm not sure what's going on with the `geom_text` `x` and `y` parameters; in solution, I've left them as they are
  in the boilerplate code (http://www.r-graph-gallery.com/128-ring-or-donut-plot/) and added `axis.title.x = element_blank()`
  and `axis.title.y = element_blank()` instead.
  "
  predictions.donut.plot <- preparePredictionsForDonutPlot(predictions)
  donut.plot <- ggplot(predictions.donut.plot, aes(fill=productivity_level, ymin = ymin, ymax = ymax, xmax = 4, xmin = 3)) +
    geom_rect(colour="grey30") +
    coord_polar(theta="y") +
    geom_text(aes(label = paste(mean*100, "%", sep=""), x = 3.5, y = (ymin+ymax)/2), show.legend = FALSE) +
    xlim(c(0, 4)) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(size = 18)
    ) +
    labs(title="Expected Productivity-Level Proportions (MAP)") +
    theme(plot.title = element_text(hjust = 0.5))
  return( donut.plot )
}

generateFacetedHistogram <- function(predictions) {
  predictions.long <- melt(predictions)
  ggplot(predictions.long, aes(x = value, fill = variable)) +
    geom_histogram(binwidth = .003, colour = "black") +
    facet_wrap(~variable, ncol = 1) +
    labs(title = TeX("Posterior Distributions of Productivity-Level Proportions ($\\mu_i$)"), x = TeX('$\\mu_i$')) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = seq(0, .65, .05))
}

generatePlot <- function(predictions) {
  donut.plot <- generateDonutPlot(predictions)
  faceted.histogram <- generateFacetedHistogram(predictions)
  grid.arrange(donut.plot, faceted.histogram, nrow = 2, ncol = 1)
}
