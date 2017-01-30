source("http://peterhaschke.com/Code/multiplot.R")

lapply(c("dplyr", "ggplot2", "reshape2", "rethinking", "gridExtra"), require, character.only = TRUE)

# report <- readReport("~/Downloads/RescueTime_Report_Productivity__by_week_2016-01-01.csv")
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

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
  predictions.donut.plot <- preparePredictionsForDonutPlot(predictions)
  donut.plot <- ggplot(predictions.donut.plot, aes(fill=productivity_level, ymin = ymin, ymax = ymax, xmax = 4, xmin = 3)) +
    geom_rect(colour="grey30") +
    coord_polar(theta="y") +
    geom_text(aes(label = paste(mean*100, "%", sep=""), x = 3.5, y = (ymin+ymax)/2), show.legend = FALSE) +
    xlim(c(0, 4)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.ticks = element_blank()) +
    labs(title="Customized ring plot")
  return( donut.plot )
}

generatePlot <- function(predictions) {
  histogram.very.low <- ggplot(predictions, aes(x=very_low)) + geom_histogram(colour = "white", fill = "#E69F00", binwidth = 0.0025) + geom_vline(xintercept=mean(predictions$very_low), color="red")
  histogram.low <- ggplot(predictions, aes(x=low)) + geom_histogram(colour = "white", fill = "#56B4E9", binwidth = 0.0025)
  histogram.neutral <- ggplot(predictions, aes(x=neutral)) + geom_histogram(colour = "white", fill = "#009E73", binwidth = 0.0025)
  histogram.high <- ggplot(predictions, aes(x=high)) + geom_histogram(colour = "white", fill = "#F0E442", binwidth = 0.0025)
  histogram.very.high <- ggplot(predictions, aes(x=very_high)) + geom_histogram(colour = "white", fill = "#0072B2", binwidth = 0.0025)
  donut.plot <- generateDonutPlot(predictions)

  grid.arrange(histogram.very.low, donut.plot, histogram.low, histogram.neutral, histogram.high, histogram.very.high, nrow = 3, ncol = 2)
}
