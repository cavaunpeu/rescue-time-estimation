source("input.R")
source("model.R")
source("plot.R")

lapply(c("reshape2", "rstan", "tidyr"), require, character.only = TRUE)

# build model
report <- readReport("~/Downloads/RescueTime_Report_Productivity__by_week_2016-01-01.csv")
model <- buildModel(report, iter = 4000)
predictions <- simulatePredictions(model)

# inspect data
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# box plot
report.long <- melt(report)
ggplot(report.long, aes(x = factor(variable), y = value)) +
  geom_boxplot(aes(fill = factor(variable))) +
  labs(x = "Productivity Level", y = "Percentage Observed", title = "Observed Productivity Level Distributions") +
  theme(plot.title = element_text(hjust = .5)) +
  scale_fill_discrete("Productivity Level")
ggsave(file="figures/empirical_boxplot.png", dpi=300)

# individual histograms
histogram.very.low <- ggplot(report, aes(x=very_low)) +
  geom_histogram(colour = "white", fill = "#E69F00", binwidth = 0.01) +
  labs(
    title = "Empirical Distribution of 'Very Distracting Time'",
    x = "Very Distracting Time (seconds)") +
  theme(plot.title = element_text(hjust = 0.5))
histogram.very.low

histogram.low <- ggplot(report, aes(x=low)) +
  geom_histogram(colour = "white", fill = "#56B4E9", binwidth = 0.02) +
  labs(
    title = "Empirical Distribution of 'Distracting Time'",
    x = "Distracting Time (seconds)") +
  theme(plot.title = element_text(hjust = 0.5))
histogram.low

histogram.neutral <- ggplot(report, aes(x=low)) +
  geom_histogram(colour = "white", fill = "#009E73", binwidth = 0.02) +
  labs(
    title = "Empirical Distribution of 'Neutral Time'",
    x = "Neutral Time (seconds)") +
  theme(plot.title = element_text(hjust = 0.5))
histogram.neutral

histogram.productive <- ggplot(report, aes(x=low)) +
  geom_histogram(colour = "white", fill = "#F0E442", binwidth = 0.02) +
  labs(
    title = "Empirical Distribution of 'Productive Time'",
    x = "Productive Time (seconds)") +
  theme(plot.title = element_text(hjust = 0.5))
histogram.productive

histogram.very.high <- ggplot(report, aes(x=low)) +
  geom_histogram(colour = "white", fill = "#0072B2", binwidth = 0.02) +
  labs(
    title = "Empirical Distribution of 'Very Productive Time'",
    x = "Very Productive Time (seconds)") +
  theme(plot.title = element_text(hjust = 0.5))
histogram.very.high

# inspect model
parameters <- c("mu_a", "mu_b", "mu_c", "mu_d", "sigma")
print(model, pars = parameters, probs = c(.015, .985))
traceplot(model, pars = parameters, inc_warmup = TRUE)
ggsave("figures/traceplot.png", dpi=300)

# plot posteriors
predictions.long <- melt(predictions)
ggplot(predictions.long, aes(x = value, fill = variable)) +
  geom_histogram(binwidth = .003, colour = "black") +
  facet_wrap(~variable, ncol = 1) +
  labs(title = TeX("Posterior Distributions of $\\mu_i$ \n for Distinct Productivity Levels"), x = TeX('$\\mu_i$')) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("figures/posteriors.png", dpi=300)

# compute column means
generateDonutPlot(predictions)
ggsave("figures/donut_plot.png", dpi=300)
