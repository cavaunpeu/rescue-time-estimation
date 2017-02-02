source("input.R")
source("model.R")
source("plot.R")

lapply(c("reshape2", "rstan", "tidyr", "latex2exp"), require, character.only = TRUE)

# build model
report <- readReport("data/rescue_time_report.csv")
model <- buildModel(report, chains = 4, cores = 4, iter = 2000, warmup = 1000)
predictions <- simulatePredictions(model)

# create box plot
report.long <- melt(report)
ggplot(report.long, aes(x = factor(variable), y = value)) +
  geom_boxplot(aes(fill = factor(variable))) +
  labs(x = "Productivity Level", y = "Percentage Observed", title = "Observed Productivity Level Distributions") +
  theme(plot.title = element_text(hjust = .5)) +
  scale_fill_discrete("Productivity Level")
ggsave(file="figures/empirical_boxplot.png", dpi=300)

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
  labs(title = TeX("Posterior Distributions of Productivity-Level Proportions ($\\mu_i$)"), x = TeX('$\\mu_i$')) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, .65, .05))
ggsave("figures/posteriors.png", dpi=300)

# compute column means
generateDonutPlot(predictions)
ggsave("figures/donut_plot.png", dpi=300)
