# load data
weekly.percentages <- matrix(
  data = c(
    c(.2, .4, .14, .16, .1),
    c(.22, .38, .14, .16, .1),
    c(.2, .42, .12, .16, .1),
    c(.2, .4, .16, .14, .1),
    c(.2, .4, .14, .18, .08),
    c(.2, .4, .14, .16, .1),
    c(.15, .4, .14, .16, .15),
    c(.15, .45, .09, .16, .15)
  ),
  nrow = 5,
  ncol = 8
)
weekly.percentages <- as.data.frame( t(weekly.percentages) )
colnames(weekly.percentages) <- c("a", "b", "c", "d", "e")
weekly.percentages <- do.call("rbind", replicate(n = 5, expr = weekly.percentages, simplify = FALSE))

# fit a model for a
library(rethinking)

model.a <- map(
  alist(
    a ~ dnorm(mean = mu.a, sd = sigma),
    mu.a <- exp(phi.a) / ( exp(phi.a) + exp(phi.b) + exp(phi.c) + exp(phi.d) + exp(phi.e) ),
    c(phi.a, phi.b, phi.c, phi.d, phi.e) ~ dnorm(mean = 0, sd = 1000),
    sigma ~ dunif(min = 0, max = 10)
  ),
  data = weekly.percentages
)

samples <- extract.samples(object = model.a, n = 500)
# for now, you'll need to clip this below at 0, because some of the samples are negative, because you haven't yet done anything with b, c, d nor e

compute.predicted.a <- function(s) {
  mu <- exp(s['phi.a']) / ( exp(s['phi.a']) + exp(s['phi.b']) + exp(s['phi.c']) + exp(s['phi.d']) + exp(s['phi.e']) )
  prediction <- rnorm(n = 1, mean = mu, sd = s['sigma'])
  return(prediction)
}

posterior.predictive.distribution <- apply(X = samples, MARGIN = 1, FUN = function(s) compute.predicted.a(s))
hist(posterior.predictive.distribution)
