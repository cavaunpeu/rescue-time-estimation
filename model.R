library(rethinking)

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

model <- map2stan(
  alist(
    a ~ dnorm(mean = mu_a, sd = sigma),
    b ~ dnorm(mean = mu_b, sd = sigma),
    c ~ dnorm(mean = mu_c, sd = sigma),
    d ~ dnorm(mean = mu_d, sd = sigma),
    e <- 1 - a - b - c - d,
    mu_a <- exp(phi_a) / ( exp(phi_a) + exp(phi_b) + exp(phi_c) + exp(phi_d) + exp(phi_e) ),
    mu_b <- exp(phi_b) / ( exp(phi_a) + exp(phi_b) + exp(phi_c) + exp(phi_d) + exp(phi_e) ),
    mu_c <- exp(phi_c) / ( exp(phi_a) + exp(phi_b) + exp(phi_c) + exp(phi_d) + exp(phi_e) ),
    mu_d <- exp(phi_d) / ( exp(phi_a) + exp(phi_b) + exp(phi_c) + exp(phi_d) + exp(phi_e) ),
    c(phi_a, phi_b, phi_c, phi_d, phi_e) ~ dnorm(mean = 0, sd = 1),
    sigma ~ dunif(min = 0, max = 1)
  ),
  data = weekly.percentages,
  iter = 4000,
  warmup = 1000
)

plot(model)

posterior.samples <- extract.samples(model, n = 1e4)

computePredictions <- function(s) {
  exp.phi.a <- sapply(X = s["phi_a"], FUN = exp)
  exp.phi.b <- sapply(X = s["phi_b"], FUN = exp)
  exp.phi.c <- sapply(X = s["phi_c"], FUN = exp)
  exp.phi.d <- sapply(X = s["phi_d"], FUN = exp)
  exp.phi.e <- sapply(X = s["phi_e"], FUN = exp)
  a <- as.vector( exp.phi.a / (exp.phi.a + exp.phi.b + exp.phi.c + exp.phi.d + exp.phi.e) )
  b <- as.vector( exp.phi.b / (exp.phi.a + exp.phi.b + exp.phi.c + exp.phi.d + exp.phi.e) )
  c <- as.vector( exp.phi.c / (exp.phi.a + exp.phi.b + exp.phi.c + exp.phi.d + exp.phi.e) )
  d <- as.vector( exp.phi.d / (exp.phi.a + exp.phi.b + exp.phi.c + exp.phi.d + exp.phi.e) )
  e <- 1 - a - b - c - d
  return( data.frame(a=a, b=b, c=c, d=d, e=e))
}
