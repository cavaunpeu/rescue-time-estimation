library(rstan)

buildModel <- function(report, chains = 1, cores = 1, iter = 1000, warmup = 1000) {
  data <- list(very_distracting = report$very_distracting, distracting = report$distracting, neutral = report$neutral, productive = report$productive, very_productive = report$productive, N = nrow(report))
  model <- stan(file = "model.stan",  data = data, chains = chains, cores = cores, iter = iter)
  return( model )
}

simulatePredictions <- function(model) {
  posterior.samples <- as.data.frame(model)
  simulated.predictions <- computePredictions(posterior.samples)
  return( simulated.predictions )
}

computePredictions <- function(s) {
  exp.phi.a <- sapply(X = s$phi_a, FUN = exp)
  exp.phi.b <- sapply(X = s$phi_b, FUN = exp)
  exp.phi.c <- sapply(X = s$phi_c, FUN = exp)
  exp.phi.d <- sapply(X = s$phi_d, FUN = exp)
  exp.phi.e <- sapply(X = s$phi_e, FUN = exp)
  a <- as.vector( exp.phi.a / (exp.phi.a + exp.phi.b + exp.phi.c + exp.phi.d + exp.phi.e) )
  b <- as.vector( exp.phi.b / (exp.phi.a + exp.phi.b + exp.phi.c + exp.phi.d + exp.phi.e) )
  c <- as.vector( exp.phi.c / (exp.phi.a + exp.phi.b + exp.phi.c + exp.phi.d + exp.phi.e) )
  d <- as.vector( exp.phi.d / (exp.phi.a + exp.phi.b + exp.phi.c + exp.phi.d + exp.phi.e) )
  e <- 1 - a - b - c - d
  return( data.frame(very_distracting = a, distracting = b, neutral = c, productive = d, very_productive = e) )
}
