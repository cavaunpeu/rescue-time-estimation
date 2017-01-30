library(rethinking)

buildModel <- function(report) {
  map2stan(
    alist(
      very_low ~ dnorm(mean = mu_a, sd = sigma),
      low ~ dnorm(mean = mu_b, sd = sigma),
      neutral ~ dnorm(mean = mu_c, sd = sigma),
      high ~ dnorm(mean = mu_d, sd = sigma),
      very_high <- 1 - very_low - low - neutral - high,
      mu_a <- exp(phi_a) / ( exp(phi_a) + exp(phi_b) + exp(phi_c) + exp(phi_d) + exp(phi_e) ),
      mu_b <- exp(phi_b) / ( exp(phi_a) + exp(phi_b) + exp(phi_c) + exp(phi_d) + exp(phi_e) ),
      mu_c <- exp(phi_c) / ( exp(phi_a) + exp(phi_b) + exp(phi_c) + exp(phi_d) + exp(phi_e) ),
      mu_d <- exp(phi_d) / ( exp(phi_a) + exp(phi_b) + exp(phi_c) + exp(phi_d) + exp(phi_e) ),
        c(phi_a, phi_b, phi_c, phi_d, phi_e) ~ dnorm(mean = 0, sd = 1),
      sigma ~ dunif(min = 0, max = 1)
    ),
    data = report
  )
}

simulatePredictions <- function(model) {
  posterior.samples <- extract.samples(model, n = 1e3)
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
  return( data.frame(very_low = a, low = b, neutral = c, high = d, very_high = e) )
}
