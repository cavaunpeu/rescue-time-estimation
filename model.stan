data {
    int<lower=1> N;
    real very_distracting[N];
    real distracting[N];
    real neutral[N];
    real productive[N];
}

parameters {
    real phi_a;
    real phi_b;
    real phi_c;
    real phi_d;
    real phi_e;
    real<lower=0, upper=1> sigma;
}

transformed parameters {
    real mu_a;
    real mu_b;
    real mu_c;
    real mu_d;
    mu_a = exp(phi_a) / ( exp(phi_a) + exp(phi_b) + exp(phi_c) + exp(phi_d) + exp(phi_e) );
    mu_b = exp(phi_b) / ( exp(phi_a) + exp(phi_b) + exp(phi_c) + exp(phi_d) + exp(phi_e) );
    mu_c = exp(phi_c) / ( exp(phi_a) + exp(phi_b) + exp(phi_c) + exp(phi_d) + exp(phi_e) );
    mu_d = exp(phi_d) / ( exp(phi_a) + exp(phi_b) + exp(phi_c) + exp(phi_d) + exp(phi_e) );
}

model {
    sigma ~ uniform( 0 , 1 );
    phi_a ~ normal( 0 , 1 );
    phi_e ~ normal( 0 , 1 );
    phi_d ~ normal( 0 , 1 );
    phi_c ~ normal( 0 , 1 );
    phi_b ~ normal( 0 , 1 );
    very_distracting ~ normal(mu_a, sigma);
    distracting ~ normal(mu_b, sigma);
    neutral ~ normal(mu_c, sigma);
    productive ~ normal(mu_d, sigma);
}
