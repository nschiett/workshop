// Stan model 
data {
 int < lower = 1 > N; // Sample size
 vector[N] y; // Data
}
parameters {
 real<lower = 0> mu; 
 real < lower = 0 > sigma; // Error SD
}
model {
 mu ~ normal(100, 100);
 sigma ~ normal(0, 10);
 y ~ normal(mu, sigma);
}
generated quantities{
  real yrep;
  yrep = normal_rng(mu, sigma);
}
