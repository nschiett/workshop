// Stan model 
data {
 int < lower = 1 > N; // Sample size
 vector[N] y; // Data
 vector[N] x; // Data

}
parameters {
 real < lower = 0 > sigma; // Error SD
 real a;
 real < upper = 0> b;
}
transformed parameters{
  real mu[N];
  for (n in 1:N){
  mu[n] = a+ b*x[n];
  }
}
model {
 a ~ normal(0, 10);
 b ~ normal(0, 10);
 target += student_t_lpdf(sigma | 3, 0, 10);
 y ~ normal(mu, sigma);
}
generated quantities{
  real yrep[N];
  for (n in 1:N){
  yrep[n] = normal_rng(mu[n], sigma);
  }
}
