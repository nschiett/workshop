// Stan model 
data {
 int < lower = 1 > N; // Sample size
 vector[N] y; // Data
 vector[N] x; // Data
 int flow[N]; //
 int tax[N];

}
parameters {
 real < lower = 0 > sigma; // Error SD
 real a;
 real <upper = 0> b;
 real r_a[2, 6];
 real r_b[2, 6];
}
transformed parameters{
 real a_t[2, 6];
 real b_t[2, 6];
 real mu[N];

  for (i in 1:2){
    for (j in 1:6){
      a_t[i,j] = a + r_a[i,j];
      b_t[i,j] = b + r_b[i,j];
    }
  }
  for (n in 1:N){
  mu[n] = a_t[flow[n], tax[n]] + b_t[flow[n], tax[n]]*x[n];
  }
}
model {
  for (i in 1:6){
 r_a[,i] ~ normal(0, 1);
 r_b[,i] ~ normal(0, 1);
  }
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
