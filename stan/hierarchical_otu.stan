// Stan model 
data {
 int < lower = 1 > N; // Sample size
 vector[N] y; // otu rel abundance
 int<lower = 1> T; // number of phylums
 int<lower = 1> L; // number of locations
 int<lower = 1> P; // number of primer techniques
 int<lower = 1> tax[N]; 
 int<lower = 1> loc[N]; 
 int<lower = 1> pri[N]; 
 }
parameters {
 real<lower = 0, upper = 1> otu;
 real z_pri[T,P];  // effect of each primer for each phylum
 real z_loc[T,L];  // effect of each location for each phylum

 //real < lower = 0 > sigma[T,P]; // Error SD
 real sigma;
 real sigma_pri;
 real sigma_loc;
 real<lower = 2> nu;

}

transformed parameters{
  real mu[N];
  real r_pri[T,P];  // effect of each primer for each phylum
  real r_loc[T,L];  // effect of each location for each phylum

  for (t in 1:T){
    for (l in 1:L){
   r_loc[t,l] = z_loc[t,l] * sigma_loc;
   }
   for (p in 1:P){
   r_pri[t,p] = z_pri[t,p] * sigma_pri;
   }
  }
  for (n in 1:N){

  mu[n] = (otu + r_pri[tax[n], pri[n]] +  r_loc[tax[n], loc[n]]);
  }
}
model {
 otu ~ student_t(3, 0.1, 0.1);

 for (t in 1:T){
 z_pri[t,] ~ student_t(3,0,1);
 z_loc[t,] ~ student_t(3,0,1);

 }
 sigma ~ student_t(3,0,0.1);
 sigma_pri ~ student_t(3,0,0.1);
 sigma_loc ~ student_t(3,0,0.1);

  // see Stan prior distribution suggestions
 nu ~ gamma(2, 0.1);
  // likelihood
     for (n in 1:N){

 target += student_t_lpdf(y[n]| nu, mu[n], sigma);
     }
}
generated quantities{
  real yrep[N];
   for (n in 1:N){
  yrep[n] = student_t_rng(nu, mu[n], sigma);
  }
}
