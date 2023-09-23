data {
  int<lower=0> N; //Number of Studies
  real mean_gem[N]; //Mean of GEM models
  real mean_comp[N]; //Mean of comparator models
  real sd_gem[N]; //SD of GEM models
  real sd_comp[N]; //SD of comparator models
}

parameters {
  real delta[N];
  real<lower=0> tau;
  vector[N] re;
}

model {
  tau ~ uniform(0, 2.5);
  for (i in 1:N){
    real mu_gem = mean_gem[i];
    real mu_comp = mean_comp[i];
    real sigma_gem = sd_gem[i];
    real sigma_comp = sd_comp[i];
    
    real delta_mu = mu_gem - mu_comp;
    
    delta[i] ~ normal(delta_mu, sqrt(sigma_gem^2 + sigma_comp^2 + tau^2));
  }
}

