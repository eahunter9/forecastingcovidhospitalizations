functions{
  vector SEIRW(real time, vector y, array[] real params) {
    vector[5] dydt;
    real lambda_parameter;
     real beta_seasonal;
    real Hosp_Per_Day;
    real SE;
    real EI;
    real IR;
    real RS;


  beta_seasonal =params[1]*(1+params[2]*cos(params[3]*time));
  lambda_parameter = beta_seasonal *y[3]/5300000;
    SE = lambda_parameter*y[1];
    EI = params[9]*y[2];
    IR = y[3]*params[10];
    RS =  y[4] / (params[4]);
    Hosp_Per_Day = params[5]*IR;

    dydt[1] = -SE + RS;
    dydt[2] = SE-EI;
    dydt[3] = EI-IR ;
    dydt[4] = IR - RS;
    dydt[5] = Hosp_Per_Day;


    return dydt;
  }
  
}


data {
 int<lower = 1> n_obs;
 int<lower = 1> n_params;
 int<lower = 1> n_difeq;
  int<lower = 1> n_weeks;

 array[(n_obs)] int Hospitalizations;
 real t0;
 array[n_obs] int ts;


}
parameters {
  real<lower = 0.5,upper = 10> beta;
  real<lower = 0, upper = 1> beta_amplitude;
  real<lower = 0, upper = 0.1> beta_period;
    real<lower = 1, upper = 50> delay_duration;
  real<lower = 0, upper = 0.5> hr;
  real<lower = 0, upper = 0.25> I0;
  real<lower = 0, upper = 0.25> R0;
  real<lower = 0, upper = 0.25> E0;
  real<lower = 0, upper = 1> sigma;

  real<lower = 0, upper = 1> gamma;

}
transformed parameters{
   array[n_obs] vector[n_difeq] o; // Output from the ODE solver
  array[n_obs] real x;
  vector[n_difeq] x0;
  array[n_params] real params;

 x0[1] =5300000-5300000*E0-5300000*I0-5300000*R0;
  x0[2] = 5300000*E0;
  x0[3] =5300000*I0;
  x0[4] = 5300000*R0;  
  x0[5] = 23;
  params[1] =  beta;

 params[2] =beta_amplitude;
  params[3] =beta_period;
  params[4] = delay_duration;
 params[5] = hr;
    params[6] = E0;
  params[7] = I0;
  params[8] = R0;
 params[9] = sigma;
params[10] = gamma;
 
  
  o = ode_rk45(SEIRW, x0, t0, ts, params);
  x[1] =  o[1, 5]  - x0[5];
  for (i in 1:n_obs-1) {

   x[i + 1] = o[i + 1, 5] - o[i, 5] + 1e-5;
  }

} 
model {
  beta_amplitude  ~ lognormal(0, 1);
    beta_period  ~ lognormal(0, 0.1);

 beta   ~ lognormal(0, 10);
  delay_duration    ~ lognormal(1, 50);
   hr ~ lognormal(0, 0.5);
    E0 ~lognormal(0, 1);
 I0~lognormal(0, 1);
 R0 ~lognormal(0, 1);
  sigma~ lognormal(0, 1);
gamma~ lognormal(0, 1);

    Hospitalizations ~ poisson(x);

}
generated quantities {
  real log_lik;
  log_lik = poisson_lpmf(Hospitalizations|x);

}
