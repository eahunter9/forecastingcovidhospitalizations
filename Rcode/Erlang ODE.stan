functions{
  vector SEIRW(real time, vector y, array[] real params) {
    vector[8] dydt;
    real lambda_parameter;
    real Hosp_Per_Day;
    real SE;
    real EI;
    real IR;
    real RW;
    real WW1;
    real WW2;
    real WW3;
    real WW4;
    real WS;


  lambda_parameter = (params[1])*y[3]/5300000;
    SE = lambda_parameter*y[1];
    EI = params[7]*y[2];
    IR = y[3]*params[8];
    RW =  y[4] / (params[2]/4);
    WW1 = y[6] / (params[2]/4);
    WW2= y[7] / (params[2]/4);
    WS = y[8] / (params[2]/4);
    Hosp_Per_Day = params[3]*IR;

    dydt[1] = -SE + WS ;
    dydt[2] = SE-EI;
    dydt[3] = EI-IR ;
    dydt[4] = IR - RW;
    dydt[5] = Hosp_Per_Day;
    dydt[6] = RW - WW1;
    dydt[7] = WW1 - WW2;
    dydt[8] = WW2 - WS;

   // print(dydt);
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
  real<lower = 0.1, upper = 1> sigma;
  real<lower = 0.1, upper = 1> gamma;
    real<lower = 1, upper = 50> delay_duration;
  real<lower = 0, upper = 0.5> hr;
  real<lower = 0, upper = 0.5> I0;
  real<lower = 0, upper = 0.5> R0;
  real<lower = 0, upper = 0.5> E0;


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
  x0[5] = 0;
  x0[6] = 0;
  x0[7] = 0;
  x0[8] = 0;
  params[1] =  beta;

 params[2] = delay_duration;
 params[3] = hr;
    params[4] = E0;
  params[5] = I0;
  params[6] = R0;
 params[7] = sigma;
 params[8] = gamma;
 

  
  o = ode_rk45(SEIRW, x0, t0, ts, params);
  x[1] =  o[1, 5]  - x0[5];
  for (i in 1:n_obs-1) {

   x[i + 1] = o[i + 1, 5] - o[i, 5] + 1e-5;
  }

} 
model {
 beta   ~ lognormal(0.5, 10);
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
