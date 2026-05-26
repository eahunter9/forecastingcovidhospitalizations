library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(here)
library(cmdstanr)
library(posterior)
library(tidybayes)
library(tidyr)
library(readsdr)
library(bayesplot)
library(extraDistr)
library(GGally)
library(ggpubr)
library(ggridges)
library(gridExtra)
library(kableExtra)
library(Metrics)
library(patchwork)
library(purrr)
library(readr)
library(scales)
library(stringr)
library(viridisLite)
library(tibble)
library(deSolve)

pars_df <-  posterior_SEIR[, c("beta","beta_amplitude","beta_period","delay_duration","hr","I0","R0","E0","sigma","gamma","log_lik") ]
stocks_df <- pars_df

SEIR <- function(Time, State, Pars) {
  with(as.list(c(State, Pars, Time)), {
    beta_seasonal = beta_mean*(1+beta_amp*cos(Time*beta_p))
    lambda_parameter = (beta_seasonal)*Infected/5300000
    SE = lambda_parameter*Susceptible
    EI = sigma * Exposed
    IR = Infected * gamma
    RS = Recovered/delay_duration

    Hospitalizations = hr*IR
    dSusceptible = -SE + RS 
    dExposed = SE-EI
    dInfected = EI-IR
    dRecovered = IR - RS

     

    return(list(c(dSusceptible, dExposed, dInfected,dRecovered,Hospitalizations)))
  })
}

Hosp_all_ode <- matrix(0,4000,206)

for(i in 1:4000){
pars <- c(  beta_mean = as.numeric(stocks_df[i,1]),
            beta_amp = as.numeric(stocks_df[i,2]),
          beta_p= as.numeric(stocks_df[i,3]),
           delay_duration = as.numeric(stocks_df[i,4]),
           hr =as.numeric(stocks_df[i,5]),

           sigma=as.numeric(stocks_df[i,9]),
           gamma= as.numeric(stocks_df[i,10])
) # population



yini <- c(Susceptible =5300000-5300000*as.numeric(stocks_df[i,6])-5300000*as.numeric(stocks_df[i,7])-5300000*as.numeric(stocks_df[i,8]), Exposed = 5300000*as.numeric(stocks_df[i,8]), Infected = 5300000*as.numeric(stocks_df[i,6]), Recovered = 5300000*as.numeric(stocks_df[i,7]) , Hospitalizations = 23 )

times <- seq(2, 206, by = 1)
out_test <- ode(yini, times, SEIR, pars, method ="bdf")

Hosp_per_day_ode <-  rep(0,206)
Hosp_per_day_ode[1] = out_test[1,6]
for(k in 2:205){
Hosp_per_day_ode[k] <- out_test[k,6] - out_test[k-1,6]
}
Hosp_all_ode[i,] <-Hosp_per_day_ode
}








measurement <- rep(0,205)
lower_bound <- rep(0,205)
upper_bound <- rep(0,205)

for(i in 1:205){
  measurement[i] <- mean(Hosp_all_ode[,i],na.rm = TRUE)
  lower_bound[i] = quantile(Hosp_all_ode[,i], c(0.025, 0.975),na.rm = TRUE)[[1]]
  upper_bound[i] = quantile(Hosp_all_ode[,i], c(0.025, 0.975),na.rm = TRUE)[[2]]
  
}
period <- c(rep("calibration",191),rep("projection",14))
time <- c(1:205)

sim_data <- as_tibble(cbind(time,measurement ,lower_bound,upper_bound))
sim_data$period <- period
real_data <- as.tibble(read.csv("Covidweekly_all.csv",header = TRUE))
real_data$measurement <- as.numeric(round(real_data$measurement))
#real_data <- real_data[700:865,]
#real_data <- real_data[700:894,]
real_data <- real_data[700:904,]

#time <- c(1:166)
#time <- c(1:195)
time <- c(1:205)


real_data$time  <- time

library(tidyverse)
g <- ggplot(sim_data, aes(x = time, y = measurement))
  
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound, fill = period),
             alpha = 0.5, color = NA) +
  geom_line( aes(y = measurement,color = period)) +
  geom_line(data = real_data, size = 0.5, colour = "grey30",alpha = 0.8)+
labs(x = "Time", y = "Hospitalizations") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=15),legend.text =element_text(size = 13),legend.title = element_text(size = 14)) 


g