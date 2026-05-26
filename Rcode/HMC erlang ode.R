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


source("stan_data.R")

seir_mdl_sde <- list("Hospitalizations ~ poisson(net_flow(Hospitalizations))")


DT = 1/10

syn <- as.tibble(read.csv("Covidweekly_all.csv",header = TRUE))

#syn <- syn[314:851,]
#syn <- syn[700:851,]
#syn <- syn[314:880,]
#syn <- syn[700:880,]
#syn <- syn[700:890,]
syn <- syn[314:890,]

ggplot(syn,aes(x=time,y=measurement))+geom_point()+geom_line()+
  theme_classic()

stan_filepath <- file.path( "Erlang ODE.stan")




stan_d <- list(n_obs      = (nrow(syn)),
               n_weeks = nrow(syn),
               Hospitalizations  = syn$measurement,
               t0         = 0,
               n_params = 8,
               n_difeq  = 8,
               ts         = 1:(nrow(syn))
               )



mod_sde         <- cmdstan_model(stan_filepath)#, include_paths= c("C:/Users/453798/sde_stan"))


fit <- mod_sde$sample(data              = stan_d,
                  chains            =4,
                  parallel_chains   = 4,
                  iter_warmup       = 1000,
                  iter_sampling     = 1000,
                  refresh           = 100,
                  save_warmup       = FALSE,  
                  max_treedepth  = 15
               #  init = list(initial_1)
                # init_r = .1
                 ) 


fit$diagnostic_summary()


posterior_SEIR <-  as_draws_df(fit$draws()) %>%
  as_tibble()
