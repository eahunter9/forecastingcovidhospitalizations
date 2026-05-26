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
stan_filepath <- file.path( "Seasonal ODE.stan")



stan_d <- list(n_obs      = (nrow(syn)),
               n_weeks = nrow(syn),
               Hospitalizations  = syn$measurement,
               t0         = 0,
               n_params = 10,
               n_difeq  = 5,
               ts         = 1:(nrow(syn))
               )



mod_sde         <- cmdstan_model(stan_filepath)

fit <- mod_sde$sample(data              = stan_d,
                  chains            =4,
                  parallel_chains   = 4,
                  iter_warmup       = 1000,
                  iter_sampling     = 1000,
                  refresh           = 100,
                  save_warmup       = FALSE,  
                  max_treedepth  = 15

                 ) 


fit$diagnostic_summary()


posterior_SEIR <-  as_draws_df(fit$draws()) %>%
  as_tibble()
