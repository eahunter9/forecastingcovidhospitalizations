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


FILE <- "ERLANG ODE.stmx"

mdl        <- read_xmile(FILE)

posterior_SEIR <-  as_draws_df(fit$draws()) %>%
  as_tibble()

consts_df <- posterior_SEIR[,2:9]

colnames(consts_df) <- c("Beta_parameter","sigma_p","gamma_p","delay_duration", "HR","initial_infected","initial_recovered","initial_exposed")

stocks_df <- as.data.frame(cbind(posterior_SEIR$`o[1,1]`,posterior_SEIR$`o[1,2]`,posterior_SEIR$`o[1,3]`,posterior_SEIR$`o[1,4]`,posterior_SEIR$`o[1,5]`,posterior_SEIR$`o[1,6]`,posterior_SEIR$`o[1,7]`,posterior_SEIR$`o[1,8]`))#,posterior_SEIR$`o[1,9]`,posterior_SEIR$`o[1,10]`))
colnames(stocks_df) <- c("S","E","I","R","Hospitalizations","W1","W2","W3")


test<-sd_sensitivity_run(mdl$deSolve_components,consts_df = consts_df,stocks_df = stocks_df, start_time =  0, stop_time = 205)

Hosp <- select(test,c("time","measurement"))



measurement = rep(0,205)
upper_bound = rep(0,205)
lower_bound = rep(0,205)
for(i in 1:205){
  measurement[i] <- mean(Hosp$measurement[Hosp$time == i])
  upper_bound[i] <- max(Hosp$measurement[Hosp$time == i])
  lower_bound[i] <- min(Hosp$measurement[Hosp$time == i])
  
}
time <- c(1:205)
period <- c(rep("calibration",191),rep("forecast",14))
forecast_data <- as.tibble(cbind(time,measurement,upper_bound,lower_bound))
forecast_data$period <- period



realdata <- as.tibble(read.csv("Covidweekly_all.csv",header = TRUE))

#realdata <- realdata[700:865,]
realdata <- realdata[700:904,]

#realdata$time <- c(1:191)
realdata$time <- c(1:205)

library(tidyverse)
forecast <- ggplot(forecast_data, aes(x = time, y = measurement))+#, y = measurement, color = period, group =1)) +
  
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound, fill = period),
              alpha = 0.5, color = NA) +
  geom_line( aes(y = measurement,color = period)) +
  geom_line(data = realdata, size = 0.5, colour = "grey30",alpha = 0.8) +
  labs(x = "Time", y = "Hospitalizations") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=15),legend.text =element_text(size = 13),legend.title = element_text(size = 14)) 


forecast