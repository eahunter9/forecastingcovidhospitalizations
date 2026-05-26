library(tibble)
setwd("C:/Users/Elizabeth/OneDrive - National University of Ireland, Galway/calibration/waningimmunity/delay")
seasonal_data <- as.tibble(read.csv("seasonal predictions scenario1_ensemble.csv"))
#seasonal_data <- as.tibble(read.csv("seasonal_predictions_scenario1_2 ensemble.csv"))
#seasonal_data <- as.tibble(read.csv("seasonal predictions scenario 2 ensemble.csv"))
#seasonal_data <- as.tibble(read.csv("seasonal_predictions_scenario2_2 ensemble.csv"))
#seasonal_data <- as.tibble(read.csv("seasonal predictions scenario 2 ensemble.csv"))
#seasonal_data <- as.tibble(read.csv("seasonal_predictions_scenario3_2 ensemble.csv"))

seasonal_data$X <- c(851:864)
#seasonal_data$X <- c(881:894)
#seasonal_data$X <- c(891:904)

#seasonal_data$model <- "seasonal"

delay_data <- as.tibble(read.csv("scenario 1 delay ensemble.csv"))
#delay_data <- as.tibble(read.csv("scenario1_2 delay ensemble.csv"))
#delay_data <- as.tibble(read.csv("scenario 2 delay ensemble.csv"))
#delay_data <- as.tibble(read.csv("scenario2_2 delay ensemble.csv"))
#delay_data <- as.tibble(read.csv("scenario 2 delay ensemble.csv"))
#delay_data <- as.tibble(read.csv("scenario3_2 delay ensemble.csv"))

delay_data$X <- c(851:864)
#delay_data$X <- c(881:894)
#delay_data$X <- c(891:904)

#delay_data$model <- "delay"

realdata <- as.tibble(read.csv("Covidweekly_all.csv",header = TRUE))

#realdata <- as.tibble(read.csv("covid_data.csv",header = TRUE))

setwd("C:/Users/Elizabeth/OneDrive - National University of Ireland, Galway/calibration/waningimmunity/delay/python")


auto_arima_data <- as.tibble(read.csv("autoarima predictions.csv"))
#auto_arima_data <- as.tibble(read.csv("autoarima predictions short term.csv"))
#auto_arima_data <- as.tibble(read.csv("autoarima predictions 2ndpoint.csv"))
#auto_arima_data <- as.tibble(read.csv("autoarima predictions 2ndpoint shortterm.csv"))
#auto_arima_data <- as.tibble(read.csv("autoarima predictions 3rdpoint.csv"))
#auto_arima_data <- as.tibble(read.csv("autoarima predictions 3rdpoint shortterm.csv"))


#auto_arima_data$model <- "arima"
lstm_data<- as.tibble(read.csv("LSTM predictions.csv"))
#lstm_data<- as.tibble(read.csv("LSTM predictions short term.csv"))
#lstm_data<- as.tibble(read.csv("LSTM predictions 2nd point.csv"))
#lstm_data<- as.tibble(read.csv("LSTM predictions 2nd point short term.csv"))
#lstm_data<- as.tibble(read.csv("LSTM predictions 3rd point.csv"))
#lstm_data<- as.tibble(read.csv("LSTM predictions 3rd point short term.csv"))



darts_ensemble_data<- as.tibble(read.csv("ensemble darts scenario 1.csv"))




#lstm_data$model <- "lstm"

#alldata <- as.tibble(rbind(seasonal_data,delay_data,auto_arima_data,lstm_data))
#colnames(alldata) <- c("time","lower_bound","measurement","upper_bound","model")
#realdata_error <- realdata[54:60,]
realdata_error <- realdata[851:864,]
#realdata_error <- realdata[881:894,]
#realdata_error <- realdata[891:904,]


ensemble <- function(df1,df2,df3,df4,n){
  ensemble_q2.5 <- rep(0,n)
  ensemble_q5 <- rep(0,n)
  ensemble_q97.5 <- rep(0,n)

  for(i in 1:n){
  ensemble_q2.5[i] <- median(c(df1$lower_bound[i],df2$lower_bound[i],df3$lower_bound[i],df4$lower_bound[i]))
  ensemble_q5[i] <- median(c(df1$median[i],df2$median[i],df3$median[i],df4$median[i]))
  ensemble_q97.5[i] <- median(c(df1$upper_bound[i],df2$upper_bound[i],df3$upper_bound[i],df4$upper_bound[i]))


  }

  ensemble_output <- as.tibble(cbind(ensemble_q2.5,ensemble_q5,ensemble_q97.5))
  return(ensemble_output)
}







ensemble_accuracy <- function(df1,df2,df3,df4,ensemble,n){

  
  accuracy_1 <- rep(0,2)
  accuracy_2 <- rep(0,2)
  accuracy_3 <- rep(0,2)
  accuracy_4 <- rep(0,2)
  accuracy_5 <- rep(0,2)
  
  MAPE1 =0
  MAPE2 = 0
  MAPE3 = 0
  MAPE4 = 0
  MAPE5 = 0
  RMSE1 =0
  RMSE2 = 0
  RMSE3 = 0
  RMSE4 = 0
  RMSE5 = 0
  for(i in 1:n){
    
    MAPE1 <-   MAPE1 + abs((realdata_error$measurement[i] - df1$median[i])/realdata_error$measurement[i])
    MAPE2 <- MAPE2+ abs((realdata_error$measurement[i] - df2$median[i])/realdata_error$measurement[i])
    MAPE3 <- MAPE3+ abs((realdata_error$measurement[i] - df3$median[i])/realdata_error$measurement[i])
    MAPE4 <- MAPE4+ abs((realdata_error$measurement[i] - df4$median[i])/realdata_error$measurement[i])
    MAPE5 <- MAPE5+ abs((realdata_error$measurement[i] - ensemble$ensemble_q5[i])/realdata_error$measurement[i])
    
    RMSE1 <- RMSE1 + (realdata_error$measurement[i]-df1$median[i])^2
    RMSE2 <- RMSE2 + (realdata_error$measurement[i]-df2$median[i])^2
    RMSE3 <- RMSE3 + (realdata_error$measurement[i]-df3$median[i])^2
    RMSE4 <- RMSE4 + (realdata_error$measurement[i]-df4$median[i])^2
    RMSE5 <- RMSE5 + (realdata_error$measurement[i]-ensemble$ensemble_q5[i])^2
    
    
  }
  accuracy_1[1] =MAPE1/n * 100
  accuracy_2[1] = MAPE2/n * 100
  accuracy_3[1] = MAPE3/n * 100
  accuracy_4[1]= MAPE4/n* 100
  accuracy_5[1]= MAPE5/n * 100
  
  accuracy_1[2] =sqrt(RMSE1/n)
  accuracy_2[2] = sqrt(RMSE2/n)
  accuracy_3[2] = sqrt(RMSE3/n)
  accuracy_4[2]= sqrt(RMSE4/n)
  accuracy_5[2]= sqrt(RMSE5/n)
  
  accuracy <- as.tibble(rbind(accuracy_1,accuracy_2,accuracy_3,accuracy_4,accuracy_5))
  return(accuracy)
}


output <- ensemble(seasonal_data, delay_data,auto_arima_data,lstm_data,14)
#output <- ensemble(auto_arima_data,seasonal_data,delay_data,lstm_data,100)

#accuracy <- ensemble_accuracy(seasonal_data, delay_data,auto_arima_data,lstm_data,output,14)
#accuracy <- ensemble_accuracy(auto_arima_data,seasonal_data,delay_data,lstm_data,output,100)
accuracy <- ensemble_accuracy(seasonal_data, darts_ensemble_data,auto_arima_data,lstm_data,output,14)



colnames(output) <- c("lower_bound","measurement","upper_bound")
#output$time <- c(54:60)
#output$time <- c(851:864)
#output$time <- c(881:894)
output$time <- c(891:904)

output$model <- "ensemble"
#alldata<-as.tibble(rbind(alldata,output))
#real_data <- realdata[54:60,]
#real_data <- realdata[314:864,]
#real_data <- realdata[700:864,]
#real_data <- realdata[314:894,]
#real_data <- realdata[700:894,]
real_data <- realdata[314:904,]
#real_data <- realdata[700:904,]
#time <- c(54:60)
#time <- c(314:864)
#time <- c(700:864)
#time <- c(314:894)
#time <- c(700:894)
time <- c(314:904)
#time <- c(700:904)

length(time)
real_data$time  <- time

library(tidyverse)
g <- ggplot(output, aes(x = time, y = measurement, color = "Ensemble"))+#, y = measurement, color = period, group =1)) +
  
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound),
              alpha = 0.5, color = "grey70") +
  geom_line( aes(y = measurement)) +
  geom_line(data = real_data, size = 0.5, colour = "grey30",alpha = 0.8) +
  geom_line(data = lstm_data,size =0.5, aes(x = X, y=median, color = "LSTM Model"))+
  geom_line(data = auto_arima_data,size =0.5, aes(x = X, y= median,color = "ARIMA Model"))+
  geom_line(data = delay_data,size =0.5, aes(x = X, y=median, color = "Delay ODE"))+
  geom_line(data = seasonal_data,size =0.5, aes(x = X, y=median, color = "Seasonal ODE"))+
  labs(x = "Time", y = "Hospitalizations",color = "legend") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=15),legend.text =element_text(size = 13),legend.title = element_text(size = 14)) 

  
g



library(tidyverse)
g <- ggplot(output, aes(x = time, y = measurement, color = "Ensemble"))+#, y = measurement, color = period, group =1)) +
  
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound),
              alpha = 0.5, fill = "pink") +
  geom_line( aes(y = measurement)) +
  geom_line(data = real_data, size = 0.5, colour = "grey30",alpha = 0.8) +
  #geom_line(data = lstm_data,size =0.5, aes(x = X, y=median, color = "LSTM Model"))+
  #geom_line(data = auto_arima_data,size =0.5, aes(x = X, y= median,color = "ARIMA Model"))+
  #geom_line(data = delay_data,size =0.5, aes(x = X, y=median, color = "Delay ODE"))+
  #geom_line(data = seasonal_data,size =0.5, aes(x = X, y=median, color = "Seasonal ODE"))+
  labs(x = "Time", y = "Hospitalizations",color = "legend") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=15),legend.text =element_text(size = 13),legend.title = element_text(size = 14)) 


g

library(tidyverse)
arima_plot <- ggplot(auto_arima_data, aes(x = X, y = median, color = "Auto-Arima"))+#, y = measurement, color = period, group =1)) +
  
  geom_ribbon(data = auto_arima_data,aes(ymin = lower_bound, ymax = upper_bound),
              alpha = 0.5, fill = "pink") +
  geom_line( aes(y = median)) +
  geom_line(data = real_data,aes(x = time, y = measurement), size = 0.5, colour = "grey30",alpha = 0.8)+
    
    #geom_line(data = lstm_data,size =0.5, aes(x = X, y=median, color = "LSTM Model"))+
  #geom_line(data = auto_arima_data,size =0.5, aes(x = X, y= median,color = "ARIMA Model"))+
  #geom_line(data = delay_data,size =0.5, aes(x = X, y=median, color = "Delay ODE"))+
  #geom_line(data = seasonal_data,size =0.5, aes(x = X, y=median, color = "Seasonal ODE"))+
  labs(x = "Time", y = "Hospitalizations",color = "legend") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=15),legend.text =element_text(size = 13),legend.title = element_text(size = 14)) 


arima_plot


lstm_plot <- ggplot(lstm_data, aes(x = X, y = median, color = "LSTM"))+#, y = measurement, color = period, group =1)) +
  
  geom_ribbon(data = lstm_data,aes(ymin = lower_bound, ymax = upper_bound),
              alpha = 0.5, fill = "pink") +
  geom_line( aes(y = median)) +
  geom_line(data = real_data,aes(x = time, y = measurement), size = 0.5, colour = "grey30",alpha = 0.8)+

#geom_line(data = lstm_data,size =0.5, aes(x = X, y=median, color = "LSTM Model"))+
#geom_line(data = auto_arima_data,size =0.5, aes(x = X, y= median,color = "ARIMA Model"))+
#geom_line(data = delay_data,size =0.5, aes(x = X, y=median, color = "Delay ODE"))+
#geom_line(data = seasonal_data,size =0.5, aes(x = X, y=median, color = "Seasonal ODE"))+
labs(x = "Time", y = "Hospitalizations",color = "legend") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=15),legend.text =element_text(size = 13),legend.title = element_text(size = 14)) 

lstm_plot
