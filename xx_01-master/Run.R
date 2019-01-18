#Library needed for this script
library(data.table)
library(dplyr)
library(ISOweek)
library(tidyr)
library(ggplot2)
library(forecast)
library(lubridate)


#Change the working directory
setwd("/git/xx_01_submission/xx_01-master")

fileSources = file.path("code", list.files("code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

#Load the data
d <- readRDS("data_raw/individual_level_data.RDS")
#Create new numerical value 
d$newvalue <- as.numeric(sub("([0-9]+).*$", "\\1", d$value))

#Split the d data based on the municipalities
X <- split(d,d$location)

#Create fake data within the same time frame as the raw data
fakedata <- data.table(date = seq.Date(as.Date("2000-01-01"), as.Date("2010-12-31"), by = 1))


#==================


#AGGREGATING THE DATA
for (i in 1:422) {
     #Aggregate the raw data by the date, for every municipality 
     d=left_join(fakedata, aggregate(data=X[[i]], newvalue ~ date + location, FUN=sum), by="date")
     #Fill in the NA gaps with the corresponding municipality in the location column and 0 in the newvalues column (reffering to 0 sick people)
     d$location <- fillNAgaps(d$location)
     d$newvalue[is.na(d$newvalue)] <- 0
     
     d$date <- strptime(d$date, "%Y-%m-%d")
     d$date <- as.Date(d$date)
     
     #Split the data into the training and production data
     training <- subset(d, date<"2010-01-01")
     production <- subset(d, date>"2010-01-01")
     
     #Colaps the data into the iso-year/iso-week
     training$week <- ISOweek(training$date)
     training <- aggregate(data=training, newvalue ~ week + location, FUN=sum)
     production$week <- ISOweek(production$date)
     production <- aggregate(data=production, newvalue ~ week + location, FUN=sum)

     #Saving all 422 training datasets
     nam <- paste("training",i,sep="")
     assign(nam, training)
   }


#==================
  

#CHECKING SEASONALITY 
timedata <- data.frame()
#Counter starts from 0 so that first value does not end up as a header in timedata
for (i in 0:422) {
  #Compute the periodogram to identify the dominant periods
  p <- periodogram(training$newvalue)
  temp <- data.frame(freq=p$freq,spec=p$spec)
  order <- temp[order(-temp$spec),]
  top2 <- head(order,2)
  #Convert the frequency with the biggest power to time, to calculate the seasonality
  time <- 1/top2$freq
  #Save the time of the most dominant period of each municipality in a timedata
  timedata <- rbind(timedata, time)
}
#Timedata has in the first column only values of 54 
#The weekly data i.e. training1 to training422 is periodic with the period being 54 weeks which is approx 1 year

#==================


#DECOMPOSING THE DATA FOR ONE MUNICIPALITY (155th randomly chosen)
training155 <- ts(training155$newvalue, frequency=365.25/7, start=decimal_date(ymd("2000-01-01"))) #end=decimal_date(ymd("2001-01-01")))
trainingcomponent <- decompose(training155)
plot(trainingcomponent)

#Splitting the data into seasonality variable, trend variable and random variable (in additive model when they are summed up they represent observed data)
split155 <- stl(training155, s.window="periodic")

#Fitting ARIMA models
#Finding best K to minimize AICc 
bestfit=list(aicc=Inf)
for (i in 1:25) {
  fit <- auto.arima(training155, xreg=fourier(training155,K=i), seasonal=FALSE)
  if(fit$aicc < bestfit$aicc)
    bestfit <- fit
    
  else break;
  
  print(i)
  print(fit$aicc)
  }

#K=1 for 155th municipality
fc155 <- forecast(bestfit, xreg=fourier(training155, K=1, h=365.25/7))
autoplot(fc155)


#==================


#GGPLOT script with the dummy data

training1$mean <- mean(training1$newvalue)
training1$sd <- as.numeric(sd(training1$newvalue))

ggplot(data=training1, aes(x=week, y=newvalue, group=1)) +
  geom_line() +
  geom_ribbon(aes(ymin=training1$mean-training1$newvalue, ymax=training1$mean+training1$newvalue),linetype=2,alpha=0.1,fill="red") + 
  geom_ribbon(aes(ymin=training1$newvalue, ymax=100),linetype=2,alpha=0.8,fill="blue") 



