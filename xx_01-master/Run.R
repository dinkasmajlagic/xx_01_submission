#Library needed for this script
library(data.table)
library(dplyr)
library(ISOweek)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forecast)
library(TSA)


#Change the working directory
setwd("/git/xx_01_submission/xx_01-master")

#Run the functions from the folder code
fileSources = file.path("code", list.files("code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

#Load the data
d <- readRDS("data_raw/individual_level_data.RDS")
#Create new numerical value
d$newvalue <- as.numeric(sub("([0-9]+).*$", "\\1", d$value))

#Split the d data based on the municipalities
X <- split(d,d$location)

#Create fake data (to be able to fill in the days with no sick people)
fakedata <- data.table(date = seq.Date(as.Date("2000-01-01"), as.Date("2010-12-31"), by = 1))

for (i in 1:422) {
     #Aggregate by the date for every municipality 
     d=left_join(fakedata, aggregate(data=X[[i]], newvalue ~ date + location, FUN=sum), by="date")
     #Fill in the NA gaps with the corresponding municipality in the location column and 0 in the newvalues column (reffering to 0 sick people)
     d$location <- fillNAgaps(d$location)
     d$newvalue[is.na(d$newvalue)] <- 0
     
     d$date <- strptime(d$date, "%Y-%m-%d")
     d$date <- as.Date(d$date)
     
     #Split the data into the training and production data
     training <- subset(d, date<"2010-01-01")
     production <- subset(d, date>"2010-01-01")
     
     #Colaps the data into iso-year/iso-week
     training$week <- ISOweek(training$date)
     training <- aggregate(data=training, newvalue ~ week + location, FUN=sum)
     production$week <- ISOweek(production$date)
     production <- aggregate(data=production, newvalue ~ week + location, FUN=sum)

     nam <- paste("training",i,sep="")
     assign(nam, training)
     #nam <- paste("production",i,sep="")
     #assign(nam, production)
   }



#SEASONALITY
#Compute the Fourier transformation
#p will have a frequency on x axis and "power" of each frequency on y axis
p <- periodogram(training422$newvalue)

dd <- data.frame(freq=p$freq,spec=p$spec)
order <- dd[order(-dd$spec),]
top2 <- head(order,2)
#display the two highest "power" frequencies
top2

#Convert the frequency with the biggest power to time, to calculate the seasonality
time <- 1/top2$freq
#display the time of the biggest frequency (i.e. seasonality)
time

training423 <- ts(training422$newvalue, frequency=365.25/7) #start=decimal_date(ymd("2000-01-01")), end=decimal_date(ymd("2001-01-01")))
trainingcomponent <- decompose(training423)
plot(trainingcomponent)

#Splitting the data into seasonality variable, trend variable and random variable (in additive model when they are summed up they represent observed data)
stl(training423, s.window="periodic")

#Fitting ARIMA models
arima <- auto.arima(training423)

#Create 2SD prediction interval


#GGPLOT
a <- training152
a$mean <- mean(a$newvalue)
a$sd <- as.numeric(sd(a$newvalue))

ggplot(data=a, aes(x=week, y=newvalue, group=1)) +
  geom_line() +
  geom_ribbon(aes(ymin=a$mean-a$newvalue, ymax=a$mean+a$newvalue),linetype=2,alpha=0.1,fill="red") + 
  geom_ribbon(aes(ymin=a$newvalue, ymax=100),linetype=2,alpha=0.6,fill="blue") 


#Make a data frame for first municipality
#d1=do.call(rbind.data.frame, X[1])

#Make a data frame for each municipality
#for (i in 1:422) {
#nam <- paste("d",i,sep="")
#assign(nam, do.call(rbind.data.frame, X[i]))
#}

#Aggregate by the date for the first municipality
#dnew1 <- aggregate(data=d1, newvalue ~ date + location, FUN=sum)
#r1 <- left_join(fakedata,dnew1,by="date")
#r1$location <- fillNAgaps(r1$location)
#r1$newvalue[is.na(r1$newvalue)] <- 0

#Aggregate by municipality and date
#dnew <- aggregate(data=d, newvalue ~ date + location, FUN=sum)


