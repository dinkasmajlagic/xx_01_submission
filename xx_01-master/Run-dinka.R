library(data.table)
library(dplyr)
library(ISOweek)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forecast)


#Change the working directory
setwd("/git/xx_01_submission/xx_01-master")

fileSources = file.path("code", list.files("code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

source("code/FillNAgaps.R")

#Load the data
d <- readRDS("data_raw/individual_level_data.RDS")
#Create new numerical value
d$newvalue <- as.numeric(sub("([0-9]+).*$", "\\1", d$value))

#Split the d data based on municipalities
X <- split(d,d$location)

#Create fake data
fakedata <- data.table(date = seq.Date(as.Date("2000-01-01"), as.Date("2010-12-31"), by = 1))



for (i in 1:422) {
     #Aggregate by the date for every municipality + don't forget rows/days with zero sick people
     d=left_join(fakedata, aggregate(data=X[[i]], newvalue ~ date + location, FUN=sum), by="date")
     #Fill in the NA gaps with the corresponding municipality in the location column and 0 in the newvalues column (reffering to 0 sick people)
     d$location <- fillNAgaps(d$location)
     d$newvalue[is.na(d$newvalue)] <- 0
     #Colaps the data into iso-year/iso-week
     d$date <- strptime(d$date, "%Y-%m-%d")
     d$date <- as.Date(d$date)
     
     training <- subset(d, date<"2010-01-01")
     #training$week <- date2ISOweek(training$date)
     training$week <- ISOweek(training$date)
     training <- aggregate(data=training, newvalue ~ week + location, FUN=sum)
     #training$date <- ISOweek2date(training$week)
     #training <- ts(training$newvalue) #frequency=365.25/7) #start=decimal_date(ymd("2000-01-01")), end=decimal_date(ymd("2001-01-01")))
     #fit <- tbats(training)
     #seasonal <- !is.null(fit$seasonal)
     #nam <- paste("seasonal",i,sep="")
     #assign(nam, seasonal)
     
     #trainingcomponent <- decompose(training)
     #plot(trainingcomponent)
     
     #production <- subset(d, date>"2010-01-01")
     #production$week <- ISOweek(production$date)
     #production <- aggregate(data=production, newvalue ~ week + location, FUN=sum)
     
     nam <- paste("training",i,sep="")
     assign(nam, training)
     #nam <- paste("production",i,sep="")
     #assign(nam, production)
   }

#SEASONALITY

training423 <- ts(training422$newvalue, frequency=365.25/7) #start=decimal_date(ymd("2000-01-01")), end=decimal_date(ymd("2001-01-01")))
#stl(training423, "periodic")
trainingcomponent <- decompose(training423)
plot(trainingcomponent)


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


