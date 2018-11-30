#setting the working directory
setwd("~/R directory/Uber")

#loading required packages
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(ggplot2)
library(vcd)

#reading data from .csv file
Uber_Request_Data <- read_csv("~/R directory/Uber/Uber Request Data.csv")
View(Uber_Request_Data)
uber <- Uber_Request_Data
str(uber)
summary(uber)

a <- uber


#----------------------------Data Cleaning-----------------------

#converting to lower case
uber$Status<- tolower(uber$Status)
uber$`Pickup point` <- tolower(uber$`Pickup point`)

#checking and removing duplicate rows
duplicated(uber$`Request id`)
sum(duplicated(uber$`Request id`))

#making date fields consistent
uber$`Request timestamp` <- gsub("/","-", uber$`Request timestamp`)
uber$`Drop timestamp`<- gsub("/","-",uber$`Drop timestamp`)
View(uber)

#splitting columns
tmp1 <- data.frame(do.call(rbind, strsplit(as.vector(uber$`Request timestamp`), split = " ")))
names(tmp1) <- c("Request_date", "Request_time")
uber1 <- cbind(tmp1$Request_date, tmp1$Request_time, uber)
names(uber1) <- c("Request_date", "Request_time", "Request_id","Pickup_point","Driver_id","Status","Request_timestamp","Drop_timestamp")

tmp2 <- data.frame(do.call(rbind, strsplit(as.vector(uber$`Drop timestamp`), split = " ")))
names(tmp2) <- c("Drop_date", "Drop_time")
uber2 <- cbind(tmp2$Drop_date, tmp2$Drop_time, uber1)
names(uber2) <- c("Drop_date","Drop_time","Request_date", "Request_time", "Request_id","Pickup_point","Driver_id","Status","Request_timestamp","Drop_timestamp")
uber <- uber2
View(uber)

#removing extra columns
uber <- uber[,-10]
uber <- uber[,-9]
View(uber)

#converting to date format
uber$Drop_date <- as.Date(uber$Drop_date, format="%d-%m-%Y")
uber$Request_date <- as.Date(uber$Request_date, format="%d-%m-%Y")

#converting to time format
a1 <- strptime(uber$Request_time, format="%H:%M")
a1 <- sub(".* ", "", a1)
a1<-substr(a1,1,5)
uber$Request_time <- a1

a2 <- strptime(uber$Drop_time, format="%H:%M")
a2 <- sub(".* ", "", a2)
a2<-substr(a1,1,5)
uber$Drop_time <- a2

#adding fields to the dataset
#1. Time-slot 

Request_hour<-substr(uber[,4],1,2)
Request_hour<-as.numeric(Request_hour)

Request_interval<-c()

for(i in 1:length(Request_hour))
{
  if(Request_hour[i]>=4 & Request_hour[i]<=5)
  {
    Request_interval[i]<-"early morning"
  } else if(Request_hour[i]>=6 & Request_hour[i]<=11){
    Request_interval[i]<-"morning"
  } else if(Request_hour[i]>=12 & Request_hour[i]<=16){
    Request_interval[i]<-"afternoon"
  }else if(Request_hour[i]>=17 & Request_hour[i]<=20){
    Request_interval[i]<-"evening"
  }else if(Request_hour[i]>=21 & Request_hour[i]<=23){
    Request_interval[i]<-"night"
  }else {
    Request_interval[i]<-"late night"
  }
}

uber <- cbind(uber,Request_interval)
View(uber)

#2. Adding a Weekday field
uber$Request_date
uber$Request_day <- weekdays(as.Date(uber$Request_date))
uber$Drop_day <- weekdays(as.Date(uber$Drop_date))
uber$Request_day<- tolower(uber$Request_day)
uber$Drop_day<- tolower(uber$Drop_day)
View(uber)

#Writing data to .csv format
write.csv(uber, file = "Uber_data.csv")

#-----------------------Data Analysis------------------------------

str(uber)
summary(uber)

#graphs are plotted in tableau.
