#1
library(dplyr)
library(ggplot2)
library(lubridate)
library(grid)
activity <- read.csv("activity.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)

activity$date <- as.Date(as.character(activity$date),"%Y-%m-%d")

#2
days <- 
  activity %>%
  group_by(date) %>%
  summarize(steps=sum(steps,na.rm=TRUE),
            steps_mean=sum(steps,na.rm=TRUE)/n())

options(scipen=999)    #removing scientific notations
ggplot(days, aes(date, steps)) + 
  geom_bar(stat="identity", fill="burlywood1",color="brown3") +
  xlab("") + 
  ylab("Steps taken") +
  ggtitle("Total number of steps taken each day")

#3
sum(days$steps,na.rm=TRUE)/nrow(days)
median(days$steps)

#4
ggplot(days, aes(date, steps_mean)) + geom_line(color="cadetblue3") +
  xlab("") + ylab("Steps taken") +
  ggtitle("Average number of steps taken each day")

#5
intervals <- 
  activity %>%
  group_by(interval) %>%
  summarize(steps=sum(steps,na.rm=TRUE),
            steps_mean=mean(steps,na.rm=TRUE)/n())

intervals[which.max(intervals$steps_mean),"interval"]

#6
length(which(is.na(activity$steps)))
length(which(is.na(activity$steps)))/nrow(activity)

#7
#Creating new dataset equal to the original dataset but with the missing data filled in
steps_fixed <-
  activity %>%
  group_by(interval) %>%
  summarize(steps_mean=sum(steps,na.rm=TRUE)/n())
activity_fixed <-
  activity %>%
  mutate(steps_fixed=ifelse(is.na(steps),steps_fixed$steps_mean,steps))

#Creating histogram of the total number of steps taken each day
days_fixed <- 
  activity_fixed %>%
  group_by(date) %>%
  summarize(steps=sum(steps_fixed,na.rm=TRUE),
            steps_mean=sum(steps_fixed,na.rm=TRUE)/n())
second <- ggplot(days_fixed, aes(date, steps)) + 
  geom_bar(stat="identity", fill="lightcoral",color="lightpink") +
  xlab("") + 
  ylab("Steps taken") +
  ggtitle("Total number of steps taken each day- NAs fixed")
multiplot(first, second)

#Calculating mean and median total number of steps taken per day in new dataset
sum(days_fixed$steps,na.rm=TRUE)/nrow(days)
median(days_fixed$steps)

#Impact of imputing missing data on the estimates of the total daily number of steps
#Mean increased 13% but median hasn`t moved.

#8
weekdays_weekends <-
  activity %>%
  mutate(day_type=ifelse(weekdays(activity$date)=="Saturday" | weekdays(activity$date)=="Sunday","Weekend","Weekday")) %>%
  group_by(day_type,interval) %>%
  summarize(steps=sum(steps,na.rm=TRUE),
            steps_mean=sum(steps,na.rm=TRUE)/n())

ggplot(weekdays_weekends, aes(factor(interval), steps, fill = factor(day_type))) + 
  geom_bar(stat="identity") +
  facet_grid(.~day_type) +
  scale_fill_hue(name="Day Type",labels=c("Weekday","Weekend")) +
  scale_x_discrete(breaks=NULL) + 
  xlab("5-minute intervals") + 
  ylab("Steps taken") +
  ggtitle("Average number of steps taken per 5-minute interval across weekdays and weekends")
dev.copy(png,filename="rep04.png",width=1000,height=1000)
dev.off()