---
title: "Reproducible Research Peer Assessment 1"
output: html_document
---

##Reprodcible Research Peer Assessment 1
This is a markdown file for Peer Assessment 1 of Coursera's Reproducible Research Course.

###Loading and processing data.
```{r}
 setwd("C:/Users/jcranford/Documents/Coursera/Reproducible_Research/Peer_Assessment_1")
data <- read.csv("activity.csv")
```
Removing NA values.
```{r}
cleaned.data <- na.omit(data)
```
###What are the total number of steps taken each day?
Total number of steps taken each day with histogram are below.
```{r}
steps.sum <- aggregate(cleaned.data$steps, by=list(cleaned.data$date), FUN=sum)
names(steps.sum) <- c("Date", "Count")
hist1<-hist(steps.sum$Count, col="red", xlab="Step Count", main="Histogram of Step Count by Day")
print(hist1)
```
Mean steps per day
```{r}
mean(steps.sum$Count)
```
Median steps per day
```{r}
median(steps.sum$Count)
```
###What is the average daily activity pattern?
Time series plot of 5-miute interval and average number of steps taken across all days.
```{r}
rm(steps.sum)
average.steps<- aggregate(cleaned.data$steps, by=list(cleaned.data$interval), FUN=mean, na.rm=TRUE)
names(average.steps)<- c("interval", "average")
plot(average.steps$interval, average.steps$average, type="l", col="red", xlab="interval", ylab="average count of steps", main="Average Count of Steps per Interval" )
```
Which interval, on average across all days in the dataset, contains the maximum number of steps?
```{r}
average.steps[average.steps$average==max(average.steps$average),]
```
###Imputing Missing Values
Total number of missing values in the dataset
```{r}
incomplete.cases <-sum(is.na(data$steps))
print(incomplete.cases)
```
Filling in all missing values in the dataset
```{r}
new.data<- data 
for(i in 1:nrow(new.data)){if (is.na(new.data$steps[i])){new.data$steps[i]<- average.steps[which(new.data$interval[i]==average.steps$interval),]$average}}
```
Test to ensure all missing values have been filled
```{r}
incomplete.new.cases <-sum(is.na(new.data$steps))
print(incomplete.new.cases)
```
Histogram of new data
```{r}
new.steps.sum<- aggregate(new.data$steps, by=list(new.data$date), FUN=sum)
names(new.steps.sum) <- c("Date", "Count")
hist2 <- hist(new.steps.sum$Count, col="red", xlab="Step Count", main="Histogram of Step Count by Day")
print(hist2)
```
Mean steps of new data
```{r}
mean(new.steps.sum$Count)
```
Median steps of new data
```{r}
median(new.steps.sum$Count)
```
After comparing it seems that the new and old mean are equal, while the new median is greater than the old median.

###Are there differences in activity patterns between weekdays and weekends?
New factor variable for weekdays and weekends
```{r}
weekdays.steps <- function(cleaned.data) {
  weekdays.steps<- aggregate(cleaned.data$steps, by=list(interval = cleaned.data$interval), FUN=mean, na.rm=TRUE)
weekdays.steps$interval <- as.interger(levels(weekdays.steps$interval)[weekdays.steps$interval])
colnames(weekdays.steps) <- c("interval", "count")
weekday.steps
}

