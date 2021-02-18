---
title: "Reproducible Research - Project 1"
output:
  html_document: 
    keep_md: yes
  pdf_document: default
---

## Loading and preprossing the data  
Open tools

```r
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
library("lubridate", "dplyr", "lattice")
```
  
Download, Unzip and Read the data

```r
setwd("~/Downloads")
file <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(file, "Activity_monitoring.zip", method = "curl")
unzip("activity_monitoring.zip")
activity <- read.table("activity.csv", header = TRUE, sep =",")
```


##  What is the mean total number of steps taken per day?  
1. Calculate the total number of steps taken per day

```r
#change date from character to date class
activity$date <- as.Date(activity$date, "%Y-%m-%d")
#Remove NA values for this section per instructions
activity2 <- activity[complete.cases(activity),]
#aggregate df by the sum of steps by date & print result
totalsteps <- aggregate(steps ~ date, activity2, sum)
print(totalsteps)
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```
  
2. Make a histogram of the total number of steps taken per day

```r
hist(totalsteps$steps, main = "Total Number of Steps taken per Day", xlab = "Total number of steps in a day", col = 4)
```

![](PA1_template_files/figure-html/histogram1-1.png)<!-- -->
  
3. Calculate the mean and median of the total number of steps taken per day

```r
mean(totalsteps$steps)
```

```
## [1] 10766.19
```

```r
median(totalsteps$steps)
```

```
## [1] 10765
```
  
## What is the average daily activity pattern?  
1. Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days

```r
#aggregate average steps per interval
totalsteps2 <- aggregate(steps ~ interval, activity2, mean)
plot(totalsteps2$interval, totalsteps2$steps, type = "l", xlab = "Interval", ylab = "Average Number of Steps", main = "Average Number of Steps by Interval")
```

![](PA1_template_files/figure-html/time_series_plot-1.png)<!-- -->
  
2. Which 5-minute interval, on average across all the days in the dataset contains the maximum number of steps?

```r
#prints which row has max steps
max_interval <- which.max(totalsteps2$steps)
#subset df to print which interval has max number of steps
totalsteps2[max_interval,]
```

```
##     interval    steps
## 104      835 206.1698
```
  
This notes that interval *835* has the highest average number of steps, which is approximately *206* steps.

## Imputing Missing Data  
Missing days may introduce bias into some calculations or summaries of the data.
  
1. Calculate and report the total number of missing values in the dataset.

```r
sum(is.na(activity))
```

```
## [1] 2304
```
There are *2304* rows will missing (NA) values.
  
2. Devise a strategy for filling in the missing values.
For this exercise, we will replace any NA values with the mean in the 5 minute interval


```r
#calculate average steps per interval
steps_average <- aggregate(steps~interval, data = activity, FUN = mean)
#create container
steps_full <- as.numeric()
#create vector with total number of rows in activity
x <- nrow(activity)
#
for(i in 1:x){
  y <- activity[i,]
  #if steps is NA, put in the average for the interval
  if (is.na(y$steps)){
    steps <- subset(steps_average, interval == y$interval)$steps
  #otherwise, just fill in steps as is
  } else{
    steps <- y$steps
  }
  steps_full <- c(steps_full,steps)
}
```
  
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
#make a copy of original df
activity_full <- activity
#replace original steps column with filled steps column
activity_full$steps <- steps_full
head(activity_full)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```
  
4. Make a histogram of the total number of steps taken each day. 

```r
totalsteps_full <- aggregate(steps~date, data = activity_full, sum, na.rm = TRUE)
hist(totalsteps_full$steps, main = "Total Number of Steps taken per Day", xlab = "Total number of steps in a day", col = 6)
```

![](PA1_template_files/figure-html/histogram2-1.png)<!-- -->
  
Calculate and report the mean and median total number of steps taken per day.

```r
#mean of dataframe without NAs
mean(totalsteps_full$steps)
```

```
## [1] 10766.19
```

```r
#mean of original dataframe
mean(totalsteps$steps)
```

```
## [1] 10766.19
```

```r
#median of dataframe without NAs
median(totalsteps_full$steps)
```

```
## [1] 10766.19
```

```r
#median of original dataframe
median(totalsteps$steps)
```

```
## [1] 10765
```
  
The mean and median of both data frames are similar; only the median showed a slight increase in the data frame without NAs.

## Are there differences in activity patterns between weekdays and weekends?
Use the dataset with filled-in missing values.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating wither a given date is a weekday or weekend day

```r
#create a new column with name of day
day <- weekdays(activity_full$date)

#create container
week_type <- vector()
#create vector with total number of rows in activity
a <- nrow(activity_full)
#
for(i in 1:a){
  #if day = Saturday fill in "Weekend"
  if (day[i] == "Saturday"){
    week_type[i] <- "Weekend"
 #if day = Sunday fill in "Weekend"
  } 
  else if (day[i] == "Sunday"){ 
    week_type[i] <- "Weekend"
  } 
  #otherwise, fill in "Weekday"
  else{
    week_type[i] <- "Weekday"
  }
}
activity_full$week_type <- week_type
head(activity_full)
```

```
##       steps       date interval week_type
## 1 1.7169811 2012-10-01        0   Weekday
## 2 0.3396226 2012-10-01        5   Weekday
## 3 0.1320755 2012-10-01       10   Weekday
## 4 0.1509434 2012-10-01       15   Weekday
## 5 0.0754717 2012-10-01       20   Weekday
## 6 2.0943396 2012-10-01       25   Weekday
```
  
2. Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days. 

```r
#aggregate average steps per weekday
library(lattice)
activity_full$week_type <- as.factor(activity_full$week_type)
final <- aggregate(steps ~ interval + week_type, data = activity_full, mean)
xyplot(steps~interval|week_type, final, type = "l", xlab = "Interval", ylab = "Average Number of Steps", main = "Number of Steps by Day Type", layout = c(1,2) )
```

![](PA1_template_files/figure-html/Week_intervals-1.png)<!-- -->
