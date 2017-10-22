# Reproducible Research: Peer Assessment 1

## Introduction
It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a
[Fitbit](http://www.fitbit.com), [Nike
Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or
[Jawbone Up](https://jawbone.com/up). These type of devices are part of
the "quantified self" movement -- a group of enthusiasts who take
measurements about themselves regularly to improve their health, to
find patterns in their behavior, or because they are tech geeks. But
these data remain under-utilized both because the raw data are hard to
obtain and there is a lack of statistical methods and software for
processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012
and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web
site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this
dataset.

## Loading and preprocessing the data
At the beginning we need to import the ggplot2 library:

```r
library(ggplot2)
```

The first thing that we need to do is to read the data and preprocess/transform the data into a format suitable for analysis.

```r
activity <- read.csv("activity.csv", header=TRUE)
activity$date <-as.Date(activity$date, "%Y-%m-%d")
missing <- activity[is.na(activity$steps),]
```
There are some missing values:

```r
dim(missing)
```

```
## [1] 2304    3
```

## What is mean total number of steps taken per day?
Next we need to make a histogram of the total number of steps taken each day

```r
steps_sum <- aggregate(steps~date, activity, sum)
ggplot(data=steps_sum, aes(steps_sum$date, steps_sum$steps)) + 
  geom_histogram(stat = "identity", bins = nlevels(steps_sum$date)) +
  labs(title = "Total number of steps taken each day", x = "Day", y = "Total number of steps")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

And calculate and report the **mean** and **median** total number of steps taken per day

```r
summary(steps_sum$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

## What is the average daily activity pattern?
The next step is to make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
intervals <- split(activity$steps, activity$interval)
averageStepsInterval <- sapply(intervals, mean, na.rm=TRUE)
uniqueIntervals <- unique(activity$interval)
maxInterval <- max(averageStepsInterval, na.rm=TRUE)
maxIndex <- as.numeric(which(averageStepsInterval == maxInterval))
plot(uniqueIntervals, averageStepsInterval, type="l",
     main="Average number of steps per interval", 
     xlab="Interval", ylab="Number of steps")
points(uniqueIntervals[maxIndex], averageStepsInterval[maxIndex], col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

The 5-minute interval, that on average across all the days in the dataset, contains the maximum number of steps is:

```r
averageStepsInterval[maxIndex]
```

```
##      835 
## 206.1698
```

## Imputing missing values
There are a number of days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce bias into some calculations or summaries of the data.

At the beginning we need to calculate the total number of missing values in the dataset:

```r
length(missing$steps)
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. We will use the averaged number of steps from all days.

```r
activityNoMissing <- activity[!is.na(activity$steps),]
mean_steps <- with(activityNoMissing, tapply(steps, activityNoMissing$interval, mean))
missing$steps <- mean_steps
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
active <- rbind(activityNoMissing, missing)
active <- active[order(active$date), ]
```

And at the end we need to make a histogram of the total number of steps taken each day and calculate and report the **mean** and **median** total number of steps taken per day. 

```r
total_steps <-aggregate(steps~date, active, sum)
ggplot(data=total_steps, aes(total_steps$date, total_steps$steps)) + 
  geom_histogram(stat = "identity", bins = nlevels(total_steps$date)) +
  labs(title = "Total number of steps taken each day", x = "Day", y = "Total number of steps")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

And calculate and report the **mean** and **median** total number of steps taken per day

```r
summary(total_steps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

Mean and median total number of steps taken per day for the filled in missing values differ from these of the origional dataset.

## Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day

```r
active$days <- weekdays(active$date)
weekend <- grep("sobota|niedziela", active$days, ignore.case = T)
weekend_active <-  active[weekend, ]
weekend_active$weekday <- "weekend"
weekday_active <- subset(active, active$days!=weekend)
```

```
## Warning in active$days != weekend: długość dłuszego obiektu nie jest
## wielokrotnością długości krótszego obiektu
```

```r
weekday_active$weekday <- "weekday"
activ <- rbind(weekday_active, weekend_active)
```

Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```r
mean_number_steps <- aggregate(steps~ interval+weekday, activ, mean)
g <- qplot(interval, steps, data = mean_number_steps, facets = weekday~., geom = "line")
g + geom_line() + xlab("Interval") + ylab("Number of steps") + ggtitle("Average number of steps on weekdays and weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->


