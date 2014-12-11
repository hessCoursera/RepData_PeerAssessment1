# Reproducible Research: Peer Assessment 1

Introduction
===

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

## Loading and preprocessing the data
Using data from gihub rdpeng/RepData_PeerAssessment1
originalley from originally from:
* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]


```r
filename = "activity.csv"
if (!file.exists("data")) {
  unzip(zipfile="activity.zip")
}
data <- read.csv(filename)
```

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken



## What is mean total number of steps taken per day?


```r
stepsperday <- sapply(split(data$steps,data$date),sum,na.rm = TRUE)
msteps <- mean(stepsperday,na.rm = T)
mdsteps <- median(stepsperday,na.rm = T)
sdsteps <- sd(stepsperday,na.rm = T)
library('ggplot2')
qplot(stepsperday,binwidth=1000,  title="histogram of the total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png) 

The mean   total number of steps is 9354.2295082
The median total number of steps is 9354.2295082



## What is the average daily activity pattern?

```r
data$day = strftime(data$date,'%A')
#qplot(data = data,x=interval,y=steps,color=day, alpha = I(0.4))+ stat_smooth(na.rm = T)
qplot(data = data,x=interval,y=steps, alpha = I(0.2))+ stat_smooth(na.rm = T)
```

```
## geom_smooth: method="auto" and size of largest group is >=1000, so using gam with formula: y ~ s(x, bs = "cs"). Use 'method = x' to change the smoothing method.
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
# doing it by hand:
plot(data$steps ~ data$interval,type="l", col="grey")
# now calculating  and plotting mean 
stepsperinterval <- sapply(split(DT$steps,DT$interval),mean,na.rm = TRUE)
# creating a timescale
timelist = as.numeric(levels(as.factor(data$interval)))
# overplotting mean
lines(stepsperinterval ~ timelist,type="l",col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-2.png) 

```r
mostactivetime = data$interval[which.max(stepsperinterval)]
```

The most steps are recorded when the time of day is 835


## Imputing missing values
the total number of missing values in the dataset:

```r
summary(data$step)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.00   37.38   12.00  806.00    2304
```
filling in all of the missing values in the dataset, by imputing with average of that time of day:

```r
imputed_data=data
#find mask of all NA
allnans = is.na(data$steps)
# assigning daytime means to replacemens
replacements = stepsperinterval[as.factor(data$interval)]
imputed_data$steps[allnans]= replacements[allnans]
summary(imputed_data$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.00    0.00   37.38   27.00  806.00
```
now checking the imputed data:

```r
stepsperday_imputed <- sapply(split(imputed_data$steps,imputed_data$date),sum,na.rm = TRUE)
msteps_imputed <- mean(stepsperday_imputed,na.rm = T)
mdsteps_imputed <- median(stepsperday_imputed,na.rm = T)
sdsteps_imputed <- sd(stepsperday_imputed,na.rm = T)
qplot(stepsperday,binwidth=1000)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 


The mean   total number of steps was 9354.2295082 imputed 1.0766189\times 10^{4}
The median total number of steps was 9354.2295082 imputed 1.0766189\times 10^{4}
Hence there is little change, since the mean on the original data was computed neglecting the NA's and the imputation was done based on the mean.
Furthermore the NA's are only 13% of the dat.


## Are there differences in activity patterns between weekdays and weekends?

```r
data$weekend = (data$day %in% c("Sunday","Saturday"))
qplot(data = data,x=interval,y=steps,color=weekend,facets=.~ weekend, alpha = I(0.4))+ stat_smooth(na.rm = T)
```

```
## geom_smooth: method="auto" and size of largest group is >=1000, so using gam with formula: y ~ s(x, bs = "cs"). Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is >=1000, so using gam with formula: y ~ s(x, bs = "cs"). Use 'method = x' to change the smoothing method.
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

```r
## and overlay
qplot(data = data,x=interval,y=steps,color=weekend,alpha = I(0.4))+ stat_smooth(na.rm = T)
```

```
## geom_smooth: method="auto" and size of largest group is >=1000, so using gam with formula: y ~ s(x, bs = "cs"). Use 'method = x' to change the smoothing method.
```

![](PA1_template_files/figure-html/unnamed-chunk-6-2.png) 

There is a clear difference. The morining commute peak is missing at the weekens. Instead more steps are recorded in the afternoon.
