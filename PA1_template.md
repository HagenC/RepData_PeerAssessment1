
#Assignment: Course Project 1, Christian Hagen



```r
library(lattice)
```

##Loading data


```r
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}

Activity <- read.csv("activity.csv")
```

##What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1.Calculate the total number of steps taken per day

```r
StepsPerDay <- aggregate(steps ~ date, Activity, sum)
```

2.If you do not understand the difference between a histogram and a barplot, research the difference between them. 
Make a histogram of the total number of steps taken each day


```r
hist(StepsPerDay$steps, xlab="Steps", main ="Histogram of steps/day"  )
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

3. Calculate and report the mean and median of the total number of steps taken per day


```r
hist(StepsPerDay$steps, xlab="Steps", main = "Histogram of steps/day")
Mean <- mean(StepsPerDay$steps)
Median <- median(StepsPerDay$steps)
Mean
```

```
## [1] 10766.19
```

```r
Median
```

```
## [1] 10765
```

```r
legend("topright", legend=c("mean=10766" , "median=10765"))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

##What is the average daily activity pattern?

1.Make a time series plot (i.e. type = #l#) of the 5-minute interval (x-axis) and the average number of steps taken, 
averaged across all days (y-axis)


```r
MeanstepsPerIntervalDay <- aggregate(steps~interval, Activity, mean, na.rm=TRUE)
plot(MeanstepsPerIntervalDay$interval,MeanstepsPerIntervalDay$steps, type = "l", xlab = "Interval", ylab = "Mean steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)


2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
MaxInterval <- MeanstepsPerIntervalDay[which.max(MeanstepsPerIntervalDay$steps)  , ]
MaxInterval               
```

```
##     interval    steps
## 104      835 206.1698
```

##Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias
into some calculations or summaries of the data.

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
NAs <- complete.cases(Activity)
table(NAs)
```

```
## NAs
## FALSE  TRUE 
##  2304 15264
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Decided to takr the mean of the mean of all days.

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
ActivityImputed <- Activity
MeanSteps <- aggregate(steps ~ date, ActivityImputed, mean)
ActivityImputed <- within(ActivityImputed, steps <- ifelse(is.na(steps), mean(MeanSteps$steps),  ActivityImputed$steps ))
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
TotalStepsPerDay <- aggregate(steps ~ date, ActivityImputed, sum)
mean(TotalStepsPerDay$steps)
```

```
## [1] 10766.19
```

```r
median(TotalStepsPerDay$steps)
```

```
## [1] 10766.19
```

```r
par(mfrow = c(2,1))
hist(TotalStepsPerDay$steps, xlab="Steps", main ="Imputed-data Histogram of steps/day"  )
legend("topright", legend=c("mean=10766" , "median=10765"))
hist(StepsPerDay$steps, xlab="Steps", main ="Original-data Histogram of steps/day"  )
legend("topright", legend=c("mean=10766" , "median=10765"))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

5.Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
ActivityImputed$date <- as.Date(ActivityImputed$date, "%Y-%m-%d")  
ActivityImputed$weekday <- weekdays(ActivityImputed$date)
ActivityImputedDaytype <- within(ActivityImputed,  daytype <- ifelse(weekday == "Saturday" | weekday== "Sunday", "weekend", "weekday"))
ActivityImputedDaytype$daytype <- as.factor(ActivityImputedDaytype$daytype)
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
MeanStepsWeek <- aggregate(steps ~ interval + daytype, ActivityImputedDaytype, mean)
xyplot(steps~interval|daytype, MeanStepsWeek, layout=c(1,2), type="l", ylab= "Number of steps", xlab= "Interval")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)







