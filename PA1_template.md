# Reproducible Research: Peer Assessment 1




## Loading and preprocessing the data
* Load the data (i.e. read.csv()). Process/transform the data (if necessary) into a format suitable for your analysis


```r
library(readr)
activity <- read_csv("~/R_Scripts/Programming_Examples/reproductible research/activity.csv")
```

```
## Parsed with column specification:
## cols(
##   steps = col_integer(),
##   date = col_date(format = ""),
##   interval = col_integer()
## )
```

```r
head(activity)
```

```
## # A tibble: 6 x 3
##   steps       date interval
##   <int>     <date>    <int>
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?
* For this part of the assignment, you can ignore the missing values in the dataset.
Calculate the total number of steps taken per day. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.


```r
library(ggplot2)
activity_split_date<-split(activity, activity$date)
steps<-sapply(activity_split_date,"[",1)
steps_sum<-as.data.frame(unname(sapply(steps, sum)))
colnames(steps_sum)<-c("steps_sum")
qplot(steps_sum, data=steps_sum, bins=20, xlab="total daily steps")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/mean_median-1.png)<!-- -->

* Calculate and report the mean and median of the total number of steps taken per day

```r
print ("Total daily steps mean")
```

```
## [1] "Total daily steps mean"
```

```r
mean(as.numeric(steps_sum[,1]), na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
print ("Total daily steps median")
```

```
## [1] "Total daily steps median"
```

```r
median(as.numeric(steps_sum[,1]), na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
my_mean<-function(x){
   step_mean = mean(as.numeric(x$steps), na.rm=TRUE)
}
activity_split_interval<-split(activity, activity$interval)
intervals<-as.data.frame(sapply(activity_split_interval,my_mean))
intervals$interval<-as.numeric(names(activity_split_interval))
colnames(intervals)<-c("Interval_Mean","Interval")
ggplot(data=intervals, aes(x=Interval, y=Interval_Mean, group=1)) +
  geom_line()
```

![](PA1_template_files/figure-html/daily_activity_pattern-1.png)<!-- -->

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
intervals$Interval[which(intervals$Interval_Mean==max(intervals$Interval_Mean))]
```

```
## [1] 835
```
## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs). Total Number of Rows with NA's:


```r
 length(which(is.na(activity$steps)))
```

```
## [1] 2304
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. Create a new dataset that is equal to the original dataset but with the missing data filled in.

Using mean for given interval,

```r
my_replacement<-function(x){
    if (is.na(x[1]))
    {
        x[1]<-intervals$Interval_Mean[which(intervals$Interval==as.numeric(x[3]))]
    }
    else 
    {
            x[1]<-x[1]
        }
}

activity$steps<-as.numeric(unlist(apply(activity, 1, function(x) my_replacement(x) )))
```


* Make a histogram of the total number of steps taken each day. 


```r
library(ggplot2)
activity_split_date<-split(activity, activity$date)
steps<-sapply(activity_split_date,"[",1)
steps_sum<-as.data.frame(unname(sapply(steps, sum)))
colnames(steps_sum)<-c("steps_sum")
qplot(steps_sum, data=steps_sum, bins=20, xlab="total daily steps")
```

![](PA1_template_files/figure-html/updated_plot-1.png)<!-- -->

* Calculate and report the mean and median total number of steps taken per day. 

```r
print ("Total daily steps mean with updated data ")
```

```
## [1] "Total daily steps mean with updated data "
```

```r
mean(as.numeric(steps_sum[,1]), na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
print ("Total daily steps median with updated data")
```

```
## [1] "Total daily steps median with updated data"
```

```r
median(as.numeric(steps_sum[,1]), na.rm=TRUE)
```

```
## [1] 10766.19
```
* Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

** Mean and median are matched.

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

* Create a new factor variable in the dataset with two levels ??? 'weekday' and 'weekend' indicating whether a given date is a weekday or weekend day.


```r
activity$weekday<-factor(weekdays(activity$date,abbreviate = TRUE))
activity$weekday = ifelse(activity$weekday  %in% c("Sat", "Sun"), "weekend", "weekday")
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
my_mean<-function(x){
   step_mean = mean(as.numeric(x$steps), na.rm=TRUE)
   #x<-cbind(x,step_mean)
}
activity_split_interval_day<-split(activity, list(activity$interval, activity$weekday))
intervals<-as.data.frame(sapply(activity_split_interval_day,my_mean))
intervals$intervals<-as.numeric(sapply(strsplit(names(activity_split_interval_day),"[.]"), "[",1))
intervals$weekday<-sapply(strsplit(names(activity_split_interval_day),"[.]"), "[",2)
colnames(intervals)<-c("Interval_Mean","Interval","Weekday")
intervals<-transform(intervals, Weekday=factor(Weekday))

ggplot(intervals, aes(Interval, Interval_Mean))+geom_line()+facet_grid(Weekday~.)
```

![](PA1_template_files/figure-html/weekend_weekday_daily_activity_pattern-1.png)<!-- -->
