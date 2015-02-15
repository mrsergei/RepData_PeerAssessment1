# Reproducible Research: Peer Assessment 1


```r
require(knitr, quietly=TRUE)
opts_chunk$set(warning=FALSE, error=FALSE, message=FALSE)
```


```r
require(dplyr, quietly=TRUE)
require(ggplot2, quietly=TRUE)
require(gridExtra, quietly=TRUE)
require(scales, quietly=TRUE)
```


## Loading and preprocessing the data

Load the data  from comma separated file `activity.csv`

```r
activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

We will create a transformed version of the original data capturing the key statistics such as total number of steps taken per day, mean  and median of the number of steps over 5 minute intervals per day.

```r
activity$date    <- as.Date(activity$date)
activity_summary <- activity %>%
                    group_by(date) %>% 
                    summarise("total"  = sum(steps), 
                              "mean"   = mean(steps), 
                              "median" = median(steps), 
                              "nas"    = sum(is.na(steps)))
head(activity_summary)
```

```
## Source: local data frame [6 x 5]
## 
##         date total     mean median nas
## 1 2012-10-01    NA       NA     NA 288
## 2 2012-10-02   126  0.43750      0   0
## 3 2012-10-03 11352 39.41667      0   0
## 4 2012-10-04 12116 42.06944      0   0
## 5 2012-10-05 13294 46.15972      0   0
## 6 2012-10-06 15420 53.54167      0   0
```


## What is mean total number of steps taken per day?

We examine the data by looking a the histogram of the total number of steps taken each day, where green vertical line shows the average (mean) number of steps taken per day. We also show the total number of steps taken each day for the duration of the series. Horizontal green line here  depicts the average(mean) number of steps taken per day also.


```r
mean_txt   <- paste0("mean = ", round(mean(activity_summary$total, na.rm=TRUE), 2))
median_txt <- paste0("median = ", round(median(activity_summary$total, na.rm=TRUE), 2))

g1 <- ggplot(data=activity_summary, aes(x=total)) +
      geom_histogram(aes(fill = ..count..)) +
      scale_fill_gradient("Count", low = "yellow", high = "red") +
      geom_vline(xintercept=mean(activity_summary$total, na.rm=TRUE), col="green", size=2) +
      annotate("text", x = 17000, y = 7.5, label = mean_txt) +
      annotate("text", x = 17000, y = 7, label = median_txt) +
      guides(fill=FALSE) +
      ggtitle("Number of steps per day") +
      xlab("Total Number of steps taken per day") +
      ylab("Number of days with the same count") +
      theme_bw()

g2 <- ggplot(data=activity_summary, aes(x=date, y=total, fill=total)) +
      geom_histogram(stat="identity") +
      geom_hline(yintercept=mean(activity_summary$total, na.rm=TRUE), col="green", size=2) +
      guides(fill=FALSE) +
      ggtitle("Total number steps taken per day") +
      xlab("Date") +
      ylab("Total number of teps") +
      theme_bw()

grid.arrange(g1, g2, ncol=2)
```

![](PA1_template_files/figure-html/Histogram of Steps per Day-1.png) 

The mean and the median of the total number of steps taken per day   

```r
mean(activity_summary$total, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(activity_summary$total, na.rm=TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

We will create daily activity pattern dataset by averaging steps taken for a given 5 minute interval across all days in the data set ignoring the days that have no recorded values of the steps.

```r
activity_daily <- activity %>%
                  group_by(interval) %>%
                  summarise("steps_ave" = mean(steps, na.rm=T))
```

Below we construct the time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis). X axes was labeled using the time in the day (rather than numeric vlaue of the interval) for easier interpretability.


```r
max_interval <- activity_daily$interval[which.max(activity_daily$steps_ave)]
max_label    <- paste0("max step interval = ", max_interval)

g <- ggplot(data=activity_daily, aes(interval, steps_ave)) +
     geom_line(col = "blue", size = 2, alpha = 0.8) +
     geom_vline(xintercept = max_interval, col="red", size = 2, alpha = 0.6) +
     annotate("text", x = 1800, y = 150, label = max_label) +
     ggtitle("Average daily activity") +
     xlab("Time, hours") +
     ylab("Average number of steps") +
     scale_x_continuous(breaks= c(0, 600, 1200, 1800, 2355), labels=c(0, 6, 12, 18, 24)) +
     theme_bw()
print(g)
```

<img src="PA1_template_files/figure-html/average daily activity plot-1.png" title="" alt="" style="display: block; margin: auto;" />

The following 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps:


```r
activity_daily$interval[which.max(activity_daily$steps_ave)]
```

```
## [1] 835
```
NOTE: This correspnds to `8:35` in the morning.

## Imputing missing values

Total number of missing values in the data set is `sum(is.na(activity$steps)) = 2304`. Total number of 5 minute intervals per day is `24*60/5 = 288`. From activity summary statistics we determine that we are missing data for `length(which(activity_summary$nas==24*60/5)) = 8` days:


```r
activity_summary[activity_summary$nas > 0,]
```

```
## Source: local data frame [8 x 5]
## 
##         date total mean median nas
## 1 2012-10-01    NA   NA     NA 288
## 2 2012-10-08    NA   NA     NA 288
## 3 2012-11-01    NA   NA     NA 288
## 4 2012-11-04    NA   NA     NA 288
## 5 2012-11-09    NA   NA     NA 288
## 6 2012-11-10    NA   NA     NA 288
## 7 2012-11-14    NA   NA     NA 288
## 8 2012-11-30    NA   NA     NA 288
```

For days that are missing measurement of the number of steps we will use the previous day values as the imputed value except for the day one where day 2 values will be used:


```r
act_imputed      <- activity
act_imputed$date <- as.factor(act_imputed$date)
act_list         <- split(act_imputed, act_imputed$date)

na_days <- which(activity_summary$nas == 24*60/5)
nmax <- length(act_list)

for(i in na_days) {
    if(i == 1) {
        act_list[[i]]$steps <- act_list[[i+1]]$steps
    } else {
        act_list[[i]]$steps <- act_list[[i-1]]$steps
    }
}

act_imputed      <- unsplit(act_list, act_imputed$date)
act_imputed$date <- as.Date(act_imputed$date)

act_imp_sum <- act_imputed %>%
               group_by(date) %>% 
               summarise("total"  = sum(steps))
```

Let's look at the histogram of the daily total number of steps and the total number of step taken per day for the duration of the series for the new dataset with imputed values. Purple line hre represents average(mean) number of steps taken per day. 


```r
imp_mean_txt <- paste0("mean = ", round(mean(act_imp_sum$total), 2))
imp_median_txt <- paste0("median = ", round(median(act_imp_sum$total), 2))

g1 <- ggplot(data=act_imp_sum, aes(x=total)) +
      geom_histogram(aes(fill = ..count..)) +
      scale_fill_gradient("Count", low = "yellow", high = "red") +
      geom_vline(xintercept=mean(act_imp_sum$total, na.rm=TRUE), col="purple", size=2) +
      annotate("text", x = 17000, y = 7.5, label = imp_mean_txt) +
      annotate("text", x = 17000, y = 7, label = imp_median_txt) +
      guides(fill=FALSE) +
      ggtitle("Number of steps per day") +
      xlab("Total Number of steps taken per day") +
      ylab("Number of days with the same count") +
      theme_bw()

g2 <- ggplot(data=act_imp_sum, aes(x=date, y=total, fill=total)) +
      geom_histogram(stat="identity") +
      geom_hline(yintercept=mean(act_imp_sum$total, na.rm=TRUE), col="purple", size=2) +
      guides(fill=FALSE) +
      ggtitle("Total number steps taken per day") +
      xlab("Date") +
      ylab("Total number of teps") +
      theme_bw()

grid.arrange(g1, g2, ncol=2)
```

![](PA1_template_files/figure-html/daily average activity for imputed data set-1.png) 

Calculate and report the mean and median total number of steps taken per day  for the data set with imputed values.

```r
mean(act_imp_sum$total)
```

```
## [1] 10304.18
```

```r
median(act_imp_sum$total)
```

```
## [1] 10571
```
Please note that the new values for mean and median differ from the ones calculated previously. The new values for mean and median are **lower** for the imputed data set than mean and median for the original data set with missing values.

## Are there differences in activity patterns between weekdays and weekends?

We sart analysis of the difference between weekday and weekend activity patterns by introducing a factor variable into the imputed dataset to tag days that correspond to weekend vs weekday.


```r
act_imputed$daytype <- "weekday"
act_imputed$daytype[weekdays(act_imputed$date) %in% c("Saturday", "Sunday")] <- "weekend"
act_imputed$daytype <- as.factor(act_imputed$daytype)

act_wkd <- act_imputed %>% 
           group_by(daytype, interval) %>%
           summarize(steps_ave = mean(steps))

str(act_wkd)
```

```
## Classes 'grouped_df', 'tbl_df', 'tbl' and 'data.frame':	576 obs. of  3 variables:
##  $ daytype  : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ steps_ave: num  2.0222 0.4 0.1556 0.1778 0.0889 ...
##  - attr(*, "vars")=List of 1
##   ..$ : symbol daytype
##  - attr(*, "drop")= logi TRUE
```

Here you can see noticeable difference in the average weekday patters vs weekend pattern. Weekend activity pattern has notable higher activity in the afternoon. While majority of activity in weekday pattern is around morning time only (before typical work hours) and some in the evening (after typical work hours)


```r
g <- ggplot(act_wkd, aes(interval, steps_ave)) + 
     geom_line(colour = "blue", size = 2, alpha = 0.8) +
     facet_grid(daytype ~ .) +
     ggtitle("Average daily activity") +
     xlab("Time, hours") +
     ylab("Average number of steps") +
     scale_x_continuous(breaks= c(0, 600, 1200, 1800, 2355), labels=c(0, 6, 12, 18, 24)) +
     theme_bw()
print(g)
```

<img src="PA1_template_files/figure-html/Weekend and weekday average activity-1.png" title="" alt="" style="display: block; margin: auto;" />
