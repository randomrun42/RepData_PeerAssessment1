---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
unzip("activity.zip")
data <- read.csv("activity.csv")
data$date <- as.Date(data$date, "%Y-%m-%d")
```


## What is mean total number of steps taken per day?

```r
hist(tapply(data$steps, data$date, mean, na.rm=TRUE),
     xlab = "Steps per Day", main = "Histogram of Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean(data$steps, na.rm=TRUE)
```

```
## [1] 37.3826
```

```r
median(data$steps, na.rm=TRUE)
```

```
## [1] 0
```


## What is the average daily activity pattern?

```r
data$intervalmean <- tapply(data$steps, data$interval, mean, na.rm = TRUE)

with(data[order(data$interval),], plot(interval, intervalmean, type = "l"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
data$interval[which.max(data$intervalmean)]
```

```
## [1] 835
```


## Imputing missing values

```r
data$stepsimputed[is.na(data$steps)] <- data$intervalmean[is.na(data$steps)]
data$stepsimputed[!is.na(data$steps)] <- data$steps[!is.na(data$steps)]

hist(stepsperdayimputed <- tapply(data$stepsimputed, data$date, mean),
     xlab = "Steps per Day", main = "Histogram of Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean(data$stepsimputed)
```

```
## [1] 37.3826
```

```r
median(data$stepsimputed)
```

```
## [1] 0
```


## Are there differences in activity patterns between weekdays and weekends?

```r
data$daytype <- factor(ifelse(weekdays(data$date) == "Sunday" |
                                  weekdays(data$date) == "Saturday",
                              "weekend", "weekday"))

data$daytypeintervalmean[data$daytype=="weekday"] <-
    tapply(data$steps[data$daytype=="weekday"],
           data$interval[data$daytype=="weekday"],
           mean, na.rm = TRUE)

data$daytypeintervalmean[data$daytype=="weekend"] <-
    tapply(data$steps[data$daytype=="weekend"],
           data$interval[data$daytype=="weekend"],
           mean, na.rm = TRUE)

library(lattice)
xyplot(daytypeintervalmean ~ interval | daytype,
       data = data[order(data$interval),],
       layout = c(1, 2), type = "l", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
