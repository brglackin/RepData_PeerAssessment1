# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
We first change the working directory to the repository location and load our datafile.


```r
setwd("~/GitHub/RepData_PeerAssessment1")
data<-read.csv("data/activity.csv")
```

## What is mean total number of steps taken per day?
We can aggregate the data by date, getting the sum of the steps per day. For this, we ignore any blank/missing entries.


```r
totalStepsByDate<-aggregate(steps ~ date, data, sum,na.rm=T)
```
A histogram can be created from this by using the hist command as follows:


```r
hist(totalStepsByDate$steps, xlab = "Average Daily Steps Taken", ylab="Frequency", 
     main="Frequency of the Average Daily Steps Taken")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

In order to get the mean and median of this data, we can simply call the mean and median functions on the steps variable, respectively.


```r
dataMean<-mean(totalStepsByDate$steps, na.rm=T)
dataMedian<-median(totalStepsByDate$steps, na.rm=T)
dataMean;dataMedian
```

```
## [1] 10766
```

```
## [1] 10765
```
The mean number of steps per day is <b>10766</b> and the median number of steps per day is <b>10765</b>.

## What is the average daily activity pattern?
We can aggregate the steps counts by interval over all days and plot the results:


```r
meanStepsByInterval<-aggregate(steps ~ interval, data, mean,na.rm=T)
plot(meanStepsByInterval$interval,meanStepsByInterval$steps, xlab="Interval Number",ylab = "Average Steps", main="Average Steps per Interval for all Days")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

To find the interval withe the largest average steps count, we use the which.max function:


```r
largestMean<-meanStepsByInterval[which.max( meanStepsByInterval[,2] ),]
largestMean
```

```
##     interval steps
## 104      835 206.2
```

The largest average number of steps is <b>206.2</b> which occurs in interval <b>835</b>.

## Imputing missing values
We first compute the number of rows with missing values:


```r
nrow(data) - sum(complete.cases(data))
```

```
## [1] 2304
```

For each missing value, we use the average for that interval instead, applying this to a copy of the original dataset.


```r
dataNoNAs <- data
for (i in 1:nrow(dataNoNAs))
{
  if(is.na(dataNoNAs[i,1]))
  {
    dataNoNAs[i,1]<-meanStepsByInterval[meanStepsByInterval$interval == dataNoNAs[i,3],2]
  }
}
```
We confirm that we no longer have any missing values:

```r
nrow(dataNoNAs[is.na(dataNoNAs$steps),])
```

```
## [1] 0
```

We aggregate the average steps counts by date and plot a histogram. 

```r
totalStepsByDateNoNAs<-aggregate(steps ~ date, dataNoNAs, sum,na.rm=T)
hist(totalStepsByDateNoNAs$steps,xlab = "Average Daily Steps Taken", ylab="Frequency", 
     main="Frequency of the Average Daily Steps Taken")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

We calculate the mean and median of this modified data as before.

```r
mean(totalStepsByDateNoNAs$steps, na.rm=T)
```

```
## [1] 10766
```

```r
median(totalStepsByDateNoNAs$steps, na.rm=T)
```

```
## [1] 10766
```

## Are there differences in activity patterns between weekdays and weekends?
We assign a factor variable to each record indicating whether the record represents a week day or weekend day, and create a new dataframe with the extra column added to the original dataset.

```r
weekendDays <-c("Saturday", "Sunday")
dayType <-apply(dataNoNAs,1,function(row) 
  {
    ifelse(weekdays(as.Date(row[2],"%Y-%m-%d")) %in% weekendDays, "weekend","weekday")
   })
dataNoNAs<-data.frame(dataNoNAs,dayType=as.factor(dayType))
```

We aggregate the data by interval and dayType, and plot the graph with the lattice package's xyplot command.


```r
library(lattice)

aggregatedData <- aggregate(steps ~ interval+dayType, dataNoNAs, mean)
xyplot(steps~interval|dayType, aggregatedData,
   type = "l",
   main="Steps per Interval", 
   ylab="Number of Steps", xlab="Interval",
   layout=c(1,2))
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 
