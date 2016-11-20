Step 1: Code to import "activity.csv" dataset. Then the str, summary,
head, tail and nrow functions were used to checkthat the import was
successful.

Included in this step is the use of the lubridate package to convert the
date variable from a FACTOR data to a Date variable. This new variable
is called DTE.

Included in this step is the creation of a weekday variable using the
wday function of lubridate package, this variable is called weekday.

Missing data has not been modified.

    library(lubridate)
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'
    ## 
    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     intersect, setdiff, union
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     filter
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(ggplot2)
    setwd("E:/RCourses/ReproducibleResearch/Project1")
    stepdata <- read.csv("activity.csv")
    summary(stepdata)

    ##      steps                date          interval     
    ##  Min.   :  0.00   1/10/2012 :  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   1/11/2012 :  288   1st Qu.: 588.8  
    ##  Median :  0.00   10/10/2012:  288   Median :1177.5  
    ##  Mean   : 37.38   10/11/2012:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   11/10/2012:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   11/11/2012:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

    head(stepdata)

    ##   steps      date interval
    ## 1    NA 1/10/2012        0
    ## 2    NA 1/10/2012        5
    ## 3    NA 1/10/2012       10
    ## 4    NA 1/10/2012       15
    ## 5    NA 1/10/2012       20
    ## 6    NA 1/10/2012       25

    str(stepdata)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "1/10/2012","1/11/2012",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

    stepdata$DTE <- dmy(stepdata$date)
    stepdata$day <- wday(stepdata$DTE, label = TRUE) 
    stepdata$weekday <- ifelse(stepdata$day == "Sun" | stepdata$day == "Sat", 0,1)
    table(stepdata$weekday)

    ## 
    ##     0     1 
    ##  4608 12960

    summary(stepdata)

    ##      steps                date          interval           DTE            
    ##  Min.   :  0.00   1/10/2012 :  288   Min.   :   0.0   Min.   :2012-10-01  
    ##  1st Qu.:  0.00   1/11/2012 :  288   1st Qu.: 588.8   1st Qu.:2012-10-16  
    ##  Median :  0.00   10/10/2012:  288   Median :1177.5   Median :2012-10-31  
    ##  Mean   : 37.38   10/11/2012:  288   Mean   :1177.5   Mean   :2012-10-31  
    ##  3rd Qu.: 12.00   11/10/2012:  288   3rd Qu.:1766.2   3rd Qu.:2012-11-15  
    ##  Max.   :806.00   11/11/2012:  288   Max.   :2355.0   Max.   :2012-11-30  
    ##  NA's   :2304     (Other)   :15840                                        
    ##     day          weekday      
    ##  Sun  :2304   Min.   :0.0000  
    ##  Mon  :2592   1st Qu.:0.0000  
    ##  Tues :2592   Median :1.0000  
    ##  Wed  :2592   Mean   :0.7377  
    ##  Thurs:2592   3rd Qu.:1.0000  
    ##  Fri  :2592   Max.   :1.0000  
    ##  Sat  :2304

    head(stepdata)

    ##   steps      date interval        DTE day weekday
    ## 1    NA 1/10/2012        0 2012-10-01 Mon       1
    ## 2    NA 1/10/2012        5 2012-10-01 Mon       1
    ## 3    NA 1/10/2012       10 2012-10-01 Mon       1
    ## 4    NA 1/10/2012       15 2012-10-01 Mon       1
    ## 5    NA 1/10/2012       20 2012-10-01 Mon       1
    ## 6    NA 1/10/2012       25 2012-10-01 Mon       1

    tail(stepdata)

    ##       steps       date interval        DTE day weekday
    ## 17563    NA 30/11/2012     2330 2012-11-30 Fri       1
    ## 17564    NA 30/11/2012     2335 2012-11-30 Fri       1
    ## 17565    NA 30/11/2012     2340 2012-11-30 Fri       1
    ## 17566    NA 30/11/2012     2345 2012-11-30 Fri       1
    ## 17567    NA 30/11/2012     2350 2012-11-30 Fri       1
    ## 17568    NA 30/11/2012     2355 2012-11-30 Fri       1

    nrow(stepdata)

    ## [1] 17568

Step 2: The objective for this section is to generate a histogram of the
total number of steps taken per day. All cases with missing values were
removed.This refined version of stepdata was called stepdata1 The
stepdata1 dataset was summarised using dpylr package to result in a
dataset group by date with total daily steps (variable called
stepsperdayRaw) for each date. The summarisedataset is called
totalstepsperdayRaw. Histogram is then generated from this dataset.

    stepdata1 <- stepdata[complete.cases(stepdata),]
    nrow(stepdata1)

    ## [1] 15264

    groupbyday <- group_by(stepdata1,DTE)
    totalstepsperdayRaw <- summarise(groupbyday, stepsperdayRaw = sum(steps))
    totalstepsperdayRaw

    ## Source: local data frame [53 x 2]
    ## 
    ##           DTE stepsperdayRaw
    ## 1  2012-10-02            126
    ## 2  2012-10-03          11352
    ## 3  2012-10-04          12116
    ## 4  2012-10-05          13294
    ## 5  2012-10-06          15420
    ## 6  2012-10-07          11015
    ## 7  2012-10-09          12811
    ## 8  2012-10-10           9900
    ## 9  2012-10-11          10304
    ## 10 2012-10-12          17382
    ## ..        ...            ...

    nrow(totalstepsperdayRaw)

    ## [1] 53

    hist(totalstepsperdayRaw$stepsperdayRaw, 
         main="Histogram of Number of Steps Taken Per Day",
         las =1, 
         xlab="Total No daily Steps")

![](./PA1_template_files/figure-markdown_strict/Histogram%20Steps%20Raw%20Data-1.png)

Step 3: The mean and median number of steps taken per day was determined
using the summarise function of dplyr using data without missing data.

         meanstepsperdayRaw <- summarise(totalstepsperdayRaw, meansteps1 = mean(stepsperdayRaw))
         meanstepsperdayRaw

    ## Source: local data frame [1 x 1]
    ## 
    ##   meansteps1
    ## 1   10766.19

         medianstepsperdayRaw <- summarise(totalstepsperdayRaw, mediansteps1 = median(stepsperdayRaw))
         medianstepsperdayRaw

    ## Source: local data frame [1 x 1]
    ## 
    ##   mediansteps1
    ## 1        10765

Step 4: The time series plot of the 5 minute intervals and the average
number of steps taken for each 5-minute interval.

    groupbyinterval <- group_by(stepdata1,interval)
    avgstepsperintervalRaw <- summarise(groupbyinterval, avgstepsperintRaw = mean(steps))
    summary(avgstepsperintervalRaw)

    ##     interval      avgstepsperintRaw
    ##  Min.   :   0.0   Min.   :  0.000  
    ##  1st Qu.: 588.8   1st Qu.:  2.486  
    ##  Median :1177.5   Median : 34.113  
    ##  Mean   :1177.5   Mean   : 37.383  
    ##  3rd Qu.:1766.2   3rd Qu.: 52.835  
    ##  Max.   :2355.0   Max.   :206.170

    stepplot <- ggplot(data = avgstepsperintervalRaw, aes(interval,avgstepsperintRaw))+
            geom_line()+
            ggtitle("Average Number Steps per 5 Min Interval")+
            xlab("5 Minute Interval") +
            ylab("Avg Steps Per 5 minute Interval")
    stepplot

![](./PA1_template_files/figure-markdown_strict/Time%20Series%20Plot%20Raw-1.png)

Step 5: The arrange function of dplyr was used to sort dataset by
avgstepsperRaw in descending order, then use head function to determine
the interval which has the highest average number of steps.

    sortedbystepsRaw <- arrange(avgstepsperintervalRaw,desc(avgstepsperintRaw))
    maxstepint <- head(sortedbystepsRaw,1)
    maxstepint$interval

    ## [1] 835

    maxstepint$avgstepsperintRaw

    ## [1] 206.1698

Step 6: Imputing missing step data for a 5 minute interval is done by
replacing each missing step datum with the average step vale for the
corresponding 5 minute interval. The raw activity.csv dataset was split
into two parts: one dataset with complete data (stepdata1) and one
dataset with missing step data (stepmissing).

Using the stepdata1 dataset, a dataset was created with the average
number of steps for each 5 minute interval (avgstepsperintervalRaw -
created at Step 4). The avgstepsperintervalRaw dataset was merged with
stepmissing dataset by interval to replace the missing value with the
corresponding intervals average number steps for that interval.

Then combine datasets: stepdata1 and stepmissing to make the final
dataset with imputed missing data: stepdataALL. A variable was created
in both stepdata1 and stepmissing datasets called "realdata" which
denoted the observation real or imputed.

    stepdata1 <- stepdata[complete.cases(stepdata),]
    stepmissing <- stepdata[!complete.cases(stepdata),]
    nrow(stepdata1)

    ## [1] 15264

    nrow(stepmissing)

    ## [1] 2304

    nrow(avgstepsperintervalRaw)

    ## [1] 288

    head(stepmissing)

    ##   steps      date interval        DTE day weekday
    ## 1    NA 1/10/2012        0 2012-10-01 Mon       1
    ## 2    NA 1/10/2012        5 2012-10-01 Mon       1
    ## 3    NA 1/10/2012       10 2012-10-01 Mon       1
    ## 4    NA 1/10/2012       15 2012-10-01 Mon       1
    ## 5    NA 1/10/2012       20 2012-10-01 Mon       1
    ## 6    NA 1/10/2012       25 2012-10-01 Mon       1

    stepmissing <- left_join(stepmissing, avgstepsperintervalRaw, by = "interval")
    stepmissing$steps <- stepmissing$avgstepsperintRaw
    stepmissing <- select(stepmissing, steps, date, interval, DTE, day, weekday)
    stepmissing$realdata <- 0
    stepdata1$realdata <- 1
    stepdataALL <- rbind(stepdata1,stepmissing)
    str(stepdataALL)

    ## 'data.frame':    17568 obs. of  7 variables:
    ##  $ steps   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ date    : Factor w/ 61 levels "1/10/2012","1/11/2012",..: 23 23 23 23 23 23 23 23 23 23 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
    ##  $ DTE     : POSIXct, format: "2012-10-02" "2012-10-02" ...
    ##  $ day     : Ord.factor w/ 7 levels "Sun"<"Mon"<"Tues"<..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ weekday : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ realdata: num  1 1 1 1 1 1 1 1 1 1 ...

Step 7: Histogram of the average steps per day is created using the same
code as used in Step 2 but using stapdataALL dataset. In addition, the
mean and median total steps per day are calculated using stepdataALL
dataset.

    groupbydayALL <- group_by(stepdataALL,DTE)
    totalstepsperdayALL <- summarise(groupbydayALL, stepsperdayALL = sum(steps))
    totalstepsperdayALL

    ## Source: local data frame [61 x 2]
    ## 
    ##           DTE stepsperdayALL
    ## 1  2012-10-01       10766.19
    ## 2  2012-10-02         126.00
    ## 3  2012-10-03       11352.00
    ## 4  2012-10-04       12116.00
    ## 5  2012-10-05       13294.00
    ## 6  2012-10-06       15420.00
    ## 7  2012-10-07       11015.00
    ## 8  2012-10-08       10766.19
    ## 9  2012-10-09       12811.00
    ## 10 2012-10-10        9900.00
    ## ..        ...            ...

    nrow(totalstepsperdayALL)

    ## [1] 61

    hist(totalstepsperdayALL$stepsperdayALL, 
         main="Histogram of Number of Steps Taken Per Day",
         las =1, 
         xlab="Total No daily Steps")

![](./PA1_template_files/figure-markdown_strict/Histogram%20Steps%20Includes%20Imputed%20Data-1.png)

    ## Calculate mean and median number of steps per day
         meanstepsperdayALL <- summarise(totalstepsperdayALL, meanstepsALL = mean(stepsperdayALL))
         meanstepsperdayALL

    ## Source: local data frame [1 x 1]
    ## 
    ##   meanstepsALL
    ## 1     10766.19

         medianstepsperdayALL <- summarise(totalstepsperdayALL, medianstepsALL = median(stepsperdayALL))
         medianstepsperdayALL

    ## Source: local data frame [1 x 1]
    ## 
    ##   medianstepsALL
    ## 1       10766.19

Step 8: Create plots of average steps per interval across weekdays and
weekends using ggplot package. Dataset created first to group by
interval and weekday variables

    groupbyintervalALL <- group_by(stepdataALL,interval,weekday)
    avgstepsperintervalALL <- summarise(groupbyintervalALL, avgstepsperintALL = mean(steps))
    avgstepsperintervalALL$weekday <- ifelse(avgstepsperintervalALL$weekday==0, "Weekend", "Weekday") 
    avgstepsperintervalALL$weekday <- factor(avgstepsperintervalALL$weekday)
    summary(avgstepsperintervalALL)

    ##     interval         weekday    avgstepsperintALL
    ##  Min.   :   0.0   Weekday:288   Min.   :  0.000  
    ##  1st Qu.: 588.8   Weekend:288   1st Qu.:  2.047  
    ##  Median :1177.5                 Median : 28.133  
    ##  Mean   :1177.5                 Mean   : 38.988  
    ##  3rd Qu.:1766.2                 3rd Qu.: 61.263  
    ##  Max.   :2355.0                 Max.   :230.378

    stepplotWeekday <- ggplot(data = avgstepsperintervalALL, aes(interval,avgstepsperintALL))+
            geom_line()+
            facet_grid(weekday~.)+
            ggtitle("Average Number Steps per 5 Min Interval for Weekdays and Weekends")+
            xlab("5 Minute Interval") +
            ylab("Avg Steps Per 5 minute Interval")
    print(stepplotWeekday)

![](./PA1_template_files/figure-markdown_strict/Plot%20of%20Steps%20Across%20Weekday%20and%20Weekend-1.png)
