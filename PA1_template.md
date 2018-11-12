Load the data
-------------

Set working directory and download and unzip the file from the internet.

    # Set working directory
    setwd("~/Documents/BGS_admin/Training/Data_science/Module_5_Reproducible Research/Week_2_not_completed/Assignment")

    # Download the file from the internet
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "activity.zip", mode="wb")

    # Unzip the file
    unzip("activity.zip")
    Steps <- read.csv("activity.csv", header = TRUE)

Summarise the data
------------------

Summarise the step data.

    # Summarise the data
    head(Steps)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

    summary(Steps)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

    str(Steps)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

What is mean total number of steps taken per day?
-------------------------------------------------

The total number of steps taken per day is shown below.

    # Aggregate the steps taken by day and then sum to calculate the total steps taken per day
    Steps_Day <- aggregate(Steps$steps, by=list(Steps$date), sum)
    colnames(Steps_Day) <- c("Day", "Total_steps")

    # Display the result
    head(Steps_Day)

    ##          Day Total_steps
    ## 1 2012-10-01          NA
    ## 2 2012-10-02         126
    ## 3 2012-10-03       11352
    ## 4 2012-10-04       12116
    ## 5 2012-10-05       13294
    ## 6 2012-10-06       15420

Figure 1 shows a histogram of the total steps taken per day.

    hist(Steps_Day$Total_steps, xlab = "Steps", breaks = 20, main="Total number of steps per day", col = c("blue"))

![](PA1_template_files/figure-markdown_strict/steps%20histogram-1.png)

The mean and median of the total number of steps taken per day is shown
below.

    # Calculate the mean
    mean(Steps_Day$Total_steps, na.rm=TRUE)

    ## [1] 10766.19

    # Calculate the median
    median(Steps_Day$Total_steps, na.rm=TRUE)

    ## [1] 10765

What is the average daily activity pattern?
-------------------------------------------

Figure 2 shows a time series plot of the average number of steps across
all days plotted against the whole day split into 5 minute intervals.

    # Create a new matrix with mean values for each 5 minute interval, in order to do so remove any 5 minute interval without data.
    NA_ind <- is.na(Steps$steps)
    Steps_nNA <- Steps[!NA_ind,]
    Steps_int <- aggregate(Steps_nNA$steps, by=list(Steps_nNA$interval), mean)
    colnames(Steps_int) <- c("Interval", "Steps")

    # Plot the average number of steps across the days per 5 minute interval
    plot(Steps_int$Interval, Steps_int$Steps, type = "l", xlab = "Five minute interval", ylab = "Average steps", main="Average steps per 5 minute interval")

![](PA1_template_files/figure-markdown_strict/steps%20timeseries-1.png)

The 5 minute interval during the day, which on average, has the maximum
number of steps is shown below.

    Steps_int_max <- Steps_int[which.max(Steps_int$Steps),]
    Steps_int_max

    ##     Interval    Steps
    ## 104      835 206.1698

Imputing missing values
-----------------------

The number of missing values from the database are shown below.

    No_vals <- sum(NA_ind)
    No_vals

    ## [1] 2304

The missing values are replaced in the original dataset with the mean of
each 5 minute interval calculated using the remainder of the dataset.

    # Construct a function to replace the missing values
    replace_with_mean <- function(num) replace(num, is.na(num), mean(num, na.rm = TRUE))

    #Replace the missing values
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    Steps_replaced <- (Steps %>% group_by(interval) %>% mutate(steps = replace_with_mean(steps)))

    # Display the new dataset
    head(Steps_replaced)

    ## # A tibble: 6 x 3
    ## # Groups:   interval [6]
    ##    steps date       interval
    ##    <dbl> <fct>         <int>
    ## 1 1.72   2012-10-01        0
    ## 2 0.340  2012-10-01        5
    ## 3 0.132  2012-10-01       10
    ## 4 0.151  2012-10-01       15
    ## 5 0.0755 2012-10-01       20
    ## 6 2.09   2012-10-01       25

    # Check that no missing values remain.
    sum(is.na(Steps_replaced))

    ## [1] 0

The new dataset is summarised below.

    # Convert the new dataset to a dataframe
    Steps_replaced <- as.data.frame(Steps_replaced)

    # Summarise the new dataframe
    head(Steps_replaced)

    ##       steps       date interval
    ## 1 1.7169811 2012-10-01        0
    ## 2 0.3396226 2012-10-01        5
    ## 3 0.1320755 2012-10-01       10
    ## 4 0.1509434 2012-10-01       15
    ## 5 0.0754717 2012-10-01       20
    ## 6 2.0943396 2012-10-01       25

    summary(Steps_replaced)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 27.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##                   (Other)   :15840

    str(Steps_replaced)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

The mean and median of the total number of steps taken per day is shown
below.

    # Aggregate the steps taken by day and then sum to calculate the total steps taken per day
    Steps_Day_rep <- aggregate(Steps_replaced$steps, by=list(Steps_replaced$date), sum)
    colnames(Steps_Day_rep) <- c("Day", "Total_steps")

    # Calculate the mean
    mean(Steps_Day_rep$Total_steps)

    ## [1] 10766.19

    # Calculate the median
    median(Steps_Day_rep$Total_steps)

    ## [1] 10766.19

Replacing the missing values has increased the median by 1.19, but the
mean remains the same.

Figure 3 shows a histogram of the total steps taken per day but using
the new dataset (red) compared to the old dataset (blue).

    par(1,2)

    ## [[1]]
    ## NULL
    ## 
    ## [[2]]
    ## NULL

    hist(Steps_Day_rep$Total_steps, xlab = "Steps", breaks = 20, main="Total number of steps per day", col = c("red"))

![](PA1_template_files/figure-markdown_strict/steps%20histogram%20new%20dataset-1.png)

    hist(Steps_Day$Total_steps, xlab = "Steps", breaks = 20, main="Total number of steps per day", col = c("blue"))

![](PA1_template_files/figure-markdown_strict/steps%20histogram%20new%20dataset-2.png)

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

The new dataset is split up into weekends and weekdays and activity
levels are compared in Figure 4.

    # Create new dataframe wth weekday and weekend variables
    Steps_replaced$date <- as.Date(Steps_replaced$date)
    Steps_replaced$weekday <- weekdays(Steps_replaced$date)
    Steps_replaced$weekend <- ifelse(Steps_replaced$weekday=="Saturday" | Steps_replaced$weekday=="Sunday", "Weekend", "Weekday" )

    # Plot the difference between activity patterns on weekdays and weekends.
    library(ggplot2)
    Week_period <- aggregate(Steps_replaced$steps , by= list(Steps_replaced$weekend, Steps_replaced$interval), na.omit(mean))
    names(Week_period) <- c("weekend", "interval", "steps")

    ggplot(Week_period, aes(x=interval, y=steps, color=weekend)) + geom_line()+
        facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
        ggtitle("Comparison of Average Number of Steps in Each Interval") + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

![](PA1_template_files/figure-markdown_strict/steps%20timeseries%20new%20dataset-1.png)

There is a difference in activity levels between weekdays and weekends,
in particular there is a more gradual increase in activity at the
weekend, and activity levels remain more consistent.
