Activity Monitoring Devices
---------------------------

Read the raw data from a local csv file and add to r. Then set up some
values for the code and set up the dates column.

    library(dplyr)
    library(magrittr)
    library(data.table)
    library(knitr)
    library(kableExtra)
    library(lattice)

    activity <- read.csv("C:/Users/Bryan/Desktop/DATA SCIENCE CERT/Coursera5_ReproducibleResearch/Ass2/activity.csv", header=TRUE, stringsAsFactors = FALSE)
    activity$date <- as.Date(activity$date)

    nactivity <- activity  ## Retain NA values for later calculations
    activity <- na.omit(activity)  ## Remove NA values for parts 1,2

Take the number of steps from the device, gather the total, and return a
histogram of the total steps per day. This will then be summarized as a
table showing the mean and median for the steps/day:

    options(digits = 4)
    opts_chunk$set(tidy.opts=list(width.cutoff=80),tidy=TRUE)
    tot.step <- aggregate(steps ~ date,FUN=sum,data=activity)
    tot.step$date <- as.Date(tot.step$date, format="%Y-%m-%d")
    names(tot.step) <- c("Date","TotalSteps")

    hist(tot.step$TotalSteps,    #Step histogram as Total/day
              main="Total Steps per Day",
              xlab = "Step Range", col = 3,
              breaks=20) 

![](PA1_template_files/figure-markdown_strict/total%20steps-1.png)

    # Follow the histogram with statistical summary functions:
    mean(tot.step$TotalSteps)

    ## [1] 10766

    median(tot.step$TotalSteps)

    ## [1] 10765

Create a table to examine the data array into a data frame processing
the mean steps taken for each 5-min interval in a 24 hour period. Begin
by calculating the mean steps of each day via 5-min intervals, then turn
it into a (readable) table and xy plot:

    time.step <- as.data.frame(tapply(activity$steps,activity$interval,mean,simplify=FALSE)) # mean calculation
    time.step <- cbind(rownames(time.step),time.step)
    colnames(time.step) <- c("interval", "mean.step")
    rownames(time.step) <- c()
    time.step$mean.step <- as.numeric(as.character(time.step$mean.step))
    time.step$interval <- as.numeric(as.character(time.step$interval))
    xyplot(mean.step~interval, #Generate a time-based xy plot with the interval data
           data=time.step,type = "l",
           main = "Average Steps per Day",
           xlab = "Time Interval (min)", ylab = "Mean Steps",
           scales=list(tck=c(1,0))
           ) 

![](PA1_template_files/figure-markdown_strict/steps%20per%20interval-1.png)

From this data, we can see that the max avg. steps is 206.1698, which
occurs at the interval 835.

We should now take a look at the values that have been left OUT of the
data and determine what to do with them. Let's make a new column
counting the NA values in the total data, 'nactivity'.

    nactivity$na_count <- apply(nactivity, 1, function(x) sum(is.na(x)))

We now have a new column that effectivly gives a T/F value for NAs which
reports a sum of 2304 rows of NA data out of 17568.

To fill in the NA values, we will use the mean for that five-minute
interval:

    tot.activity <- nactivity
    colnames(tot.activity)

    ## [1] "steps"    "date"     "interval" "na_count"

    for (i in 1:nrow(tot.activity)) {
          if (is.na(tot.activity$steps[i])) {
                this.step <- as.numeric(time.step$mean.step[which(time.step$interval == tot.activity$interval[i])])
                tot.activity$steps[i] <- this.step
          }
    }

    tot.step <- aggregate(steps ~ date,FUN=sum,data=tot.activity)
    tot.step$date <- as.Date(tot.step$date, format="%Y-%m-%d")
    names(tot.step) <- c("Date","TotalSteps")

    hist(tot.step$TotalSteps,    #Step histogram as Total/day
              main="Completed Total Steps per Day",
              xlab = "Step Range", col = 2,
              breaks=20) 

![](PA1_template_files/figure-markdown_strict/estimate%20NA%20values-1.png)

    # Follow the histogram with statistical summary functions:
    mean(tot.step$TotalSteps)

    ## [1] 10766

    median(tot.step$TotalSteps)

    ## [1] 10766

It is likely that the steps measured will vary from weekday to weekend.
Herein, we shall add columns to the dataset with the weekday and a T/F
column determining if it is a weekend.

    tot.activity$week <- weekdays(tot.activity$date)
    tot.activity$weekend <- tot.activity$week == "Saturday" | tot.activity$week == "Sunday"

    # Interval steps for a weekday
    time.step.wd <- tapply(tot.activity$steps[which(tot.activity$weekend == FALSE)], 
        tot.activity$interval[which(tot.activity$weekend == FALSE)], mean, simplify = FALSE)
    time.step.wd <- as.data.frame(time.step.wd)
    time.step.wd$weekend <- FALSE
    setDT(time.step.wd, keep.rownames = TRUE)[]

    ##        rn time.step.wd weekend
    ##   1:    0        2.251   FALSE
    ##   2:    5       0.4453   FALSE
    ##   3:   10       0.1732   FALSE
    ##   4:   15       0.1979   FALSE
    ##   5:   20      0.09895   FALSE
    ##  ---                          
    ## 284: 2335        2.249   FALSE
    ## 285: 2340         2.24   FALSE
    ## 286: 2345       0.2633   FALSE
    ## 287: 2350       0.2969   FALSE
    ## 288: 2355         1.41   FALSE

    colnames(time.step.wd) <- c("Interval", "Mean", "Weekend")

    # Interval steps for a weekend
    time.step.we <- tapply(tot.activity$steps[which(tot.activity$weekend == TRUE)], tot.activity$interval[which(tot.activity$weekend == 
        TRUE)], mean, simplify = FALSE)
    time.step.we <- as.data.frame(time.step.we)
    time.step.we$weekend <- TRUE

    setDT(time.step.we, keep.rownames = TRUE)[]

    ##        rn time.step.we weekend
    ##   1:    0       0.2146    TRUE
    ##   2:    5      0.04245    TRUE
    ##   3:   10      0.01651    TRUE
    ##   4:   15      0.01887    TRUE
    ##   5:   20     0.009434    TRUE
    ##  ---                          
    ## 284: 2335        11.59    TRUE
    ## 285: 2340        6.288    TRUE
    ## 286: 2345        1.705    TRUE
    ## 287: 2350       0.0283    TRUE
    ## 288: 2355       0.1344    TRUE

    colnames(time.step.we) <- c("Interval", "Mean", "Weekend")

    # Generate a numeric table & plot for weekday/weekend, now merged
    time.step.week <- rbind(time.step.wd, time.step.we)
    colnames(time.step.week) <- c("interval", "mean", "weekend")
    time.step.week$mean <- as.numeric(time.step.week$mean)
    time.step.week$interval <- as.numeric(time.step.week$interval)

    xyplot(mean ~ interval | weekend  # interval vs. mean as a factor of weekend (T/F)
    , 
        data = time.step.week, type = "l", main = "NA-Free Average Steps per Day", xlab = "Time Interval (min)", 
        ylab = "Mean Steps", strip = strip.custom(factor.levels = c("Weekday", "Weekend"), 
            par.strip.text = list(cex = 0.8)), layout = c(1, 2))

![](PA1_template_files/figure-markdown_strict/interval%20measurement%20steps-1.png)

One can see from the above graphs that this client has a much higher, but consistent, step count on weekends averaging around 125 steps. During a weekday, there is a high quantity of steps (230) around 8am when the person is likely scrambling to get to work. This is followed by a daily low of steps, presumably due to a stationary 10 - 6 job.
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
