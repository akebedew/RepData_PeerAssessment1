## Load Required Libraries:
library(plyr)
library(lattice)

###Loading and preprocessing the data

## Unzip the data in a sub-directory named 'activity' and read the data
	
activityData <- read.csv("./activity/activity.csv", header = TRUE, sep = ",")

### Check for the Attributes/properties of the data
	dim(activityData) # Dimensions 
	names(activityData) ## Variable Names 
	length(unique(activityData$date))  ## Number of days in the dataset
      NoofObs<- table(activityData$date, useNA = "ifany")  ## Number of Observations for each day
	sapply(activityData[1, ], class)  ## the class of each variable

### Process/transform the data (if necessary) into a format suitable for your analysis

## changing the class of the variable 'steps' to be Numeric:
	activityData$steps <- as.numeric(activityData$steps)
## changing the class of the variable 'interval' to be Numeric:
	activityData$interval <- as.numeric(activityData$interval)
	activityDataClean <- activityData[complete.cases(activityData), ]  # Clean data after removing NAs.

### What is mean total number of steps taken per day?

### Prepare data to construct a histogram of the total number of steps taken each day

### Calculate the total number of steps for each day
	DaySteps <- ddply(activityDataClean, c("date"), function(x) apply(x[1], 2, sum))
	head(DaySteps)

## construct a histogram of the total number of steps taken each day
	hist(DaySteps$steps, xlab = "Total Number of Steps per Day", 
	main = "Histogram of the Total Number of Steps Taken Each Day", col="blue")

### Calculating the mean and median total number of steps taken per day

	mean(DaySteps$steps, na.rm = TRUE)  # the mean of total steps per day
	median(DaySteps$steps, na.rm = TRUE) # the median of total steps per day

### What is the average daily activity pattern?

### Calculate the mean of steps related to each interval:
meanSteps <- ddply(activityDataClean, c("interval"), function(x) apply(x[1], 2, mean))

### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
### and the average number of steps taken, averaged across all days (y-axis)

	xyplot(meanSteps$steps ~ meanSteps$interval, type = "l", ylab = "Average number of steps taken, averaged 
	across all days", xlab = "5-minute interval", main = "Plot of Intervals vs. Mean Steps", lwd = 1.5)

### Which 5-minute interval, on average across all the days in the dataset, 
### contains the maximum number of steps?

## First obtain the the maximum number of steps
	maxStep<- max(meanSteps$steps)
	maxStep  
## The obtain the interval related to the maximum of mean step
	maxInt<-meanSteps[meanSteps$steps == max(meanSteps$steps), ]
	maxInt
## or
	maxInterval <- meanSteps[which.max(meanSteps$steps), 1]
	maxInterval

### Imputing missing values

### Calculate and report the total number of missing values in the dataset 
### (i.e. the total number of rows with NAs)
	sum(is.na(activityData$interval)) # interval with NA 
	sum(is.na(activityData$date))  # dates with NA
	sum(is.na(activityData$steps))  # Total number of rows with NAs

## Create a new dataset that is equal to the original dataset but with the 
## missing data filled in. In this case, the missing values are filled by replacing the rows 
## having NAs with the mean steps for each 5-min interval across all data

	meanSteps <- ddply(activityDataClean, c("interval"), function(x) apply(x[1], 2, mean))
	revisedData <- activityData
	revisedData[1:288, 1] <- meanSteps$steps
	revisedData[2017:2304, 1] <- meanSteps$steps
	revisedData[8929:9216, 1] <- meanSteps$steps
	revisedData[9793:10080, 1] <- meanSteps$steps
	revisedData[11233:11520, 1] <- meanSteps$steps
	revisedData[11521:11808, 1] <- meanSteps$steps
	revisedData[12673:12960, 1] <- meanSteps$steps
	revisedData[17281:17568, 1] <- meanSteps$steps

## Check if NAs are removed	
	sum(is.na(revisedData))  

## Calculating the sum of steps related to each day:
revisedmeanSteps <- ddply(revisedData, c("date"), function(x) apply(x[1], 2, sum))

### Make a histogram of the total number of steps taken each day and Calculate 
### and report the mean and median total number of steps taken per day. 
### Do these values differ from the estimates from the first part of the assignment? 

	
	hist(revisedmeanSteps$steps, xlab = "Total Number of Steps/Day", 
	main = "Histogram of the Total Number of Steps Taken Each Day", 
    	col = "red")

### What is the impact of imputing missing data on the estimates of the total daily number of steps?
### This histogram is slightly different from the previous one which was created
## before filling the NAs. This histogram has higher frequency around mean steps

## Compute the mean of total steps after filling in all NAs: the mean is
## exactly the same as before. Imputing missing data did not have majore
## impact on the estimates of the total daily number of steps.
	
	mean(revisedmeanSteps$steps)  # 10766.19

## Compute the median of total steps after filling in all NAs: The median
## was increased very slightly and is now equal to the mean. Imputing missing
## data did not have majore impact on the estimates of the total daily number
## of steps.
	median(revisedmeanSteps$steps)  # 10766.19

### Are there differences in activity patterns between weekdays and weekends?

## Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
## indicating whether a given date is a weekday or weekend day.
## Adding new column to the dataset 'newData' containing the week days'names:

	revisedData$WeekD <- weekdays(as.Date(revisedData$date))

## Replacing the names of week day with 'Weekday' and 'Weekend':

	revisedData$WeekD[revisedData$WeekD == "Friday"] <- "Weekday"
	revisedData$WeekD[revisedData$WeekD == "Monday"] <- "Weekday"
	revisedData$WeekD[revisedData$WeekD == "Thursday"] <- "Weekday"
	revisedData$WeekD[revisedData$WeekD == "Tuesday"] <- "Weekday"
	revisedData$WeekD[revisedData$WeekD == "Wednesday"] <- "Weekday"
	revisedData$WeekD[revisedData$WeekD == "Saturday"] <- "Weekend"
	revisedData$WeekD[revisedData$WeekD == "Sunday"] <- "Weekend"
	table(revisedData$WeekD)

## Construct a panel plot containing a time series plot of the 5-min
## interval and the average number of steps taken, averaged across all
## weekday days or weekend days.

	newMeanSteps <- ddply(revisedData, c("interval", "WeekD"), function(x) 
	apply(x[1], 2, mean))
	
	xyplot(newMeanSteps$steps ~ newMeanSteps$interval | newMeanSteps$WeekD, 
	type = "l", ylab = "Number of Steps",xlab = "Interval", 
	main = "Plot of the 5-minute Interval vs. Average Number of Steps", layout = c(1, 2))

