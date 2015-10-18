# HEADER: Loading and preprocessing the data
library(plyr)
library(lubridate)

# Read the data from the directory

data <- read.csv("activity.csv", header=TRUE, na.strings = "NA")
# Convert the date column to date format
data$date <- as.Date(data$date, format = "%Y-%m-%d")

# HEADER: What is the mean total number of steps taken per day, ignoring NA?
# Total number of steps taken per day
dataComplete <- data[complete.cases(data),]
totalSteps <- aggregate(steps  ~ date, 
                        FUN=sum, data=dataComplete)

# Histogram of total steps taken per day

hist(totalSteps$steps,
     main="Histogram of Total Steps Taken Per Day",
     xlab="Total Number of Steps in a day",
     sub="Only Complete Cases")

# Mean and median of total number of steps taken per day
meanSteps <- mean(totalSteps$steps)
medianSteps <- median(totals$steps)

print(meanSteps)
print(medianSteps)

# HEADER: What is the average daily activity pattern?
# Aggregate average steps taken over each interval
totalIs <- aggregate(steps  ~ interval, 
                     data=dataComplete, 
                     FUN="mean")

plot(totalIs$interval, totalIs$steps,
     type= "l",
     main="Average Daily Activity Pattern",
     xlab="Five Minute Interval",
     ylab= "Average Number of Steps")

totalIsMax <- totalIs[totalIs$steps == max(totalIs$steps),]
print(totalIsMax)
# HEADER: Inputing missing values

# What is the total number of missing values in the dataset?

sum(is.na(data))

# Strategy for filling in NAs 
# take mean daily steps calculated above and divide 
# by number of 5 minute intervals in a day (288)

newNA <- meanSteps/288
dataNAInput <- data
dataNAInput[is.na(dataNAInput)] <- newNA

# New histogram of total number of steps taken each day
totalStepsNA <- aggregate(steps  ~ date, 
                        FUN=sum, data=dataNAInput)
hist(totalStepsNA$steps,
     main="Histogram of Total Steps Taken Per Day",
     sub="NA Values Replaced with Daily Average/Number of Intervals",
     xlab="Total Number of Steps in a day")

# Mean and Median of total steps per day
meanStepsNA <- mean(totalStepsNA$steps)
medianStepsNA <- median(totalStepsNA$steps)

print(meanStepsNA)
print(medianStepsNA)
# Mean and median of the total number of steps taken

# Describe impact of imputing missing data

# HEADER: Are there differences in activity patterns 
# between weekdays and weekends?
# Find the day of the week using lubridate
data$date <- ymd(data$date)
data$day <- paste(wday(data$date))

# Create two datasets - one for the weekend, one for the weekday
# day = 1 (Sunday) or 7 (Saturday)
weekend <- data[ which(data$day== 1 
                       | data$day == 7), ]
weekday <- data[which(data$day >= 2 | data$day <= 6), ]

totalIsWeekend <- aggregate(steps  ~ interval, 
                     data=weekend, 
                     FUN="mean")
totalIsWeekday <- aggregate(steps  ~ interval, 
                     data=weekday, 
                     FUN="mean")
par(mfrow = c(2,1), mar = c(5, 4, 2, 1))
plot(totalIsWeekday$interval, totalIsWeekday$steps,
     type= "l",
     main="Average Daily Activity Pattern - Weekday",
     xlab="Five Minute Interval",
     ylab= "Average Number of Steps")
plot(totalIsWeekend$interval, totalIsWeekend$steps,
     type= "l",
     main="Average Daily Activity Pattern - Weekend",
     xlab="Five Minute Interval",
     ylab= "Average Number of Steps")
