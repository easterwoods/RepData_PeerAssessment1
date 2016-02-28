## Loading and Processing data
rawData <- read.csv("activity.csv")
library(lubridate)
library(dplyr)
dataDate <- mutate(rawData, date = ymd(rawData$date))

## Calculate meant total number of steps per day
dataNoInterval <- select(dataDate, -interval)
dataNoInterval <- filter(dataNoInterval, steps != "NA")
dataByDate <- group_by(dataNoInterval, date)
stepSum <- summarize(dataByDate, sum = sum(steps))
hist(stepSum$sum, col = "blue", breaks = 20)
rug(stepSum$sum)
stepsMean <- mean(stepSum$sum)
stepsMedian <- median(stepSum$sum)
abline(v = stepsMean, col = "green", lwd = 2)

## Average Daily activity pattern
dataDailyActivity <- group_by(dataDate, interval)
dailyActivityMean <- summarize(dataDailyActivity, average_interval = mean(steps, na.rm = TRUE))
plot(dailyActivityMean$interval, dailyActivityMean$average_interval, type = "l", col = "green", 
     main = "Average Daily Activity Pattern", xlab = "Time Interval", ylab = "Average Steps Taken")
arrangedDailyActivity <- arrange(dailyActivityMean, desc(average_interval))
arrangedDailyActivity[1,]
abline(v = 835, col = "blue", lwd = 2)

## Imputing Missing Values
sum(is.na(dataDate$steps))
imputedData <- transform(dataDate, steps = ifelse(is.na(steps), dailyActivityMean$average_interval, steps))
groupedImputedData <- group_by(imputedData, date)
imputedSum <- summarize(groupedImputedData, sum = sum(steps))
hist(imputedSum$sum, col = "blue", breaks = 20)
rug(imputedSum$sum)
stepsMean2 <- mean(imputedSum$sum)
stepsMedian2 <- median(imputedSum$sum)
abline(v = stepsMean2, col = "green", lwd = 2)

## Activity Difference between weekend/weekday
weekdayData <- mutate(imputedData, weekday = weekdays(imputedData$date, abbreviate = FALSE))
weekdayData$weekday[weekdayData$weekday == "Monday"] <- "weekday"
weekdayData$weekday[weekdayData$weekday == "Tuesday"] <- "weekday"
weekdayData$weekday[weekdayData$weekday == "Wednesday"] <- "weekday"
weekdayData$weekday[weekdayData$weekday == "Thursday"] <- "weekday"
weekdayData$weekday[weekdayData$weekday == "Friday"] <- "weekday"
weekdayData$weekday[weekdayData$weekday == "Saturday"] <- "weekend"
weekdayData$weekday[weekdayData$weekday == "Sunday"] <- "weekend"
weekdays <- filter(weekdayData, weekday == "weekday")
weekends <- filter(weekdayData, weekday == "weekend") 
groupedWeekdays <- group_by(weekdays, interval)
meanWeekdays <- summarize(groupedWeekdays, average_interval = mean(steps))
groupedWeekends <- group_by(weekends, interval)
meanWeekends <- summarize(groupedWeekends, average_interval = mean(steps))
par(mfrow=c(2, 1))
plot(meanWeekdays$interval, meanWeekdays$average_interval, type = "l", col = "green", 
     main = "Interval Mean Weekdays", xlab = "Time Interval", ylab = "Average Steps Taken")
plot(meanWeekends$interval, meanWeekends$average_interval, type = "l", col = "green", 
     main = "Interval Mean Weekends", xlab = "Time Interval", ylab = "Average Steps Taken")



