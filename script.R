if (!file.exists("activity.csv")) {
  unzip("activity.zip")
}

# Loading and preprocessing the data

data <- read.csv("activity.csv")
data <- transform(data, date = as.Date(date))

require(plyr)
require(ggplot2)

# What is mean total number of steps taken per day?

daily <- ddply(data, ~date, summarise, steps = sum(steps))
png(filename = "steps_hist.png", width = 480, height = 480)
hist(daily$steps, breaks = 10, xlab = "Step count", main = "Step histogram")
dev.off()

daily_mean <- mean(daily$steps, na.rm = TRUE)
daily_median <- median(daily$steps, na.rm = TRUE)

print(paste("Daily mean:", daily_mean))
print(paste("Daily median:", daily_median))

# What is the average daily activity pattern?

daily_pattern <- ddply(data, ~interval, summarize, steps = mean(steps, na.rm = TRUE))
png(filename = "daily_pattern.png", width = 480, height = 480)
plot(daily_pattern$steps, type = "l", ylab = "Steps", xlab = "Interval", main = "Daily pattern")
dev.off()

print(paste("Busiest interval:", daily_pattern$interval[which.max(daily_pattern$steps)]))

# Imputing missing values

print(paste("Number of empty values:", sum(is.na(data$steps))))

steps <- data$steps
data_imput <- ddply(data, ~interval, function(interval) {
  mean_steps <- mean(interval$steps, na.rm = TRUE)
  interval$steps[is.na(interval$steps)] <- mean_steps
    
  interval
})

daily_imput <- ddply(data_imput, ~date, summarise, steps = sum(steps))

png(filename = "steps_hist_imput.png", width = 480, height = 480)
hist(daily_imput$steps, breaks = 10, xlab = "Step count", main = "Step histogram (imput values)")
dev.off()

daily_mean_imput <- mean(daily_imput$steps, na.rm = TRUE)
daily_median_imput <- median(daily_imput$steps, na.rm = TRUE)

print(paste("Daily mean (imput):", daily_mean_imput))
print(paste("Daily median (imput):", daily_median_imput))

# Are there differences in activity patterns between weekdays and weekends?

data$week_part <- sapply(data$date, function (date) {
  if (weekdays(date) %in% c("Saturday", "Sunday")) {
    return("weekend")
  } else {
    return("weekday")
  }
})

data_weekday <- subset(data, data$week_part == "weekday")
data_weekend <- subset(data, data$week_part == "weekend")

daily_pattern_weekday <- ddply(data_weekday, ~interval, summarize, steps = mean(steps, na.rm = TRUE))
daily_pattern_weekend <- ddply(data_weekend, ~interval, summarize, steps = mean(steps, na.rm = TRUE))

png(filename = "weekly_pattern.png", width = 480, height = 480)
par(mfrow = c(1,2))
plot(daily_pattern_weekday$steps, type = "l", ylab = "Steps", xlab = "Interval", main = "Daily pattern on weekdays")
plot(daily_pattern_weekend$steps, type = "l", ylab = "Steps", xlab = "Interval", main = "Daily pattern on weekends")
dev.off()