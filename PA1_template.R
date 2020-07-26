#Dowloading and unzipping the dataset
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "activity.zip", mode="wb")
unzip("activity.zip")

#Reading the data
data <- read.csv("activity.csv", header = TRUE)

#Calculating the total number of steps per day
library(ggplot2)
total_steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)

#Plotting a histogram of total number of steps per day
qplot(total_steps, binwidth = 1000, col = "blue", xlab="Total steps per day", ylab = "Frequency", main="Total number of steps per day")

#Calculating mean and median
mean(total_steps, na.rm=TRUE)
median(total_steps, na.rm=TRUE)

#Average number of steps taken
average_steps <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)

#Time series plot
ggplot(data=average_steps, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("Interval") +
  ylab("Average number of steps taken")

#The 5-minute interval that, on average, contains the maximum number of steps
average_steps[which.max(average_steps$steps),]

#Imputing missing data
missing <- is.na(data$steps)
fill_values <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (average_steps[average_steps$interval==interval, "steps"])
  return(filled)
}
filled_data <- data
filled_data$steps <- mapply(fill_values, filled_data$steps, filled_data$interval)

#Creating a histogram of total number of steps per day after imputing data
imputed_total_steps <- tapply(filled_data$steps, filled_data$date, FUN=sum)
qplot(imputed_total_steps, binwidth = 1000, col = "blue", xlab="Total steps per day", ylab = "Frequency", main="Total number of steps per day")

#Calculating mean and median
mean(imputed_total_steps)
median(imputed_total_steps)

#Computing weekday or weekend
week_day <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
filled_data$date <- as.Date(filled_data$date)
filled_data$day <- sapply(filled_data$date, FUN=week_day)

#Plot
average_steps_weekday <- aggregate(steps ~ interval + day, data=filled_data, mean)
ggplot(average_steps_weekday, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("Interval") + ylab("Number of steps")
