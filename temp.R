library(readr)
library(ggplot2)
library(imputeTS)
library(dplyr)

#unzip("activity.zip")
activity_data <- read_csv("activity.csv", col_types = cols(date = col_date(format = "%Y-%m-%d")))

# 1
## What is mean total number of steps taken per day?

total_steps_by_date <- aggregate(steps ~ date,activity_data, sum, na.rm = TRUE)
mean_steps_by_date <- aggregate(steps ~ date,activity_data, mean, na.rm = TRUE)

ggplot(data = total_steps_by_date, aes(x = date, y = steps)) + geom_point(color= 'red') + 
  geom_line(color= 'red') +
  ggtitle('Total Steps by Day')

ggplot() + geom_violin(data = total_steps_by_date, aes(x = date, y = steps), color= 'red') +
  ggtitle('Violin Plot of Total Steps by Day') + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title = element_blank())

ggplot(data = mean_steps_by_date, aes(x = date, y = steps)) + geom_point(color= 'red') + 
  geom_line(color= 'red') +
  ggtitle('Mean Steps by Day')

ggplot() + geom_violin(data = mean_steps_by_date, aes(x = date, y = steps), color= 'red') +
  ggtitle('Violin Plot of Mean Steps by Day') + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title = element_blank())


#2 
## What is the average daily activity pattern?

# Calculate mean steps per interval and omit NAs
average_daily_activity_pattern <- aggregate(steps ~ interval,activity_data, mean, na.rm = TRUE)

ggplot() + geom_line(data = average_daily_activity_pattern, aes(x = interval, y = steps), color= 'red') +
  theme_grey() +  
  labs(x = "interval", y = "Number of steps", title = "Average Daily Activity Pattern")

## Imputing missing values
# I use the imputeTS library to analyze and impute values based on moving avergae values

# display missing value stats
statsNA(activity_data$steps)

# plot distribution of NA
plotNA.distributionBar(activity_data$steps)
#plotNA.distribution(activity_data$steps)
#plotNA.gapsize(activity_data$steps)

# use moving average to impute
imputed_steps <- na_ma(activity_data$steps, k = 4, weighting = "exponential", maxgap = Inf)
#plot(na_ma(activity_data$steps, k = 4, weighting = "exponential", maxgap = Inf))

# # use mean to impute
# imputed_steps2 <-na_mean(activity_data$steps, option = "mean", maxgap = Inf)
# plot(na_mean(activity_data$steps, option = "mean", maxgap = Inf))

# plot NA distribution
plotNA.distributionBar(activity_data$steps)

# replace missing values
activity_data_imputed <- activity_data %>%
  mutate(steps  = ifelse(is.na(steps), imputed_steps[1], steps))
statsNA(activity_data_imputed$steps)

# plot of activity with imputed data
ggplot() + geom_line(data = activity_data_imputed, aes(x = interval, y = steps), color= 'red') +
  theme_grey() +  
  labs(x = "interval", y = "Number of steps", title = "Average Daily Activity Pattern (with imputed data)")

## Are there differences in activity patterns between weekdays and weekends?

# use daily activity with imputed data, create indicator for weekend days and subset data 
activity_data_imputed <- mutate(activity_data_imputed, day = weekdays(activity_data_imputed$date))
weekend_days <- c('Saturday', 'Sunday')
activity_data_imputed$weekend <- factor((weekdays(activity_data_imputed$date) %in% weekend_days), levels=c(TRUE, FALSE))

weekdays <- subset(activity_data_imputed, weekend == FALSE)
weekends <- subset(activity_data_imputed, weekend == TRUE)

# calculate means for weekdays and weekends
# weekendmeans <- with(weekends, tapply(steps, interval, mean))
# weekdaymeans <- with(weekdays, tapply(steps, interval, mean))

# plot weekdays activity pattern
average_weekday_activity_pattern <- aggregate(steps ~ interval,weekdays, mean, na.rm = TRUE)
ggplot() + geom_line(data = average_weekday_activity_pattern, aes(x = interval, y = steps), color= 'red') +
  theme_grey() +  
  ylim(0,220) +
  labs(x = "interval", y = "Number of steps", title = "Average Weekday Activity Pattern")

# plot weekend activity pattern
average_weekend_activity_pattern <- aggregate(steps ~ interval,weekends, mean, na.rm = TRUE)
ggplot() + geom_line(data = average_weekend_activity_pattern, aes(x = interval, y = steps), color= 'red') +
  theme_grey() +  
  ylim(0,220) +
  labs(x = "interval", y = "Number of steps", title = "Average Weekend Activity Pattern")

