library("psych")
library("ggplot2")
library("corrplot")
library("gridExtra")
library("fpp2")

######################################################### Moving Average Part #######################################################################################################################################
# ------------------------------------------------------ moving average for t1 --------------------------------------------------------------------------------------------------------------------------------------
#function to grab moving average
test5 <- read.csv(file="test5.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
test5 <- na.omit(test5)
test5$Year <- as.numeric(format(as.Date(test5$Date, "%d/%m/%Y"), "%Y"))
test5$Day <- as.POSIXlt(test5$Date)$wday
x_test5 <- paste(test5$Date, test5$Time)
dateTime_test5 <- as.POSIXlt(x_test5, format = "%d/%m/%Y %H:%M:%S")
test5$Week <- strftime(dateTime_test5, format = "%V")
test5_filter_morning <- subset(test5, (strptime(Time, format = "%H:%M:%S") >= strptime("05:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("09:30:00", format = "%H:%M:%S")))
test5_filter_evening <- subset(test5, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))


moving_morning_GA_test5_weekday <- ma(test5_filter_morning$Global_active_power, order = 20)
moving_evening_GA_test5_weekday <- ma(test5_filter_evening$Global_active_power, order = 20)
moving_morning_GI_test5_weekday <- ma(test5_filter_morning$Global_intensity, order = 20)
moving_evening_GI_test5_weekday <- ma(test5_filter_evening$Global_intensity, order = 20)

##### store these moving average to the table 
test5_filter_morning$moving_morning_GA_test5_weekday <- moving_morning_GA_test5_weekday
test5_filter_evening$moving_evening_GA_test5_weekday <- moving_evening_GA_test5_weekday
test5_filter_morning$moving_morning_GI_test5_weekday <- moving_morning_GI_test5_weekday
test5_filter_evening$moving_evening_GI_test5_weekday <- moving_evening_GI_test5_weekday
# week day - 2010-10-09 Tuesday -----------------------------------------------------------------------------------------------------------------------------------------------------------
subset_test5_weekday_morning<-subset(test5_filter_morning, test5_filter_morning$Week == 40 & test5_filter_morning$Day == 2 & test5_filter_morning$Year == 2010)
subset_test5_weekday_evening<-subset(test5_filter_evening, test5_filter_evening$Week == 40 & test5_filter_evening$Day == 2 & test5_filter_evening$Year == 2010)

##### calculate the difference between observations and moving average
# morning GA
for (i in 1: nrow(subset_test5_weekday_morning)) {                                                                                                                 
  calculated_GA_difference_morning <- (subset_test5_weekday_morning$Global_active_power[i] - subset_test5_weekday_morning$moving_morning_GA_test5_weekday[i])                                        
  subset_test5_weekday_morning$calculated_GA_difference_morning[i] <- calculated_GA_difference_morning                                                                                                        
}
# eveing GA
for (i in 1: nrow(subset_test5_weekday_evening)) {                                                                                                                 
  calculated_GA_difference_evening <- (subset_test5_weekday_evening$Global_active_power[i] - subset_test5_weekday_evening$moving_evening_GA_test5_weekday[i])                                        
  subset_test5_weekday_evening$calculated_GA_difference_evening[i] <- calculated_GA_difference_evening                                                                                                        
}
# morning GI
for (i in 1: nrow(subset_test5_weekday_morning)) {                                                                                                                 
  calculated_GI_difference_morning <- (subset_test5_weekday_morning$Global_intensity[i] - subset_test5_weekday_morning$moving_morning_GI_test5_weekday[i])                                        
  subset_test5_weekday_morning$calculated_GI_difference_morning[i] <- calculated_GI_difference_morning                                                                                                        
}
# evening GI
for (i in 1: nrow(subset_test5_weekday_evening)) {                                                                                                                 
  calculated_GI_difference_evening <- (subset_test5_weekday_evening$Global_intensity[i] - subset_test5_weekday_evening$moving_evening_GI_test5_weekday[i])                                        
  subset_test5_weekday_evening$calculated_GI_difference_evening[i] <- calculated_GI_difference_evening                                                                                                        
}

#plot 3 graphs, red - observations, black - moving average, orange - difference between the two
ggplot(data = subset_test5_weekday_morning, aes(x=Time, y=Global_active_power, group = 1)) + geom_line(color = "red") +
  geom_line(aes(y=subset_test5_weekday_morning$moving_morning_GA_test5_weekday)) + geom_line(aes(y=subset_test5_weekday_morning$calculated_GA_difference_morning), color = 'orange') +
  ggtitle("Global_active_power vs Moving Average for 2010-10-09 5:30am to 9:30 am ") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = subset_test5_weekday_morning, aes(x=Time, y=Global_intensity, group = 1)) + geom_line(color = "red") +
  geom_line(aes(y=subset_test5_weekday_morning$moving_morning_GI_test5_weekday)) + geom_line(aes(y=subset_test5_weekday_morning$calculated_GI_difference_morning), color = 'orange') +
  ggtitle("Global_intensity vs Moving Average for 2010-10-09 5:30am to 9:30 am ") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = subset_test5_weekday_evening, aes(x=Time, y=Global_active_power, group = 1)) + geom_line(color = "red") +
  geom_line(aes(y=subset_test5_weekday_evening$moving_evening_GA_test5_weekday)) + geom_line(aes(y=subset_test5_weekday_evening$calculated_GA_difference_evening), color = 'orange') +
  ggtitle("Global_active_power vs Moving Average for 2010-10-09 5:30pm to 9:30 pm ") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = subset_test5_weekday_evening, aes(x=Time, y=Global_active_power, group = 1)) + geom_line(color = "red") +                                            
  geom_line(aes(y=subset_test5_weekday_evening$moving_evening_GA_test5_weekday)) + geom_line(aes(y=subset_test5_weekday_evening$calculated_GI_difference_evening), color = 'orange') +
  ggtitle("Global_intensity vs Moving Average for 2010-10-09 5:30pm to 9:30 pm ") +
  theme(plot.title = element_text(hjust = 0.5))

#calculate the threshold and determine the number of anomalies
test5_GA_morning_threshold_positive = 0.5
test5_GA_morning_threshold_negative = -0.5
count_GA_anomalies_test5_weekday_morning <- 0
for (i in 1: nrow(subset_test5_weekday_morning)) {
  if (!is.na(subset_test5_weekday_morning$calculated_GA_difference_morning[i])) {
    if (subset_test5_weekday_morning$calculated_GA_difference_morning[i] > test5_GA_morning_threshold_positive || subset_test5_weekday_morning$calculated_GA_difference_morning[i] < test5_GA_morning_threshold_negative) {
      count_GA_anomalies_test5_weekday_morning <- count_GA_anomalies_test5_weekday_morning + 1
    }
  }
}

test5_GA_evening_threshold_positive = 0.5
test5_GA_evening_threshold_negative = -0.5
count_GA_anomalies_test5_weekday_evening <- 0
for (i in 1: nrow(subset_test5_weekday_evening)) {
  if (!is.na(subset_test5_weekday_evening$calculated_GA_difference_evening[i])) {
    if (subset_test5_weekday_evening$calculated_GA_difference_evening[i] > test5_GA_evening_threshold_positive || subset_test5_weekday_evening$calculated_GA_difference_evening[i] < test5_GA_evening_threshold_negative) {
      count_GA_anomalies_test5_weekday_evening <- count_GA_anomalies_test5_weekday_evening + 1
    }
  }
}


test5_GI_morning_threshold_positive = 0.5
test5_GI_morning_threshold_negative = -0.5
count_GI_anomalies_test5_weekday_morning <- 0
for (i in 1: nrow(subset_test5_weekday_morning)) {
  if (!is.na(subset_test5_weekday_morning$calculated_GI_difference_morning[i])) {
    if (subset_test5_weekday_morning$calculated_GI_difference_morning[i] > test5_GI_morning_threshold_positive || subset_test5_weekday_morning$calculated_GI_difference_morning[i] < test5_GI_morning_threshold_negative) {
      count_GI_anomalies_test5_weekday_morning <- count_GI_anomalies_test5_weekday_morning + 1
    }
  }
}

test5_GI_evening_threshold_positive = 0.5
test5_GI_evening_threshold_negative = -0.5
count_GI_anomalies_test5_weekday_evening <- 0
for (i in 1: nrow(subset_test5_weekday_evening)) {
  if (!is.na(subset_test5_weekday_evening$calculated_GI_difference_evening[i])) {
    if (subset_test5_weekday_evening$calculated_GI_difference_evening[i] > test5_GI_evening_threshold_positive || subset_test5_weekday_evening$calculated_GI_difference_evening[i] < test5_GI_evening_threshold_negative) {
      count_GI_anomalies_test5_weekday_evening <- count_GI_anomalies_test5_weekday_evening + 1
    }
  }
}


# weekend - 2010-10-13 sunday -----------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
moving_morning_GA_test5_weekend<- ma(test5_filter_morning$Global_active_power, order = 20)
moving_evening_GA_test5_weekend <- ma(test5_filter_evening$Global_active_power, order = 20)
moving_morning_GI_test5_weekend <- ma(test5_filter_morning$Global_intensity, order = 20)
moving_evening_GI_test5_weekend <- ma(test5_filter_evening$Global_intensity, order = 20)

##### store these moving average to the table 
test5_filter_morning$moving_morning_GA_test5_weekend <- moving_morning_GA_test5_weekend
test5_filter_evening$moving_evening_GA_test5_weekend <- moving_evening_GA_test5_weekend
test5_filter_morning$moving_morning_GI_test5_weekend <- moving_morning_GI_test5_weekend
test5_filter_evening$moving_evening_GI_test5_weekend <- moving_evening_GI_test5_weekend
# weekend - 2010-10-13 sunday -----------------------------------------------------------------------------------------------------------------------------------------------------------
subset_test5_weekend_morning<-subset(test5_filter_morning, test5_filter_morning$Week == 41 & test5_filter_morning$Day == 0 & test5_filter_morning$Year == 2010)
subset_test5_weekend_evening<-subset(test5_filter_evening, test5_filter_evening$Week == 41 & test5_filter_evening$Day == 0 & test5_filter_evening$Year == 2010)

##### calculate the difference between observations and moving average
# morning GA
for (i in 1: nrow(subset_test5_weekend_morning)) {                                                                                                                 
  calculated_GA_difference_morning_weekend <- (subset_test5_weekend_morning$Global_active_power[i] - subset_test5_weekend_morning$moving_morning_GA_test5_weekend[i])                                        
  subset_test5_weekend_morning$calculated_GA_difference_morning[i] <- calculated_GA_difference_morning_weekend                                                                                                        
}
# eveing GA
for (i in 1: nrow(subset_test5_weekend_evening)) {                                                                                                                 
  calculated_GA_difference_evening_weekend <- (subset_test5_weekend_evening$Global_active_power[i] - subset_test5_weekend_evening$moving_evening_GA_test5_weekend[i])                                        
  subset_test5_weekend_evening$calculated_GA_difference_evening[i] <- calculated_GA_difference_evening_weekend                                                                                                        
}
# morning GI
for (i in 1: nrow(subset_test5_weekend_morning)) {                                                                                                                 
  calculated_GI_difference_morning_weekend <- (subset_test5_weekend_morning$Global_intensity[i] - subset_test5_weekend_morning$moving_morning_GI_test5_weekend[i])                                        
  subset_test5_weekend_morning$calculated_GI_difference_morning[i] <- calculated_GI_difference_morning_weekend                                                                                                        
}
# evening GI
for (i in 1: nrow(subset_test5_weekend_evening)) {                                                                                                                 
  calculated_GI_difference_evening_weekend <- (subset_test5_weekend_evening$Global_intensity[i] - subset_test5_weekend_evening$moving_evening_GI_test5_weekend[i])                                        
  subset_test5_weekend_evening$calculated_GI_difference_evening[i] <- calculated_GI_difference_evening_weekend                                                                                                        
}

#plot 3 graphs, red - observations, black - moving average, orange - difference between the two
ggplot(data = subset_test5_weekend_morning, aes(x=Time, y=Global_active_power, group = 1)) + geom_line(color = "red") +
  geom_line(aes(y=subset_test5_weekend_morning$moving_morning_GA_test5_weekend)) + geom_line(aes(y=subset_test5_weekend_morning$calculated_GA_difference_morning), color = 'orange') +
  ggtitle("Global_active_power vs Moving Average for 2010-10-13 5:30am to 9:30 am ") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = subset_test5_weekend_morning, aes(x=Time, y=Global_intensity, group = 1)) + geom_line(color = "red") +
  geom_line(aes(y=subset_test5_weekend_morning$moving_morning_GI_test5_weekend)) + geom_line(aes(y=subset_test5_weekend_morning$calculated_GI_difference_morning), color = 'orange') +
  ggtitle("Global_intensity vs Moving Average for 2010-10-13 5:30am to 9:30 am ") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = subset_test5_weekend_evening, aes(x=Time, y=Global_active_power, group = 1)) + geom_line(color = "red") +
  geom_line(aes(y=subset_test5_weekend_evening$moving_evening_GA_test5_weekend)) + geom_line(aes(y=subset_test5_weekend_evening$calculated_GA_difference_evening), color = 'orange') +
  ggtitle("Global_active_power vs Moving Average for 2010-10-13 5:30pm to 9:30 pm ") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = subset_test5_weekend_evening, aes(x=Time, y=Global_active_power, group = 1)) + geom_line(color = "red") +                                            
  geom_line(aes(y=subset_test5_weekend_evening$moving_evening_GA_test5_weekend)) + geom_line(aes(y=subset_test5_weekend_evening$calculated_GI_difference_evening), color = 'orange') +
  ggtitle("Global_intensity vs Moving Average for 2010-10-13 5:30pm to 9:30 pm ") +
  theme(plot.title = element_text(hjust = 0.5))

#calculate the threshold and determine the number of anomalies
test5_GA_morning_threshold_positive_weekend = 0.5
test5_GA_morning_threshold_negative_weekend = -0.5
count_GA_anomalies_test5_weekend_morning <- 0
for (i in 1: nrow(subset_test5_weekend_morning)) {
  if (!is.na(subset_test5_weekend_morning$calculated_GA_difference_morning[i])) {
    if (subset_test5_weekend_morning$calculated_GA_difference_morning[i] > test5_GA_morning_threshold_positive_weekend || subset_test5_weekend_morning$calculated_GA_difference_morning[i] < test5_GA_morning_threshold_negative_weekend) {
      count_GA_anomalies_test5_weekend_morning <- count_GA_anomalies_test5_weekend_morning + 1
    }
  }
}

test5_GA_evening_threshold_positive_weekend = 0.5
test5_GA_evening_threshold_negative_weekend = -0.5
count_GA_anomalies_test5_weekend_evening <- 0
for (i in 1: nrow(subset_test5_weekend_evening)) {
  if (!is.na(subset_test5_weekend_evening$calculated_GA_difference_evening[i])) {
    if (subset_test5_weekend_evening$calculated_GA_difference_evening[i] > test5_GA_evening_threshold_positive_weekend || subset_test5_weekend_evening$calculated_GA_difference_evening[i] < test5_GA_evening_threshold_negative_weekend) {
      count_GA_anomalies_test5_weekend_evening <- count_GA_anomalies_test5_weekend_evening + 1
    }
  }
}


test5_GI_morning_threshold_positive_weekend = 0.5
test5_GI_morning_threshold_negative_weekend = -0.5
count_GI_anomalies_test5_weekend_morning <- 0
for (i in 1: nrow(subset_test5_weekend_morning)) {
  if (!is.na(subset_test5_weekend_morning$calculated_GI_difference_morning[i])) {
    if (subset_test5_weekend_morning$calculated_GI_difference_morning[i] > test5_GI_morning_threshold_positive_weekend || subset_test5_weekend_morning$calculated_GI_difference_morning[i] < test5_GI_morning_threshold_negative_weekend) {
      count_GI_anomalies_test5_weekend_morning <- count_GI_anomalies_test5_weekend_morning + 1
    }
  }
}

test5_GI_evening_threshold_positive_weekend = 0.5
test5_GI_evening_threshold_negative_weekend = -0.5
count_GI_anomalies_test5_weekend_evening <- 0
for (i in 1: nrow(subset_test5_weekend_evening)) {
  if (!is.na(subset_test5_weekend_evening$calculated_GI_difference_evening[i])) {
    if (subset_test5_weekend_evening$calculated_GI_difference_evening[i] > test5_GI_evening_threshold_positive_weekend || subset_test5_weekend_evening$calculated_GI_difference_evening[i] < test5_GI_evening_threshold_negative_weekend) {
      count_GI_anomalies_test5_weekend_evening <- count_GI_anomalies_test5_weekend_evening + 1
    }
  }
}
