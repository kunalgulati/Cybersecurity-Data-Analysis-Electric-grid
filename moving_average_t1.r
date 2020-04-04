library("psych")
library("ggplot2")
library("corrplot")
library("gridExtra")
library("fpp2")

######################################################### Moving Average Part #######################################################################################################################################
# ------------------------------------------------------ moving average for t1 --------------------------------------------------------------------------------------------------------------------------------------
#function to grab moving average
test1 <- read.csv(file="test1.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
test1 <- na.omit(test1)
test1$Year <- as.numeric(format(as.Date(test1$Date, "%d/%m/%Y"), "%Y"))
test1$Day <- as.POSIXlt(test1$Date)$wday
x_test1 <- paste(test1$Date, test1$Time)
dateTime_test1 <- as.POSIXlt(x_test1, format = "%d/%m/%Y %H:%M:%S")
test1$Week <- strftime(dateTime_test1, format = "%V")
test1_filter_morning <- subset(test1, (strptime(Time, format = "%H:%M:%S") >= strptime("05:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("09:30:00", format = "%H:%M:%S")))
test1_filter_evening <- subset(test1, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))


moving_morning_GA_test1_weekday <- ma(test1_filter_morning$Global_active_power, order = 20)
moving_evening_GA_test1_weekday <- ma(test1_filter_evening$Global_active_power, order = 20)
moving_morning_GI_test1_weekday <- ma(test1_filter_morning$Global_intensity, order = 20)
moving_evening_GI_test1_weekday <- ma(test1_filter_evening$Global_intensity, order = 20)

##### store these moving average to the table 
test1_filter_morning$moving_morning_GA_test1_weekday <- moving_morning_GA_test1_weekday
test1_filter_evening$moving_evening_GA_test1_weekday <- moving_evening_GA_test1_weekday
test1_filter_morning$moving_morning_GI_test1_weekday <- moving_morning_GI_test1_weekday
test1_filter_evening$moving_evening_GI_test1_weekday <- moving_evening_GI_test1_weekday
# week day - 2010-10-09 Tuesday -----------------------------------------------------------------------------------------------------------------------------------------------------------
subset_test1_weekday_morning<-subset(test1_filter_morning, test1_filter_morning$Week == 40 & test1_filter_morning$Day == 2 & test1_filter_morning$Year == 2010)
subset_test1_weekday_evening<-subset(test1_filter_evening, test1_filter_evening$Week == 40 & test1_filter_evening$Day == 2 & test1_filter_evening$Year == 2010)

##### calculate the difference between observations and moving average
# morning GA
for (i in 1: nrow(subset_test1_weekday_morning)) {                                                                                                                 
  calculated_GA_difference_morning <- (subset_test1_weekday_morning$Global_active_power[i] - subset_test1_weekday_morning$moving_morning_GA_test1_weekday[i])                                        
  subset_test1_weekday_morning$calculated_GA_difference_morning[i] <- calculated_GA_difference_morning                                                                                                        
}
# eveing GA
for (i in 1: nrow(subset_test1_weekday_evening)) {                                                                                                                 
  calculated_GA_difference_evening <- (subset_test1_weekday_evening$Global_active_power[i] - subset_test1_weekday_evening$moving_evening_GA_test1_weekday[i])                                        
  subset_test1_weekday_evening$calculated_GA_difference_evening[i] <- calculated_GA_difference_evening                                                                                                        
}
# morning GI
for (i in 1: nrow(subset_test1_weekday_morning)) {                                                                                                                 
  calculated_GI_difference_morning <- (subset_test1_weekday_morning$Global_intensity[i] - subset_test1_weekday_morning$moving_morning_GI_test1_weekday[i])                                        
  subset_test1_weekday_morning$calculated_GI_difference_morning[i] <- calculated_GI_difference_morning                                                                                                        
}
# evening GI
for (i in 1: nrow(subset_test1_weekday_evening)) {                                                                                                                 
  calculated_GI_difference_evening <- (subset_test1_weekday_evening$Global_intensity[i] - subset_test1_weekday_evening$moving_evening_GI_test1_weekday[i])                                        
  subset_test1_weekday_evening$calculated_GI_difference_evening[i] <- calculated_GI_difference_evening                                                                                                        
}

#plot 3 graphs, red - observations, black - moving average, orange - difference between the two
ggplot(data = subset_test1_weekday_morning, aes(x=Time, y=Global_active_power, group = 1)) + geom_line(color = "red") +
  geom_line(aes(y=subset_test1_weekday_morning$moving_morning_GA_test1_weekday)) + geom_line(aes(y=subset_test1_weekday_morning$calculated_GA_difference_morning), color = 'orange') +
  ggtitle("Global_active_power vs Moving Average for 2010-10-09 5:30am to 9:30 am ") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = subset_test1_weekday_morning, aes(x=Time, y=Global_intensity, group = 1)) + geom_line(color = "red") +
  geom_line(aes(y=subset_test1_weekday_morning$moving_morning_GI_test1_weekday)) + geom_line(aes(y=subset_test1_weekday_morning$calculated_GI_difference_morning), color = 'orange') +
  ggtitle("Global_intensity vs Moving Average for 2010-10-09 5:30am to 9:30 am ") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = subset_test1_weekday_evening, aes(x=Time, y=Global_active_power, group = 1)) + geom_line(color = "red") +
  geom_line(aes(y=subset_test1_weekday_evening$moving_evening_GA_test1_weekday)) + geom_line(aes(y=subset_test1_weekday_evening$calculated_GA_difference_evening), color = 'orange') +
  ggtitle("Global_active_power vs Moving Average for 2010-10-09 5:30pm to 9:30 pm ") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = subset_test1_weekday_evening, aes(x=Time, y=Global_active_power, group = 1)) + geom_line(color = "red") +                                            
  geom_line(aes(y=subset_test1_weekday_evening$moving_evening_GA_test1_weekday)) + geom_line(aes(y=subset_test1_weekday_evening$calculated_GI_difference_evening), color = 'orange') +
  ggtitle("Global_intensity vs Moving Average for 2010-10-09 5:30pm to 9:30 pm ") +
  theme(plot.title = element_text(hjust = 0.5))

#calculate the threshold and determine the number of anomalies
test1_GA_morning_threshold_positive = 0.5
test1_GA_morning_threshold_negative = -0.5
count_GA_anomalies_test1_weekday_morning <- 0
for (i in 1: nrow(subset_test1_weekday_morning)) {
  if (!is.na(subset_test1_weekday_morning$calculated_GA_difference_morning[i])) {
    if (subset_test1_weekday_morning$calculated_GA_difference_morning[i] > test1_GA_morning_threshold_positive || subset_test1_weekday_morning$calculated_GA_difference_morning[i] < test1_GA_morning_threshold_negative) {
      count_GA_anomalies_test1_weekday_morning <- count_GA_anomalies_test1_weekday_morning + 1
    }
  }
}

test1_GA_evening_threshold_positive = 0.5
test1_GA_evening_threshold_negative = -0.5
count_GA_anomalies_test1_weekday_evening <- 0
for (i in 1: nrow(subset_test1_weekday_evening)) {
  if (!is.na(subset_test1_weekday_evening$calculated_GA_difference_evening[i])) {
    if (subset_test1_weekday_evening$calculated_GA_difference_evening[i] > test1_GA_evening_threshold_positive || subset_test1_weekday_evening$calculated_GA_difference_evening[i] < test1_GA_evening_threshold_negative) {
      count_GA_anomalies_test1_weekday_evening <- count_GA_anomalies_test1_weekday_evening + 1
    }
  }
}


test1_GI_morning_threshold_positive = 0.5
test1_GI_morning_threshold_negative = -0.5
count_GI_anomalies_test1_weekday_morning <- 0
for (i in 1: nrow(subset_test1_weekday_morning)) {
  if (!is.na(subset_test1_weekday_morning$calculated_GI_difference_morning[i])) {
    if (subset_test1_weekday_morning$calculated_GI_difference_morning[i] > test1_GI_morning_threshold_positive || subset_test1_weekday_morning$calculated_GI_difference_morning[i] < test1_GI_morning_threshold_negative) {
      count_GI_anomalies_test1_weekday_morning <- count_GI_anomalies_test1_weekday_morning + 1
    }
  }
}

test1_GI_evening_threshold_positive = 0.5
test1_GI_evening_threshold_negative = -0.5
count_GI_anomalies_test1_weekday_evening <- 0
for (i in 1: nrow(subset_test1_weekday_evening)) {
  if (!is.na(subset_test1_weekday_evening$calculated_GI_difference_evening[i])) {
    if (subset_test1_weekday_evening$calculated_GI_difference_evening[i] > test1_GI_evening_threshold_positive || subset_test1_weekday_evening$calculated_GI_difference_evening[i] < test1_GI_evening_threshold_negative) {
      count_GI_anomalies_test1_weekday_evening <- count_GI_anomalies_test1_weekday_evening + 1
    }
  }
}


# weekend - 2010-10-13 sunday -----------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
moving_morning_GA_test1_weekend<- ma(test1_filter_morning$Global_active_power, order = 20)
moving_evening_GA_test1_weekend <- ma(test1_filter_evening$Global_active_power, order = 20)
moving_morning_GI_test1_weekend <- ma(test1_filter_morning$Global_intensity, order = 20)
moving_evening_GI_test1_weekend <- ma(test1_filter_evening$Global_intensity, order = 20)

##### store these moving average to the table 
test1_filter_morning$moving_morning_GA_test1_weekend <- moving_morning_GA_test1_weekend
test1_filter_evening$moving_evening_GA_test1_weekend <- moving_evening_GA_test1_weekend
test1_filter_morning$moving_morning_GI_test1_weekend <- moving_morning_GI_test1_weekend
test1_filter_evening$moving_evening_GI_test1_weekend <- moving_evening_GI_test1_weekend
# weekend - 2010-10-13 sunday -----------------------------------------------------------------------------------------------------------------------------------------------------------
subset_test1_weekend_morning<-subset(test1_filter_morning, test1_filter_morning$Week == 41 & test1_filter_morning$Day == 0 & test1_filter_morning$Year == 2010)
subset_test1_weekend_evening<-subset(test1_filter_evening, test1_filter_evening$Week == 41 & test1_filter_evening$Day == 0 & test1_filter_evening$Year == 2010)

##### calculate the difference between observations and moving average
# morning GA
for (i in 1: nrow(subset_test1_weekend_morning)) {                                                                                                                 
  calculated_GA_difference_morning_weekend <- (subset_test1_weekend_morning$Global_active_power[i] - subset_test1_weekend_morning$moving_morning_GA_test1_weekend[i])                                        
  subset_test1_weekend_morning$calculated_GA_difference_morning[i] <- calculated_GA_difference_morning_weekend                                                                                                        
}
# eveing GA
for (i in 1: nrow(subset_test1_weekend_evening)) {                                                                                                                 
  calculated_GA_difference_evening_weekend <- (subset_test1_weekend_evening$Global_active_power[i] - subset_test1_weekend_evening$moving_evening_GA_test1_weekend[i])                                        
  subset_test1_weekend_evening$calculated_GA_difference_evening[i] <- calculated_GA_difference_evening_weekend                                                                                                        
}
# morning GI
for (i in 1: nrow(subset_test1_weekend_morning)) {                                                                                                                 
  calculated_GI_difference_morning_weekend <- (subset_test1_weekend_morning$Global_intensity[i] - subset_test1_weekend_morning$moving_morning_GI_test1_weekend[i])                                        
  subset_test1_weekend_morning$calculated_GI_difference_morning[i] <- calculated_GI_difference_morning_weekend                                                                                                        
}
# evening GI
for (i in 1: nrow(subset_test1_weekend_evening)) {                                                                                                                 
  calculated_GI_difference_evening_weekend <- (subset_test1_weekend_evening$Global_intensity[i] - subset_test1_weekend_evening$moving_evening_GI_test1_weekend[i])                                        
  subset_test1_weekend_evening$calculated_GI_difference_evening[i] <- calculated_GI_difference_evening_weekend                                                                                                        
}

#plot 3 graphs, red - observations, black - moving average, orange - difference between the two
ggplot(data = subset_test1_weekend_morning, aes(x=Time, y=Global_active_power, group = 1)) + geom_line(color = "red") +
  geom_line(aes(y=subset_test1_weekend_morning$moving_morning_GA_test1_weekend)) + geom_line(aes(y=subset_test1_weekend_morning$calculated_GA_difference_morning), color = 'orange') +
  ggtitle("Global_active_power vs Moving Average for 2010-10-13 5:30am to 9:30 am ") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = subset_test1_weekend_morning, aes(x=Time, y=Global_intensity, group = 1)) + geom_line(color = "red") +
  geom_line(aes(y=subset_test1_weekend_morning$moving_morning_GI_test1_weekend)) + geom_line(aes(y=subset_test1_weekend_morning$calculated_GI_difference_morning), color = 'orange') +
  ggtitle("Global_intensity vs Moving Average for 2010-10-13 5:30am to 9:30 am ") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = subset_test1_weekend_evening, aes(x=Time, y=Global_active_power, group = 1)) + geom_line(color = "red") +
  geom_line(aes(y=subset_test1_weekend_evening$moving_evening_GA_test1_weekend)) + geom_line(aes(y=subset_test1_weekend_evening$calculated_GA_difference_evening), color = 'orange') +
  ggtitle("Global_active_power vs Moving Average for 2010-10-13 5:30pm to 9:30 pm ") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = subset_test1_weekend_evening, aes(x=Time, y=Global_active_power, group = 1)) + geom_line(color = "red") +                                            
  geom_line(aes(y=subset_test1_weekend_evening$moving_evening_GA_test1_weekend)) + geom_line(aes(y=subset_test1_weekend_evening$calculated_GI_difference_evening), color = 'orange') +
  ggtitle("Global_intensity vs Moving Average for 2010-10-13 5:30pm to 9:30 pm ") +
  theme(plot.title = element_text(hjust = 0.5))

#calculate the threshold and determine the number of anomalies
test1_GA_morning_threshold_positive_weekend = 0.5
test1_GA_morning_threshold_negative_weekend = -0.5
count_GA_anomalies_test1_weekend_morning <- 0
for (i in 1: nrow(subset_test1_weekend_morning)) {
  if (!is.na(subset_test1_weekend_morning$calculated_GA_difference_morning[i])) {
    if (subset_test1_weekend_morning$calculated_GA_difference_morning[i] > test1_GA_morning_threshold_positive_weekend || subset_test1_weekend_morning$calculated_GA_difference_morning[i] < test1_GA_morning_threshold_negative_weekend) {
      count_GA_anomalies_test1_weekend_morning <- count_GA_anomalies_test1_weekend_morning + 1
    }
  }
}

test1_GA_evening_threshold_positive_weekend = 0.5
test1_GA_evening_threshold_negative_weekend = -0.5
count_GA_anomalies_test1_weekend_evening <- 0
for (i in 1: nrow(subset_test1_weekend_evening)) {
  if (!is.na(subset_test1_weekend_evening$calculated_GA_difference_evening[i])) {
    if (subset_test1_weekend_evening$calculated_GA_difference_evening[i] > test1_GA_evening_threshold_positive_weekend || subset_test1_weekend_evening$calculated_GA_difference_evening[i] < test1_GA_evening_threshold_negative_weekend) {
      count_GA_anomalies_test1_weekend_evening <- count_GA_anomalies_test1_weekend_evening + 1
    }
  }
}


test1_GI_morning_threshold_positive_weekend = 0.5
test1_GI_morning_threshold_negative_weekend = -0.5
count_GI_anomalies_test1_weekend_morning <- 0
for (i in 1: nrow(subset_test1_weekend_morning)) {
  if (!is.na(subset_test1_weekend_morning$calculated_GI_difference_morning[i])) {
    if (subset_test1_weekend_morning$calculated_GI_difference_morning[i] > test1_GI_morning_threshold_positive_weekend || subset_test1_weekend_morning$calculated_GI_difference_morning[i] < test1_GI_morning_threshold_negative_weekend) {
      count_GI_anomalies_test1_weekend_morning <- count_GI_anomalies_test1_weekend_morning + 1
    }
  }
}

test1_GI_evening_threshold_positive_weekend = 0.5
test1_GI_evening_threshold_negative_weekend = -0.5
count_GI_anomalies_test1_weekend_evening <- 0
for (i in 1: nrow(subset_test1_weekend_evening)) {
  if (!is.na(subset_test1_weekend_evening$calculated_GI_difference_evening[i])) {
    if (subset_test1_weekend_evening$calculated_GI_difference_evening[i] > test1_GI_evening_threshold_positive_weekend || subset_test1_weekend_evening$calculated_GI_difference_evening[i] < test1_GI_evening_threshold_negative_weekend) {
      count_GI_anomalies_test1_weekend_evening <- count_GI_anomalies_test1_weekend_evening + 1
    }
  }
}
