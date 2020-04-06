library("psych")
library("ggplot2")
library("corrplot")
library("gridExtra")
library("fpp2")

data <- read.csv(file="TrainData.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
data <- na.omit(data)
data$Year <- as.numeric(format(as.Date(data$Date, "%d/%m/%Y"), "%Y"))
data$Day <- as.POSIXlt(data$Date)$wday
x <- paste(data$Date, data$Time)
dateTime <- as.POSIXlt(x, format = "%d/%m/%Y %H:%M:%S")
data$Week <- strftime(dateTime, format = "%V")
filter_morning <- subset(data, (strptime(Time, format = "%H:%M:%S") >= strptime("05:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("09:30:00", format = "%H:%M:%S")))
filter_evening <- subset(data, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("23:30:00", format = "%H:%M:%S")))
# plot(filter_morning$, filter_morning$Global_active_power)
# find min and max
max_morning_training_GA<-max(filter_morning$Global_active_power)
min_morning_training_GA<-min(filter_morning$Global_active_power)
max_morning_training_GI<-max(filter_morning$Global_intensity)
min_morning_training_GI<-min(filter_morning$Global_intensity)
max_evening_training_GA<-max(filter_evening$Global_active_power)
min_evening_training_GA<-min(filter_evening$Global_active_power)
max_evening_training_GI<-max(filter_evening$Global_intensity)
min_evening_training_GI<-min(filter_evening$Global_intensity)

# ------------------------------------------------------- point anomaly detection by range for t1 -------------------------------------------------------------------------------------------------------------------
test1 <- read.csv(file="test1.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
test1 <- na.omit(test1)
test1$Week <- strftime(dateTime_test1, format = "%V")
test1_filter_morning <- subset(test1, (strptime(Time, format = "%H:%M:%S") >= strptime("05:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("09:30:00", format = "%H:%M:%S")))
test1_filter_evening <- subset(test1, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))

test1_GA_outliers_morning<-subset(test1_filter_morning, test1_filter_morning$Global_active_power>max_morning_training_GA | test1_filter_morning$Global_active_power<min_morning_training_GA)
test1_GA_outliers_evening<-subset(test1_filter_evening, test1_filter_evening$Global_active_power>max_evening_training_GA | test1_filter_evening$Global_active_power<min_evening_training_GA)
test1_GI_outliers_morning<-subset(test1_filter_morning, test1_filter_morning$Global_intensity>max_morning_training_GI | test1_filter_morning$Global_intensity<min_morning_training_GI)
test1_GI_outliers_evening<-subset(test1_filter_evening, test1_filter_evening$Global_intensity>max_evening_training_GI | test1_filter_evening$Global_intensity<min_evening_training_GI)


# ------------------------------------------------------- point anomaly detection by range for t2 -------------------------------------------------------------------------------------------------------------------
test2 <- read.csv(file="test2.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
test2 <- na.omit(data)
test2_filter_morning <- subset(test2, (strptime(Time, format = "%H:%M:%S") >= strptime("05:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("09:30:00", format = "%H:%M:%S")))
test2_filter_evening <- subset(test2, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))

test2_GA_outliers_morning<-subset(test2_filter_morning, test2_filter_morning$Global_active_power>max_morning_training_GA | test2_filter_morning$Global_active_power<min_morning_training_GA)
test2_GA_outliers_evening<-subset(test2_filter_evening, test2_filter_evening$Global_active_power>max_evening_training_GA | test2_filter_evening$Global_active_power<min_evening_training_GA)
test2_GI_outliers_morning<-subset(test2_filter_morning, test2_filter_morning$Global_intensity>max_morning_training_GI | test2_filter_morning$Global_intensity<min_morning_training_GI)
test2_GI_outliers_evening<-subset(test2_filter_evening, test2_filter_evening$Global_intensity>max_evening_training_GI | test2_filter_evening$Global_intensity<min_evening_training_GI)


# ------------------------------------------------------- point anomaly detection by range for t3------------------------------------------------------------------------------------------------------------------
test3 <- read.csv(file="test3.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
test3 <- na.omit(data)
test3_filter_morning <- subset(test3, (strptime(Time, format = "%H:%M:%S") >= strptime("05:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("09:30:00", format = "%H:%M:%S")))
test3_filter_evening <- subset(test3, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))

test3_GA_outliers_morning<-subset(test3_filter_morning, test3_filter_morning$Global_active_power>max_morning_training_GA | test3_filter_morning$Global_active_power<min_morning_training_GA)
test3_GA_outliers_evening<-subset(test3_filter_evening, test3_filter_evening$Global_active_power>max_evening_training_GA | test3_filter_evening$Global_active_power<min_evening_training_GA)
test3_GI_outliers_morning<-subset(test3_filter_morning, test3_filter_morning$Global_intensity>max_morning_training_GI | test3_filter_morning$Global_intensity<min_morning_training_GI)
test3_GI_outliers_evening<-subset(test3_filter_evening, test3_filter_evening$Global_intensity>max_evening_training_GI | test3_filter_evening$Global_intensity<min_evening_training_GI)


# ------------------------------------------------------- point anomaly detection by range for t4 -------------------------------------------------------------------------------------------------------------------
test4 <- read.csv(file="test4.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
test4 <- na.omit(data)
test4_filter_morning <- subset(test4, (strptime(Time, format = "%H:%M:%S") >= strptime("05:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("09:30:00", format = "%H:%M:%S")))
test4_filter_evening <- subset(test4, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))

test4_GA_outliers_morning<-subset(test4_filter_morning, test4_filter_morning$Global_active_power>max_morning_training_GA | test4_filter_morning$Global_active_power<min_morning_training_GA)
test4_GA_outliers_evening<-subset(test4_filter_evening, test4_filter_evening$Global_active_power>max_evening_training_GA | test4_filter_evening$Global_active_power<min_evening_training_GA)
test4_GI_outliers_morning<-subset(test4_filter_morning, test4_filter_morning$Global_intensity>max_morning_training_GI | test4_filter_morning$Global_intensity<min_morning_training_GI)
test4_GI_outliers_evening<-subset(test4_filter_evening, test4_filter_evening$Global_intensity>max_evening_training_GI | test4_filter_evening$Global_intensity<min_evening_training_GI)


# ------------------------------------------------------- point anomaly detection by range for t5 -------------------------------------------------------------------------------------------------------------------
test5 <- read.csv(file="test5.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
test5 <- na.omit(data)
test5_filter_morning <- subset(test5, (strptime(Time, format = "%H:%M:%S") >= strptime("05:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("09:30:00", format = "%H:%M:%S")))
test5_filter_evening <- subset(test5, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))

test5_GA_outliers_morning<-subset(test5_filter_morning, test5_filter_morning$Global_active_power>max_morning_training_GA | test5_filter_morning$Global_active_power<min_morning_training_GA)
test5_GA_outliers_evening<-subset(test5_filter_evening, test5_filter_evening$Global_active_power>max_evening_training_GA | test5_filter_evening$Global_active_power<min_evening_training_GA)
test5_GI_outliers_morning<-subset(test5_filter_morning, test5_filter_morning$Global_intensity>max_morning_training_GI | test5_filter_morning$Global_intensity<min_morning_training_GI)
test5_GI_outliers_evening<-subset(test5_filter_evening, test5_filter_evening$Global_intensity>max_evening_training_GI | test5_filter_evening$Global_intensity<min_evening_training_GI)


