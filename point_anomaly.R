# library("depmixS4")
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
filter_evening <- subset(data, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))

# find min and max
max_morning_training_GA<-max(filter_morning$Global_active_power)
min_morning_training_GA<-min(filter_morning$Global_active_power)
max_morning_training_GI<-max(filter_morning$Global_intensity)
min_morning_training_GI<-min(filter_morning$Global_intensity)
max_evening_training_GA<-max(filter_evening$Global_active_power)
min_evening_training_GA<-min(filter_evening$Global_active_power)
max_evening_training_GI<-max(filter_evening$Global_intensity)
min_evening_training_GI<-min(filter_evening$Global_intensity)

# try with no time frame restriction 
# max_morning_training_GA<-max(data$Global_active_power)
# min_morning_training_GA<-min(data$Global_active_power)
# max_morning_training_GI<-max(data$Global_intensity)
# min_morning_training_GI<-min(data$Global_intensity)
# 
# max_evening_training_GA<-max(data$Global_active_power)
# min_evening_training_GA<-min(data$Global_active_power)
# max_evening_training_GI<-max(data$Global_intensity)
# min_evening_training_GI<-min(data$Global_intensity)

# ------------------------------------------------------- point anomaly detection by range for t1 -------------------------------------------------------------------------------------------------------------------
test1 <- read.csv(file="test1.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
test1 <- na.omit(data)
# test1$Year <- as.numeric(format(as.Date(test1$Date, "%d/%m/%Y"), "%Y"))
# test1$Day <- as.POSIXlt(test1$Date)$wday
# x_test1 <- paste(test1$Date, test1$Time)
# dateTime_test1 <- as.POSIXlt(x_test1, format = "%d/%m/%Y %H:%M:%S")
# test1$DateTime<-dateTime_test1
# test1$Week <- strftime(dateTime_test1, format = "%V")
# test1_filter_morning <- subset(test1, (strptime(Time, format = "%H:%M:%S") >= strptime("05:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("09:30:00", format = "%H:%M:%S")))
# test1_filter_evening <- subset(test1, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))
# test1_GA_outliers<-subset(test1_filter_morning, ((test1_filter_morning$Global_active_power>max_morning_training_GA)||(test1$Global_active_power<min_morning_training_GA)||(test1_filter_evening$Global_active_power>max_evening_training_GA)||(test1_filter_evening$Global_active_power<min_evening_training_GA)))
# test1_GI_outliers<-subset(test1_filter_morning, ((test1_filter_morning$Global_intensity>max_morning_training_GI)||(test1$Global_intensity<min_morning_training_GI)||(test1_filter_evening$Global_intensity>max_evening_training_GI)||(test1_filter_evening$Global_intensity<min_evening_training_GI)))

# Try no time frame restriction
test1_GA_outliers<-subset(test1, ((test1$Global_active_power>max_morning_training_GA)|(test1$Global_active_power<min_morning_training_GA)|(test1$Global_active_power>max_evening_training_GA)|(test1$Global_active_power<min_evening_training_GA)))
test1_GI_outliers<-subset(test1, ((test1$Global_intensity>max_morning_training_GI)|(test1$Global_intensity<min_morning_training_GI)|(test1$Global_intensity>max_evening_training_GI)|(test1$Global_intensity<min_evening_training_GI)))


# ------------------------------------------------------- point anomaly detection by range for t2 -------------------------------------------------------------------------------------------------------------------
test2 <- read.csv(file="test2.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
test2 <- na.omit(data)
test2$Year <- as.numeric(format(as.Date(test2$Date, "%d/%m/%Y"), "%Y"))
test2$Day <- as.POSIXlt(test2$Date)$wday
x_test2 <- paste(test2$Date, test2$Time)
dateTime_test2 <- as.POSIXlt(x_test2, format = "%d/%m/%Y %H:%M:%S")
test2$DateTime<-dateTime_test2
test2$Week <- strftime(dateTime_test2, format = "%V")
test2_filter_morning <- subset(test2, (strptime(Time, format = "%H:%M:%S") >= strptime("05:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("09:30:00", format = "%H:%M:%S")))
test2_filter_evening <- subset(test2, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))

test2_GA_outliers<-subset(test2_filter_morning, ((test2_filter_morning$Global_active_power>max_morning_training_GA)||(test2$Global_active_power<min_morning_training_GA)||(test2_filter_evening$Global_active_power>max_evening_training_GA)||(test2_filter_evening$Global_active_power<min_evening_training_GA)))
test2_GI_outliers<-subset(test2_filter_morning, ((test2_filter_morning$Global_intensity>max_morning_training_GI)||(test2$Global_intensity<min_morning_training_GI)||(test2_filter_evening$Global_intensity>max_evening_training_GI)||(test2_filter_evening$Global_intensity<min_evening_training_GI)))

# ------------------------------------------------------- point anomaly detection by range for t3------------------------------------------------------------------------------------------------------------------
test3 <- read.csv(file="test3.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
test3 <- na.omit(data)
test3$Year <- as.numeric(format(as.Date(test3$Date, "%d/%m/%Y"), "%Y"))
test3$Day <- as.POSIXlt(test3$Date)$wday
x_test3 <- paste(test3$Date, test3$Time)
dateTime_test3 <- as.POSIXlt(x_test3, format = "%d/%m/%Y %H:%M:%S")
test3$DateTime<-dateTime_test3
test3$Week <- strftime(dateTime_test3, format = "%V")
test3_filter_morning <- subset(test3, (strptime(Time, format = "%H:%M:%S") >= strptime("05:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("09:30:00", format = "%H:%M:%S")))
test3_filter_evening <- subset(test3, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))

test3_GA_outliers<-subset(test3_filter_morning, ((test3_filter_morning$Global_active_power>max_morning_training_GA)||(test3$Global_active_power<min_morning_training_GA)||(test3_filter_evening$Global_active_power>max_evening_training_GA)||(test3_filter_evening$Global_active_power<min_evening_training_GA)))
test3_GI_outliers<-subset(test3_filter_morning, ((test3_filter_morning$Global_intensity>max_morning_training_GI)||(test3$Global_intensity<min_morning_training_GI)||(test3_filter_evening$Global_intensity>max_evening_training_GI)||(test3_filter_evening$Global_intensity<min_evening_training_GI)))

# ------------------------------------------------------- point anomaly detection by range for t4 -------------------------------------------------------------------------------------------------------------------
test4 <- read.csv(file="test4.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
test4 <- na.omit(data)
test4$Year <- as.numeric(format(as.Date(test4$Date, "%d/%m/%Y"), "%Y"))
test4$Day <- as.POSIXlt(test4$Date)$wday
x_test4 <- paste(test4$Date, test4$Time)
dateTime_test4 <- as.POSIXlt(x_test4, format = "%d/%m/%Y %H:%M:%S")
test4$DateTime<-dateTime_test4
test4$Week <- strftime(dateTime_test4, format = "%V")
test4_filter_morning <- subset(test4, (strptime(Time, format = "%H:%M:%S") >= strptime("05:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("09:30:00", format = "%H:%M:%S")))
test4_filter_evening <- subset(test4, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))

test4_GA_outliers<-subset(test4_filter_morning, ((test4_filter_morning$Global_active_power>max_morning_training_GA)||(test4$Global_active_power<min_morning_training_GA)||(test4_filter_evening$Global_active_power>max_evening_training_GA)||(test4_filter_evening$Global_active_power<min_evening_training_GA)))
test4_GI_outliers<-subset(test4_filter_morning, ((test4_filter_morning$Global_intensity>max_morning_training_GI)||(test4$Global_intensity<min_morning_training_GI)||(test4_filter_evening$Global_intensity>max_evening_training_GI)||(test4_filter_evening$Global_intensity<min_evening_training_GI)))

# ------------------------------------------------------- point anomaly detection by range for t5 -------------------------------------------------------------------------------------------------------------------
test5 <- read.csv(file="test5.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
test5 <- na.omit(data)
test5$Year <- as.numeric(format(as.Date(test5$Date, "%d/%m/%Y"), "%Y"))
test5$Day <- as.POSIXlt(test5$Date)$wday
x_test5 <- paste(test5$Date, test5$Time)
dateTime_test5 <- as.POSIXlt(x_test5, format = "%d/%m/%Y %H:%M:%S")
test5$DateTime<-dateTime_test5
test5$Week <- strftime(dateTime_test5, format = "%V")
test5_filter_morning <- subset(test5, (strptime(Time, format = "%H:%M:%S") >= strptime("05:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("09:30:00", format = "%H:%M:%S")))
test5_filter_evening <- subset(test5, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))

test5_GA_outliers<-subset(test5_filter_morning, ((test5_filter_morning$Global_active_power>max_morning_training_GA)||(test5$Global_active_power<min_morning_training_GA)||(test5_filter_evening$Global_active_power>max_evening_training_GA)||(test5_filter_evening$Global_active_power<min_evening_training_GA)))
test5_GI_outliers<-subset(test5_filter_morning, ((test5_filter_morning$Global_intensity>max_morning_training_GI)||(test5$Global_intensity<min_morning_training_GI)||(test5_filter_evening$Global_intensity>max_evening_training_GI)||(test5_filter_evening$Global_intensity<min_evening_training_GI)))

# test2 <- read.csv(file="test2.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
# global_active_power_t2<-test1$Global_active_power
# global_intensity_t2<-test1$Global_intensity
# plot(global_active_power_t2,global_intensity_t2,pch=20,col="#1a8cff", cex=1.0, main="test2")
# 
# 
# test3 <- read.csv(file="test3.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
# global_active_power_t3<-test1$Global_active_power
# global_intensity_t3<-test1$Global_intensity
# plot(global_active_power_t3,global_intensity_t3,pch=20,col="#1a8cff", cex=1.0, main="test3")
# 
# 
# test4 <- read.csv(file="test4.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
# global_active_power_t4<-test1$Global_active_power
# global_intensity_t4<-test1$Global_intensity
# plot(global_active_power_t4,global_intensity_t4,pch=20,col="#1a8cff", cex=1.0, main="test4")
# 
# 
# test5 <- read.csv(file="test5.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
# global_active_power_t5<-test1$Global_active_power
# global_intensity_t5<-test1$Global_intensity
# plot(global_active_power_t5,global_intensity_t5,pch=20,col="#1a8cff", cex=1.0, main="test5")