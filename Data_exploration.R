library("ggplot2")
library("corrplot")

data <- read.csv(file="TrainData.txt", header=TRUE, sep=",", na.strings = c("", "NA"))


x <- paste(data$Date, data$Time)
dateTime <- as.POSIXlt(x, format = "%d/%m/%Y %H:%M:%S")

############################################################ WEEKDAY -> MORNINGS GAP ###########################################################################
############################################################ WEEKDAY -> MORNINGS GAP ###########################################################################

#Filter weekdays
data$weekday <- strftime(dateTime, format = "%u")

# Derive all weekdays from all weeks
Weekdays_Mor_GAP <- subset(data, weekday == '1' | weekday == '2' | weekday == '3' | weekday == '4' | weekday == '5')

#filter out NA values
filtered_wd_mor_GAP <- subset(Weekdays_Mor_GAP, is.na(Global_active_power) == FALSE)

filtered_agg_wd_mor_GAP <- aggregate(list(Global_active_power = filtered_wd_mor_GAP$Global_active_power), by=list(Time = filtered_wd_mor_GAP$Time), mean)


#time window 5:30 am - 9:30 am weekday mornings
time_filter_wd_mor_GAP <- subset(filtered_agg_wd_mor_GAP, (strptime(Time, format = "%H:%M:%S") >= strptime("5:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("9:30:00", format = "%H:%M:%S")))

#Plot Global Active Power vs Time for time window (5:30 am - 9:30 am)
plot(time_filter_wd_mor_GAP$Time, time_filter_wd_mor_GAP$Global_active_power, main = "Global Active Power Consumption on Weekdays Mornings from 5:30AM to 9:30AM", xlab="Time", ylab="Global Active Power")

############################################################ WEEKDAY -> MORNINGS GI ###########################################################################
############################################################ WEEKDAY -> MORNINGS GI ###########################################################################

#Filter weekdays
data$weekday <- strftime(dateTime, format = "%u")

# Derive all weekdays from all weeks
Weekdays_Mor_GI <- subset(data, weekday == '1' | weekday == '2' | weekday == '3' | weekday == '4' | weekday == '5')

#filter out NA values
filtered_wd_mor_GI <- subset(Weekdays_Mor_GI, is.na(Global_intensity) == FALSE)
filtered_agg_wd_mor_GI <- aggregate(list(Global_intensity = filtered_wd_mor_GI$Global_intensity), by=list(Time = filtered_wd_mor_GI$Time), mean)


#time window 5:30 am - 9:30 am weekday mornings
time_filter_wd_mor_GI <- subset(filtered_agg_wd_mor_GI, (strptime(Time, format = "%H:%M:%S") >= strptime("5:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("9:30:00", format = "%H:%M:%S")))

#Plot Global Intensity vs Time for time window (5:30 am - 9:30 am)
plot(time_filter_wd_mor_GI$Time, time_filter_wd_mor_GI$Global_intensity, main = "Global Intensity Consumption on Weekdays Mornings from 5:30AM to 9:30AM", xlab="Time", ylab="Global Intensity")


############################################################ WEEKDAY -> EVENINGS GAP ###########################################################################
############################################################ WEEKDAY -> EVENINGS GAP ###########################################################################

#Filter weekdays
data$weekday <- strftime(dateTime, format = "%u")

# Derive all weekdays from all weeks
Weekdays_Eve_GAP <- subset(data, weekday == '1' | weekday == '2' | weekday == '3' | weekday == '4' | weekday == '5')

#filter out NA values
filtered_wd_eve_GAP <- subset(Weekdays_Eve_GAP, is.na(Global_active_power) == FALSE)
filtered_agg_wd_eve_GAP <- aggregate(list(Global_active_power = filtered_wd_eve_GAP$Global_active_power), by=list(Time = filtered_wd_eve_GAP$Time), mean)


#time window 5:30 pm - 9:30 pm weekday evenings
time_filter_wd_eve_GAP <- subset(filtered_agg_wd_eve_GAP, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))

#Plot Global Active Power vs Time for time window (5:30 pm - 9:30 pm)
plot(time_filter_wd_eve_GAP$Time, time_filter_wd_eve_GAP$Global_active_power, main = "Global Active Power Consumption on Weekdays Evenings from 5:30PM to 9:30PM", xlab="Time", ylab="Global Active Power")

############################################################ WEEKDAY -> EVENINGS GI ###########################################################################
############################################################ WEEKDAY -> EVENINGS GI ###########################################################################

#Filter weekdays
data$weekday <- strftime(dateTime, format = "%u")

# Derive all weekdays from all weeks
Weekdays_Eve_GI <- subset(data, weekday == '1' | weekday == '2' | weekday == '3' | weekday == '4' | weekday == '5')

#filter out NA values
filtered_wd_eve_GI <- subset(Weekdays_Eve_GI, is.na(Global_intensity) == FALSE)
filtered_agg_wd_eve_GI <- aggregate(list(Global_intensity = filtered_wd_eve_GI$Global_intensity), by=list(Time = filtered_wd_eve_GI$Time), mean)


#time window 5:30 pm - 9:30 pm weekday evenings
time_filter_wd_eve_GI<- subset(filtered_agg_wd_eve_GI, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))

#Plot Global Intensity vs Time for time window (5:30 pm - 9:30 pm)
plot(time_filter_wd_eve_GI$Time, time_filter_wd_eve_GI$Global_intensity, main = "Global Intensity Consumption on Weekdays Evenings from 5:30PM to 9:30PM", xlab="Time", ylab="Global Intensity")


############################################################ WEEKDAY -> AVERAGE GAP ############################################################
############################################################ WEEKDAY -> AVERAGE GAP ############################################################

#Filter weekdays
data$weekday <- strftime(dateTime, format = "%u")

# Derive all weekdays from all weeks
Weekdays_Avg_GAP <- subset(data, weekday == '1' | weekday == '2' | weekday == '3' | weekday == '4' | weekday == '5')

#filter out NA values
filtered_wd_GAP <- subset(Weekdays_Avg_GAP, is.na(Global_active_power) == FALSE)
filtered_agg_wd_GAP <- aggregate(list(Global_active_power = filtered_wd_GAP$Global_active_power), by=list(Time = filtered_wd_GAP$Time), mean)

#Plot Average Global Active Power vs Time Graph for weekdays
plot(filtered_agg_wd_GAP$Time, filtered_agg_wd_GAP$Global_active_power, main = "Global Active Power Consumption on Weekdays", xlab="Time", ylab="Global Active Power")

############################################################ WEEKDAY -> AVERAGE GI ############################################################
############################################################ WEEKDAY -> AVERAGE GI ############################################################

#Filter weekdays
data$weekday <- strftime(dateTime, format = "%u")

#Derive all weekdays from all weeks
Weekdays_Avg_GI <- subset(data, weekday == '1' | weekday == '2' | weekday == '3' | weekday == '4' | weekday == '5')

#filter out NA values
filtered_wd_GI <- subset(Weekdays_Avg_GI, is.na(Global_intensity) == FALSE)
filtered_agg_wd_GI <- aggregate(list(Global_intensity = filtered_wd_GI$Global_intensity), by=list(Time = filtered_wd_GI$Time), mean)

#Plot Average Global Intensity vs Time Graph for weekdays
plot(filtered_agg_wd_GI$Time, filtered_agg_wd_GI$Global_intensity, main = "Global Intensity Consumption on Weekdays", xlab="Time", ylab="Global Intensity")


############################################################ WEEKDAY -> CORRELATION FOR MORNINGS ############################################################
############################################################ WEEKDAY -> CORRELATION FOR MORNINGS ############################################################

#Filter weekdays
data$weekday <- strftime(dateTime, format = "%u")

#Derive all weekdays from all weeks
Weekdays_Mor_Corr <- subset(data, weekday == '1' | weekday == '2' | weekday == '3' | weekday == '4' | weekday == '5')

#filter out NA values
filtered_wd_mor_cor_GAP <- subset(Weekdays_Mor_Corr, is.na(Global_active_power) == FALSE)

#time window 5:30 am - 9:30 am on Weekdays Mornings
time_filter_wd_mor_Cor_GAP <- subset(filtered_wd_mor_cor_GAP, (strptime(Time, format = "%H:%M:%S") >= strptime("5:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("9:30:00", format = "%H:%M:%S")))

#correlation graph of Weekday Mornings from 5:30 am - 9:30 am
matrix_data_wd_Mor <- time_filter_wd_mor_Cor_GAP[, c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1","Sub_metering_2","Sub_metering_3")]
matrix_data_wd_Mor <- na.omit(matrix_data_wd_Mor)
display_wd_Mor <- cor(matrix_data_wd_Mor, method = "pearson")
print(display_wd_Mor)

corrplot(display_wd_Mor, method = "circle")


############################################################ WEEKDAY -> CORRELATION FOR EVENINGS ############################################################
############################################################ WEEKDAY -> CORRELATION FOR EVENINGS ############################################################

#Filter weekdays
data$weekday <- strftime(dateTime, format = "%u")

#Derive all weekdays from all weeks
Weekdays_Eve_Corr <- subset(data, weekday == '1' | weekday == '2' | weekday == '3' | weekday == '4' | weekday == '5')

#filter out NA values
filtered_wd_eve_cor_GAP <- subset(Weekdays_Eve_Corr, is.na(Global_active_power) == FALSE)

#time window 5:30 pm - 9:30 pm on Weekdays Evenings
time_filter_wd_eve_Cor_GAP <- subset(filtered_wd_eve_cor_GAP, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))

#correlation graph of Weekday Evenings from 5:30 pm - 9:30 pm
matrix_data_wd_Eve <- time_filter_wd_eve_Cor_GAP[, c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1","Sub_metering_2","Sub_metering_3")]
matrix_data_wd_Eve <- na.omit(matrix_data_wd_Eve)
display_wd_Eve <- cor(matrix_data_wd_Eve, method = "pearson")
print(display_wd_Eve)

corrplot(display_wd_Eve, method = "circle")

############################################################ WEEKEND -> AVERAGE GAP ############################################################
############################################################ WEEKEND -> AVERAGE GAP ############################################################

#Filter weekdays
data$weekday <- strftime(dateTime, format = "%u")

#Derive Saturdays and Sundays from all weeks
Weekends_Avg_GAP <- subset(data, weekday == '6' | weekday == '7')

#filter out NA values
filtered_wknd_GAP <- subset(Weekends_Avg_GAP, is.na(Global_active_power) == FALSE)
filtered_agg_wknd_GAP <- aggregate(list(Global_active_power = filtered_wknd_GAP$Global_active_power), by=list(Time = filtered_wknd_GAP$Time), mean)

#Plot Average Global Active Power vs Time Graph for weekends
plot(filtered_agg_wknd_GAP$Time, filtered_agg_wknd_GAP$Global_active_power, main = "Global Active Power Consumption on Weekends", xlab="Time", ylab="Global Active Power")

############################################################ WEEKEND -> AVERAGE GI ############################################################
############################################################ WEEKEND -> AVERAGE GI ############################################################

#Filter weekdays
data$weekday <- strftime(dateTime, format = "%u")

#Derive Saturdays and Sundays from all weeks
Weekends_Avg_GI <- subset(data, weekday == '6' | weekday == '7')

#filter out NA values
filtered_wknd_GI <- subset(Weekends_Avg_GI, is.na(Global_intensity) == FALSE)
filtered_agg_wknd_GI <- aggregate(list(Global_intensity = filtered_wknd_GI$Global_intensity), by=list(Time = filtered_wknd_GI$Time), mean)

#Plot Average Global Intensity vs Time Graph for weekends
plot(filtered_agg_wknd_GI$Time, filtered_agg_wknd_GI$Global_intensity, main = "Global Intensity Consumption on Weekends", xlab="Time", ylab="Global Intensity")

############################################################ WEEKEND -> MORNINGS GAP ############################################################
############################################################ WEEKEND -> MORNINGS GAP ############################################################

#Filter weekdays
data$weekday <- strftime(dateTime, format = "%u")

#Derive Saturdays and Sundays from all weeks
Weekends_Mor_GAP <- subset(data, weekday == '6' | weekday == '7')

#filter out NA values
filtered_wknd_mor_GAP <- subset(Weekends_Mor_GAP, is.na(Global_active_power) == FALSE)
filtered_agg_wknd_mor_GAP <- aggregate(list(Global_active_power = filtered_wknd_mor_GAP$Global_active_power), by=list(Time = filtered_wknd_mor_GAP$Time), mean)


#time window 5:30 am - 9:30 am on Weekend Mornings
time_filter_wknd_mor_GAP <- subset(filtered_agg_wknd_mor_GAP, (strptime(Time, format = "%H:%M:%S") >= strptime("5:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("9:30:00", format = "%H:%M:%S")))

#Plot Global Active Power vs Time for weekend mornings (5:30 am - 9:30 am)
plot(time_filter_wknd_mor_GAP$Time, time_filter_wknd_mor_GAP$Global_active_power, main = "Global Active Power Consumption on Weekend Mornings from 5:30AM to 9:30AM", xlab="Time", ylab="Global Active Power")

############################################################ WEEKEND -> EVENINGS GAP ############################################################
############################################################ WEEKEND -> EVENINGS GAP ############################################################

#Filter weekdays
data$weekday <- strftime(dateTime, format = "%u")

#Derive Saturdays and Sundays from all weeks
Weekends_Eve_GAP <- subset(data, weekday == '6' | weekday == '7')

#filter out NA values
filtered_wknd_eve_GAP <- subset(Weekends_Eve_GAP, is.na(Global_active_power) == FALSE)
filtered_agg_wknd_eve_GAP <- aggregate(list(Global_active_power = filtered_wknd_eve_GAP$Global_active_power), by=list(Time = filtered_wknd_eve_GAP$Time), mean)


#time window 5:30 pm - 9:30 pm on Weekend Evenings
time_filter_wknd_eve_GAP <- subset(filtered_agg_wknd_eve_GAP, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))

#Plot Global Active Power vs Time for weekend evenings (5:30 pm - 9:30 pm)
plot(time_filter_wknd_eve_GAP$Time, time_filter_wknd_eve_GAP$Global_active_power, main = "Global Active Power Consumption on Weekend Evenings from 5:30PM to 9:30PM", xlab="Time", ylab="Global Active Power")

############################################################ WEEKEND -> MORNINGS GI ############################################################
############################################################ WEEKEND -> MORNINGS GI ############################################################

#Filter weekdays
data$weekday <- strftime(dateTime, format = "%u")

#Derive Saturdays and Sundays from all weeks
Weekends_Mor_GI <- subset(data, weekday == '6' | weekday == '7')

#filter out NA values
filtered_wknd_mor_GI <- subset(Weekends_Mor_GI, is.na(Global_intensity) == FALSE)
filtered_agg_wknd_mor_GI <- aggregate(list(Global_intensity = filtered_wknd_mor_GI$Global_intensity), by=list(Time = filtered_wknd_mor_GI$Time), mean)


#time window 5:30 am - 9:30 am on Weekend Mornings
time_filter_wknd_mor_GI <- subset(filtered_agg_wknd_mor_GI, (strptime(Time, format = "%H:%M:%S") >= strptime("5:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("9:30:00", format = "%H:%M:%S")))

#Plot Global Intensity vs Time for weekend mornings (5:30 am - 9:30 am)
plot(time_filter_wknd_mor_GI$Time, time_filter_wknd_mor_GI$Global_intensity, main = "Global Intensity Consumption on Weekend Mornings from 5:30AM to 9:30AM", xlab="Time", ylab="Global Intensity")

############################################################ WEEKEND -> EVENINGS GI ############################################################
############################################################ WEEKEND -> EVENINGS GI ############################################################

#Filter weekdays
data$weekday <- strftime(dateTime, format = "%u")

#Derive Saturdays and Sundays from all weeks
Weekends_Eve_GI <- subset(data, weekday == '6' | weekday == '7')

#filter out NA values
filtered_wknd_eve_GI <- subset(Weekends_Eve_GI, is.na(Global_intensity) == FALSE)
filtered_agg_wknd_eve_GI <- aggregate(list(Global_intensity = filtered_wknd_eve_GI$Global_intensity), by=list(Time = filtered_wknd_eve_GI$Time), mean)


#time window 5:30 pm - 9:30 pm on Weekend Evenings
time_filter_wknd_eve_GI <- subset(filtered_agg_wknd_eve_GI, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))

#Plot Global Intensity vs Time for weekend evenings (5:30 pm - 9:30 pm)
plot(time_filter_wknd_eve_GI$Time, time_filter_wknd_eve_GI$Global_intensity, main = "Global Intensity Consumption on Weekend Evenings from 5:30PM to 9:30PM", xlab="Time", ylab="Global Intenisty")

############################################################ WEEKEND -> CORRELATION FOR MORNINGS ############################################################
############################################################ WEEKEND -> CORRELATION FOR MORNINGS ############################################################

#Filter weekdays
data$weekday <- strftime(dateTime, format = "%u")

#Derive Saturdays and Sundays from all weeks
Weekends_Mor_Corr <- subset(data, weekday == '6' | weekday == '7')

#filter out NA values
filtered_wknd_mor_cor_GAP <- subset(Weekends_Mor_Corr, is.na(Global_active_power) == FALSE)

#time window 5:30 am - 9:30 am on Weekend Mornings
time_filter_wknd_mor_cor_GAP <- subset(filtered_wknd_mor_cor_GAP, (strptime(Time, format = "%H:%M:%S") >= strptime("5:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("9:30:00", format = "%H:%M:%S")))

#correlation graph of Weekend Mornings from 5:30 am - 9:30 am
matrix_data_wknd_Mor <- time_filter_wknd_mor_cor_GAP[, c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1","Sub_metering_2","Sub_metering_3")]
matrix_data_wknd_Mor <- na.omit(matrix_data_wknd_Mor)
display_wknd_Mor <- cor(matrix_data_wknd_Mor, method = "pearson")
print(display_wknd_Mor)

corrplot(display_wknd_Mor, method = "circle")

############################################################ WEEKEND -> CORRELATION FOR EVENINGS ############################################################
############################################################ WEEKEND -> CORRELATION FOR EVENINGS ############################################################

#Filter weekdays
data$weekday <- strftime(dateTime, format = "%u")

#Derive Saturdays and Sundays from all weeks
Weekends_Eve_Corr <- subset(data, weekday == '6' | weekday == '7')

#filter out NA values
filtered_wknd_eve_cor_GAP <- subset(Weekends_Eve_Corr, is.na(Global_active_power) == FALSE)

#time window 5:30 pm - 9:30 pm on Weekend Evenings
time_filter_wknd_eve_cor_GAP <- subset(filtered_wknd_eve_cor_GAP, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))

#correlation graph of Weekend Evenings from 5:30 pm - 9:30 pm
matrix_data_wknd_Eve <- time_filter_wknd_eve_cor_GAP[, c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1","Sub_metering_2","Sub_metering_3")]
matrix_data_wknd_Eve <- na.omit(matrix_data_wknd_Eve)
display_wknd_Eve <- cor(matrix_data_wknd_Eve, method = "pearson")
print(display_wknd_Eve)

corrplot(display_wknd_Eve, method = "circle")



