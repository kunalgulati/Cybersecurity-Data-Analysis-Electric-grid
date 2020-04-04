# ===============================
# CODE USED TO INSTALL GGBIPLOT
# library('devtools')
# install_github('vqv/ggbiplot')
# ===============================
library(ggbiplot)

data <- read.csv(file="TrainData.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
data <- na.omit(data)
data$Day <- as.POSIXlt(data$Date)$wday
x <- paste(data$Date, data$Time)
dateTime <- as.POSIXlt(x, format = "%d/%m/%Y %H:%M:%S")
data$Week <- strftime(dateTime, format = "%V")
data$Year <- as.numeric(format(as.Date(data$Date, "%d/%m/%Y"), "%Y"))

# filter for day and timeframe
filter_monday <- subset(data, Day == 04)
morning <- subset(filter_monday, (strptime(Time, format = "%H:%M:%S") >= strptime("05:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("09:30:00", format = "%H:%M:%S")))
evening <- subset(filter_monday, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))

agg_morning <- aggregate(list(
  GAP = morning$Global_active_power, 
  GRP = morning$Global_reactive_power,
  Volt = morning$Voltage,
  GI = morning$Global_intensity,
  SM1 = morning$Sub_metering_1,
  SM2 = morning$Sub_metering_2,
  SM3 = morning$Sub_metering_3), 
  by = list(Week = morning$Week, Year = morning$Year), 
  mean
)

agg_evening <- aggregate(list(
  GAP = evening$Global_active_power, 
  GRP = evening$Global_reactive_power,
  Volt = evening$Voltage,
  GI = evening$Global_intensity,
  SM1 = evening$Sub_metering_1,
  SM2 = evening$Sub_metering_2,
  SM3 = evening$Sub_metering_3), 
  by = list(Week = evening$Week, Year = evening$Year), 
  mean
)

agg_morning <- subset(agg_morning, select = -c(Week, Year))
agg_evening <- subset(agg_evening, select = -c(Week, Year))

# apply pca
agg_morning.pca <- prcomp(agg_morning, scale = TRUE)
summary(agg_morning.pca)

agg_evening.pca <- prcomp(agg_evening, scale = TRUE)
summary(agg_evening.pca)

# graph
p_morn_12 <- ggbiplot(agg_morning.pca)
p_morn_12 + ggtitle('Morning PC1 vs PC2')

p_evening_12 <- ggbiplot(agg_evening.pca)
p_evening_12 + ggtitle('Evening PC1 vs PC2')

p_morning_34 <- ggbiplot(agg_morning.pca, choices = c(3,4))
p_morning_34 + ggtitle('Morning PC3 vs PC4')

p_evening_34 <-ggbiplot(agg_evening.pca, choices = c(3,4))
p_evening_34 + ggtitle('Evening PC3 vs PC4')

