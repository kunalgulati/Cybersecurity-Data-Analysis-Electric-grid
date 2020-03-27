library("depmixS4")
library("psych")
library("ggplot2")
library("corrplot")
library("gridExtra")
library("fpp2")

# Determine features for multivariate HMM analysis
# SUBPART 1 : Select Global Active Power (main feature) and Global Intensity as features - split train and test data
data <- read.csv(file="TrainData.txt", header=TRUE, sep=",", na.strings = c("", "NA"))

data <- na.omit(data)
# row_id <- rownames(data)
# data <- cbind(id=row_id,data)

# Result from part 1 indicate that GAP and Global Intensity are highly correlated
data$Year <- as.numeric(format(as.Date(data$Date, "%d/%m/%Y"), "%Y"))
data$Day <- as.POSIXlt(data$Date)$wday
x <- paste(data$Date, data$Time)
dateTime <- as.POSIXlt(x, format = "%d/%m/%Y %H:%M:%S")

# Derive week and filter for week 7
data$Week <- strftime(dateTime, format = "%V")


# Filter timeframe 1 and shrink data further (i.e. choose specific day, can also aggregate data)
# only want the odd rows 
# Two filters for Morning => 5:30 to 9:30 && 17:30 to 21:30
filter_morning <- subset(data, (strptime(Time, format = "%H:%M:%S") >= strptime("05:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("09:30:00", format = "%H:%M:%S")))
filter_evening <- subset(data, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))

# ========================================================= Created TRAINING Data set ========================================================= #
# Filtering data by Morning and Evening
training_morning <- subset(filter_morning, filter_morning$Year != 2009)
training_evening <- subset(filter_evening, filter_evening$Year != 2009)
# Aggregate the data for Morning and Evening
training_morning <- aggregate(list(Global_active_power = training_morning$Global_active_power, Global_intensity = training_morning$Global_intensity), by=list(Week = training_morning$Week, Day = training_morning$Day), mean)
training_evening <- aggregate(list(Global_active_power = training_evening$Global_active_power, Global_intensity = training_evening$Global_intensity), by=list(Week = training_evening$Week, Day = training_evening$Day), mean)


# ========================================================= Created TEST Data set ========================================================= #
# Filtering data by morning and evening
test_morning <- subset(filter_morning, filter_morning$Year == 2009)
test_evening <- subset(filter_evening, filter_evening$Year == 2009)
# Aggregate the data
test_morning <- aggregate(list(Global_active_power = test_morning$Global_active_power, Global_intensity = test_morning$Global_intensity), by=list(Week = test_morning$Week, Day = test_morning$Day), mean)
test_evening <- aggregate(list(Global_active_power = test_evening$Global_active_power, Global_intensity = test_evening$Global_intensity), by=list(Week = test_evening$Week, Day = test_evening$Day), mean)


# ========================================================= Created HMM Model for TRAINING Data set ========================================================= #

# ==================================== ======= Train Morning Training ==================================== ======= #

model_morning_0 <- depmix(list(Global_active_power ~ 1, Global_intensity ~ 1), data = training_morning,
                 nstates = 4, family=list(gaussian(), multinomial("identity")), ntimes = nrow(training_morning))
fm_morning_0 <- fit(model_morning_0)
print(fm_morning_0)

model_morning_1 <- depmix(list(Global_active_power ~ 1, Global_intensity ~ 1), data = training_morning,
                 nstates = 6, family=list(gaussian(), multinomial("identity")), ntimes = nrow(training_morning))
fm_morning_1 <- fit(model_morning_1)
print(fm_morning_1)

model_morning_2 <- depmix(list(Global_active_power ~ 1, Global_intensity ~ 1), data = training_morning,
                 nstates = 8, family=list(gaussian(), multinomial("identity")), ntimes = nrow(training_morning))
fm_morning_2 <- fit(model_morning_2)
print(fm_morning_1)

model_morning_3 <- depmix(list(Global_active_power ~ 1, Global_intensity ~ 1), data = training_morning,
                 nstates = 10, family=list(gaussian(), multinomial("identity")), ntimes = nrow(training_morning))
fm_morning_3 <- fit(model_morning_3)
print(fm_morning_3)

model_morning_4 <- depmix(list(Global_active_power ~ 1, Global_intensity ~ 1), data = training_morning,
                 nstates = 12, family=list(gaussian(), multinomial("identity")), ntimes = nrow(training_morning))
fm_morning_4 <- fit(model_morning_4)
print(fm_morning_4)

model_morning_5 <- depmix(list(Global_active_power ~ 1, Global_intensity ~ 1), data = training_morning,
                 nstates = 14, family=list(gaussian(), multinomial("identity")), ntimes = nrow(training_morning))
fm_morning_5 <- fit(model_morning_5)
print(fm_morning_5)

model_morning_6 <- depmix(list(Global_active_power ~ 1, Global_intensity ~ 1), data = training_morning,
                 nstates = 16, family=list(gaussian(), multinomial("identity")), ntimes = nrow(training_morning))
fm_morning_6 <- fit(model_morning_6)
print(fm_morning_6)

plot(c(4,6,8,10,12,14,16),c(BIC(fm_morning_0),BIC(fm_morning_1),BIC(fm_morning_2),BIC(fm_morning_3), BIC(fm_morning_4), BIC(fm_morning_5), BIC(fm_morning_6)),ty="b")


# ==================================== ======= Train Evening for Training data ==================================== ======= #
# State = 4, is the best
model_evening_0 <- depmix(list(Global_active_power ~ 1, Global_intensity ~ 1), data = training_evening,
                 nstates = 4, family=list(gaussian(), multinomial("identity")), ntimes = nrow(training_evening))
fm_evening_0 <- fit(model_evening_0)
print(fm_evening_0)

model_evening_1 <- depmix(list(Global_active_power ~ 1, Global_intensity ~ 1), data = training_evening,
                 nstates = 6, family=list(gaussian(), multinomial("identity")), ntimes = nrow(training_evening))
fm_evening_1 <- fit(model_evening_1)
print(fm_evening_1)

model_evening_2 <- depmix(list(Global_active_power ~ 1, Global_intensity ~ 1), data = training_evening,
                 nstates = 8, family=list(gaussian(), multinomial("identity")), ntimes = nrow(training_evening))
fm_evening_2 <- fit(model_evening_2)
print(fm_evening_2)

model_evening_3 <- depmix(list(Global_active_power ~ 1, Global_intensity ~ 1), data = training_evening,
                 nstates = 10, family=list(gaussian(), multinomial("identity")), ntimes = nrow(training_evening))
fm_evening_3 <- fit(model_evening_3)
print(fm_evening_3)

model_evening_4 <- depmix(list(Global_active_power ~ 1, Global_intensity ~ 1), data = training_evening,
                 nstates = 12, family=list(gaussian(), multinomial("identity")), ntimes = nrow(training_evening))
fm_evening_4 <- fit(model_evening_4)
print(fm_evening_4)

model_evening_5 <- depmix(list(Global_active_power ~ 1, Global_intensity ~ 1), data = training_evening,
                 nstates = 14, family=list(gaussian(), multinomial("identity")), ntimes = nrow(training_evening))
fm_evening_5 <- fit(model_evening_5)
print(fm_evening_5)

model_evening_6 <- depmix(list(Global_active_power ~ 1, Global_intensity ~ 1), data = training_evening,
                 nstates = 16, family=list(gaussian(), multinomial("identity")), ntimes = nrow(training_evening))
fm_evening_6 <- fit(model_evening_6)
print(fm_evening_6)

plot(c(4,6,8,10,12,14,16) ,c(BIC(fm_evening_0),BIC(fm_evening_1),BIC(fm_evening_2),BIC(fm_evening_3), BIC(fm_evening_4), BIC(fm_evening_5), BIC(fm_evening_6)),ty="b")


# ========================================================= Created HMM Model for TEST Data set ========================================================= #

# ======= Train Morning for TEST DATA ======= #

training_morning_for_test <- training_morning[1:288,]
model <- depmix(list(Global_active_power ~ 1, Global_intensity ~ 1), data = training_morning_for_test,
                nstates = 4, family=list(gaussian(), multinomial("identity")), ntimes = nrow(training_morning_for_test))
fm <- fit(model)
print(fm)

newModel <- depmix(list(Global_active_power ~ 1, Global_intensity ~ 1), data = test_morning,
                   nstates = 4, family=list(gaussian(), multinomial("identity")), ntimes = nrow(test_morning))

newModel <- setpars(newModel, getpars(model))
logLik(newModel)
BIC(newModel)




# Test_Data_with_Injected_Anomalies

# shrink the training data size to match testing so that parameter count matches for testing
training <- training[1:287,]
model <- depmix(list(Global_active_power ~ 1, Global_intensity ~ 1), data = training,
                nstates = 4, family=list(gaussian(), multinomial("identity")), ntimes = nrow(training))
fm <- fit(model)
print(fm)

newModel <- depmix(list(Global_active_power ~ 1, Global_intensity ~ 1), data = test_morning,
                   nstates = 4, family=list(gaussian(), multinomial("identity")), ntimes = nrow(test_morning))

newModel <- setpars(newModel, getpars(model))
logLik(newModel)
BIC(newModel)
