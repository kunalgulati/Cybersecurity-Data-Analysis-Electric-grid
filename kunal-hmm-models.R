library("depmixS4")
library("psych")
library("ggplot2")
library("corrplot")
library("gridExtra")
library("fpp2")

# Returns object 
# Object[1] = Model
# Object[2] = fm
# Object[3] = normalize_loglikehood
Hmm_model <- function(data, parm_variable, param_family, number_of_states) {
  model <- depmix(parm_variable, data = data,
                            nstates = number_of_states, family=param_family, ntimes = nrow(data))
  fm <- fit(model)
  normalize_loglikehood <- logLik(fm) / nrow(data)
  result <- list(model, fm, normalize_loglikehood  )
  return(result)
}



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

# Training Morning Model
model_morning_0 <- Hmm_model(training_morning, list(Global_active_power ~ 1, Global_intensity ~ 1),
                             list(gaussian(), multinomial("identity")), 4) 

model_morning_1 <- Hmm_model(training_morning, list(Global_active_power ~ 1, Global_intensity ~ 1),
                          list(gaussian(), multinomial("identity")), 6)

model_morning_2 <- Hmm_model(training_morning, list(Global_active_power ~ 1, Global_intensity ~ 1),
                          list(gaussian(), multinomial("identity")), 8)

model_morning_3 <- Hmm_model(training_morning, list(Global_active_power ~ 1, Global_intensity ~ 1),
                          list(gaussian(), multinomial("identity")), 10)

model_morning_4 <- Hmm_model(training_morning, list(Global_active_power ~ 1, Global_intensity ~ 1),
                          list(gaussian(), multinomial("identity")), 12)

model_morning_5 <- Hmm_model(training_morning, list(Global_active_power ~ 1, Global_intensity ~ 1),
                          list(gaussian(), multinomial("identity")), 14)

model_morning_6 <- Hmm_model(training_morning, list(Global_active_power ~ 1, Global_intensity ~ 1),
                          list(gaussian(), multinomial("identity")), 16)

# Plot the BIC vs Number of states graph, 
# to find the best model
plot(c(4,6,8,10,12,14,16),c(BIC(model_morning_0[[2]]),BIC(model_morning_1[[2]]),BIC(model_morning_2[[2]]),
                            BIC(model_morning_3[[2]]), BIC(model_morning_4[[2]]), BIC(model_morning_5[[2]]), BIC(model_morning_6[[2]])),ty="b")


# Train Evening for Training data
model_evening_0 <- Hmm_model(training_evening, list(Global_active_power ~ 1, Global_intensity ~ 1),
                             list(gaussian(), multinomial("identity")), 4)

model_evening_1 <- Hmm_model(training_evening, list(Global_active_power ~ 1, Global_intensity ~ 1),
                             list(gaussian(), multinomial("identity")), 6)

model_evening_2 <- Hmm_model(training_evening, list(Global_active_power ~ 1, Global_intensity ~ 1),
                             list(gaussian(), multinomial("identity")), 8)

model_evening_3 <- Hmm_model(training_evening, list(Global_active_power ~ 1, Global_intensity ~ 1),
                            list(gaussian(), multinomial("identity")), 10)

model_evening_4 <- Hmm_model(training_evening, list(Global_active_power ~ 1, Global_intensity ~ 1),
                             list(gaussian(), multinomial("identity")), 12)

model_evening_5 <- Hmm_model(training_evening, list(Global_active_power ~ 1, Global_intensity ~ 1),
                             list(gaussian(), multinomial("identity")), 14)

model_evening_6 <- Hmm_model(training_evening, list(Global_active_power ~ 1, Global_intensity ~ 1),
                             list(gaussian(), multinomial("identity")), 16)

# Plot the BIC vs Number of states graph, 
# to find the best model
plot(c(4,6,8,10,12,14,16),c(BIC(model_evening_0[[2]]),BIC(model_evening_1[[2]]),BIC(model_evening_2[[2]]),
                            BIC(model_evening_3[[2]]), BIC(model_evening_4[[2]]), BIC(model_evening_5[[2]]), BIC(model_evening_6[[2]])),ty="b")

# ========================================================= Created HMM Model for TEST Data set ========================================================= #

test_model_morning <- Hmm_model(test_morning, list(Global_active_power ~ 1, Global_intensity ~ 1),
                                list(gaussian(), multinomial("identity")), 4)

model_match_morning <- getpars(model_morning_0[[1]]) # best model with states = 4
model_match_morning <- model_match_morning[1:1164]  #Match the new_model_morning size
# SetPars 
test_morning_setpars <- setpars(test_model_morning[[1]], model_match_morning)
normalize_test_morning_loglike <- logLik(test_morning_setpars) / nrow(test_morning)
BIC(test_morning_setpars)







