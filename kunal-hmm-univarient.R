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

# Univarient
Hmm_model <- function(data, parm_variable, number_of_states) {
  model <- depmix(response =parm_variable, data = data, 
                  nstates = number_of_states, ntimes = nrow(data))
  fm <- fit(model)
  normalize_loglikehood <- logLik(fm) / nrow(data)
  result <- list(model, fm, normalize_loglikehood  )
  return(result)
  
  # Multivarient
  #model <- depmix(parm_variable, data = data,
   #               nstates = number_of_states, family=param_family, ntimes = nrow(data))
  #fm <- fit(model)
}

Add_date <- function(data){
  data$Year <- as.numeric(format(as.Date(test1$Date, "%d/%m/%Y"), "%Y"))
  data$Day <- as.POSIXlt(data$Date)$wday
  x <- paste(data$Date, data$Time)
  dateTime <- as.POSIXlt(x, format = "%d/%m/%Y %H:%M:%S")
  
  # Derive week and filter for week 7
  data$Week <- strftime(dateTime, format = "%V")
  return(data)
}

Model_comparison <- function(training_model, test_model, numberOfRow){
  match_model <- getpars(training_model) 
  test_model_length <- length(getpars(test_model))
  #Match the new_model_morning size
  match_model <- match_model[1:test_model_length]  
  # Compare the test vs training model
  test <- setpars(test_model, match_model)
  # Calculate Normalize Log-likelihood and BIC for test
  normalize_test_loglike <- logLik(test) / numberOfRow
  bic <- BIC(test) 
  # Create a return Object
  result <- list(normalize_test_loglike, bic)
  return(result)
}


# Determine features for multivariate HMM analysis
# SUBPART 1 : Select Global Active Power (main feature) and Global Intensity as features - split train and test data
data <- read.csv(file="TrainData.txt", header=TRUE, sep=",", na.strings = c("", "NA"))

# Test Data
test1 <- read.csv(file="test1.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
test2 <- read.csv(file="test2.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
test3 <- read.csv(file="test3.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
test4 <- read.csv(file="test4.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
test5 <- read.csv(file="test5.txt", header=TRUE, sep=",", na.strings = c("", "NA"))

data <- na.omit(data)
test1 <- na.omit(test1)
test2 <- na.omit(test2)
test3 <- na.omit(test3)
test4 <- na.omit(test4)
test5 <- na.omit(test5)

# row_id <- rownames(data)
# data <- cbind(id=row_id,data)

# Result from part 1 indicate that GAP and Global Intensity are highly correlated
data$Year <- as.numeric(format(as.Date(data$Date, "%d/%m/%Y"), "%Y"))
data$Day <- as.POSIXlt(data$Date)$wday
x <- paste(data$Date, data$Time)
dateTime <- as.POSIXlt(x, format = "%d/%m/%Y %H:%M:%S")

# Derive week and filter for week 7
data$Week <- strftime(dateTime, format = "%V")

test1 <- Add_date(test1)
test2 <- Add_date(test2)
test3 <- Add_date(test3)
test4 <- Add_date(test4)
test5 <- Add_date(test5)


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
training_morning <- aggregate(list(Global_active_power = training_morning$Global_active_power), by=list(Week = training_morning$Week, Day = training_morning$Day), mean)
training_evening <- aggregate(list(Global_active_power = training_evening$Global_active_power), by=list(Week = training_evening$Week, Day = training_evening$Day), mean)


# ========================================================= Created TEST Data set ========================================================= #
# Filtering data by morning and evening
test_morning <- subset(filter_morning, filter_morning$Year == 2009)
test_evening <- subset(filter_evening, filter_evening$Year == 2009)
# Aggregate the data
test_morning <- aggregate(list(Global_active_power = test_morning$Global_active_power), by=list(Week = test_morning$Week, Day = test_morning$Day), mean)
test_evening <- aggregate(list(Global_active_power = test_evening$Global_active_power), by=list(Week = test_evening$Week, Day = test_evening$Day), mean)

# ========================================================= Five TEST Sets ========================================================= #
# Filter the five given test data sets
# Filtering by morning and evening
# TEST SET #1
test1_morning <- subset(test1, (strptime(Time, format = "%H:%M:%S") >= strptime("05:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("09:30:00", format = "%H:%M:%S")))
test1_evening <- subset(test1, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))
# TEST SET #2
test2_morning <- subset(test2, (strptime(Time, format = "%H:%M:%S") >= strptime("05:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("09:30:00", format = "%H:%M:%S")))
test2_evening <- subset(test2, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))
# TEST SET #1
test3_morning <- subset(test3, (strptime(Time, format = "%H:%M:%S") >= strptime("05:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("09:30:00", format = "%H:%M:%S")))
test3_evening <- subset(test3, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))
# TEST SET #2
test4_morning <- subset(test4, (strptime(Time, format = "%H:%M:%S") >= strptime("05:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("09:30:00", format = "%H:%M:%S")))
test4_evening <- subset(test4, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))
# TEST SET #2
test5_morning <- subset(test5, (strptime(Time, format = "%H:%M:%S") >= strptime("05:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("09:30:00", format = "%H:%M:%S")))
test5_evening <- subset(test5, (strptime(Time, format = "%H:%M:%S") >= strptime("17:30:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") <= strptime("21:30:00", format = "%H:%M:%S")))

# Aggregate the data
# TEST SET #1

test1_morning <- aggregate(list(Global_active_power = test1_morning$Global_active_power), 
                           by=list(Week = test1_morning$Week, Day = test1_morning$Day), mean)
test1_evening <- aggregate(list=(Global_active_power = test1_evening$Global_active_power), 
                            by=list(Week = test1_evening$Week, Day = test1_evening$Day), mean)

# TEST SET #2
test2_morning <- aggregate(list(Global_active_power = test2_morning$Global_active_power), 
                           by=list(Week = test2_morning$Week, Day = test2_morning$Day), mean)
test2_evening <- aggregate(list(Global_active_power = test2_evening$Global_active_power), 
                           by=list(Week = test2_evening$Week, Day = test2_evening$Day), mean)
# TEST SET #3
test3_morning <- aggregate(list(Global_active_power = test3_morning$Global_active_power), 
                                by=list(Week = test3_morning$Week, Day = test3_morning$Day), mean)
test3_evening <- aggregate(list(Global_active_power = test3_evening$Global_active_power), 
                                by=list(Week = test3_evening$Week, Day = test3_evening$Day), mean)
# TEST SET #4
test4_morning <- aggregate(list(Global_active_power = test4_morning$Global_active_power),
                           by=list(Week = test4_morning$Week, Day = test4_morning$Day), mean)
test4_evening <- aggregate(list(Global_active_power = test4_evening$Global_active_power), 
                                by=list(Week = test4_evening$Week, Day = test4_evening$Day), mean)
# TEST SET #5
test5_morning <- aggregate(list(Global_active_power = test5_morning$Global_active_power), 
                                by=list(Week = test5_morning$Week, Day = test5_morning$Day), mean)
test5_evening <- aggregate(list(Global_active_power = test5_evening$Global_active_power), 
                                by=list(Week = test5_evening$Week, Day = test5_evening$Day), mean)


# ========================================================= Created HMM Model for TRAINING Data set ========================================================= #

# Training Morning Model
model_morning_0 <- Hmm_model(training_morning, Global_active_power ~ 1, 4) 

model_morning_1 <- Hmm_model(training_morning, Global_active_power ~ 1, 6)

model_morning_2 <- Hmm_model(training_morning, Global_active_power ~ 1, 8)

model_morning_3 <- Hmm_model(training_morning, Global_active_power ~ 1, 10)

model_morning_4 <- Hmm_model(training_morning, Global_active_power ~ 1, 12)

model_morning_5 <- Hmm_model(training_morning, Global_active_power ~ 1, 14)

model_morning_6 <- Hmm_model(training_morning, Global_active_power ~ 1, 16)

# Plot the BIC vs Number of states graph, 
# When fitting models, it is possible to increase the likelihood by adding parameters, but doing so may result in 
# overfitting. Both AIC and BIC attempt to resolve this problem
# The model with lowest BIC is choosen
plot(c(4,6,8,10,12,14,16),c(BIC(model_morning_0[[2]]),BIC(model_morning_1[[2]]),BIC(model_morning_2[[2]]),BIC(model_morning_3[[2]]), BIC(model_morning_4[[2]]), BIC(model_morning_5[[2]]), BIC(model_morning_6[[2]])),
     ty="b", main="Number of States v/s BIC (Morning)",
     xlab="Number of states", ylab="BIC")

# Train Evening for Training data
model_evening_0 <- Hmm_model(training_evening, Global_active_power ~ 1, 4)

model_evening_1 <- Hmm_model(training_evening, Global_active_power ~ 1, 6)

model_evening_2 <- Hmm_model(training_evening, Global_active_power ~ 1, 8)

model_evening_3 <- Hmm_model(training_evening, Global_active_power ~ 1, 10)

model_evening_4 <- Hmm_model(training_evening, Global_active_power ~ 1, 12)

model_evening_5 <- Hmm_model(training_evening, Global_active_power ~ 1, 14)

model_evening_6 <- Hmm_model(training_evening, Global_active_power ~ 1, 16)

# Plot the BIC vs Number of states graph, 
# When fitting models, it is possible to increase the likelihood by adding parameters, but doing so may result in 
# overfitting. Both AIC and BIC attempt to resolve this problem
# The model with lowest BIC is choosen
plot(c(4,6,8,10,12,14,16),c(BIC(model_evening_0[[2]]),BIC(model_evening_1[[2]]),BIC(model_evening_2[[2]]),BIC(model_evening_3[[2]]), BIC(model_evening_4[[2]]), BIC(model_evening_5[[2]]), BIC(model_evening_6[[2]])),
     ty="b", main="Number of States v/s BIC (Evening)", 
     xlab="Number of states", ylab="BIC")

# ========================================================= Created HMM Model for TEST Data set ========================================================= #

# HMM TEST Morning
test_model_morning <- Hmm_model(test_morning, Global_active_power ~ 1, 4)
test_model_evening <- Hmm_model(test_evening, Global_active_power ~ 1, 4)

test_morning_result <- Model_comparison(model_morning_0[[1]], test_model_morning[[1]], nrow(test_morning))
test_evening_result <- Model_comparison(model_evening_0[[1]], test_model_evening[[1]], nrow(test_evening))


# ========================================================= HMM models for 5 Data Sets  ========================================================= #

# Train models for all test sets with states = 4

# Morning
test1_model_morning <- Hmm_model(test1_morning, Global_active_power ~ 1, 4)
test2_model_morning <- Hmm_model(test2_morning, Global_active_power ~ 1, 4)
test3_model_morning <- Hmm_model(test3_morning, Global_active_power ~ 1, 4)
test4_model_morning <- Hmm_model(test4_morning, Global_active_power ~ 1, 4)
test5_model_morning <- Hmm_model(test5_morning, Global_active_power ~ 1, 4)

# TEST-2 Evening
test1_model_evening <- Hmm_model(test1_evening, Global_active_power ~ 1, 4)
test2_model_evening <- Hmm_model(test2_evening, Global_active_power ~ 1, 4)
test3_model_evening <- Hmm_model(test3_evening, Global_active_power ~ 1, 4)
test4_model_evening <- Hmm_model(test4_evening, Global_active_power ~ 1, 4)
test5_model_evening <- Hmm_model(test5_evening, Global_active_power ~ 1, 4)


# Model Comparison 
# Morning
test1_morning_result <- Model_comparison(model_morning_0[[1]], test1_model_morning[[1]], nrow(test1_morning))
test2_morning_result <- Model_comparison(model_morning_0[[1]], test2_model_morning[[1]], nrow(test2_morning))
test3_morning_result <- Model_comparison(model_morning_0[[1]], test3_model_morning[[1]], nrow(test3_morning))
test4_morning_result <- Model_comparison(model_morning_0[[1]], test4_model_morning[[1]], nrow(test4_morning))
test5_morning_result <- Model_comparison(model_morning_0[[1]], test5_model_morning[[1]], nrow(test5_morning))
# Evening
test1_evening_result <- Model_comparison(model_evening_0[[1]], test1_model_evening[[1]], nrow(test1_evening))
test2_evening_result <- Model_comparison(model_evening_0[[1]], test2_model_evening[[1]], nrow(test2_evening))
test3_evening_result <- Model_comparison(model_evening_0[[1]], test3_model_evening[[1]], nrow(test3_evening))
test4_evening_result <- Model_comparison(model_evening_0[[1]], test4_model_evening[[1]], nrow(test4_evening))
test5_evening_result <- Model_comparison(model_evening_0[[1]], test5_model_evening[[1]], nrow(test5_evening))


plot( factor(c(1,2,3,4,5,6)), xaxt = "n",
      c(test1_morning_result[[1]], test2_morning_result[[1]], test3_morning_result[[1]], test4_morning_result[[1]], test5_morning_result[[1]], test_morning_result[[1]]),
      main="LogLikehood of Test v/s Train (Morning)", sub="Comparing Normalized Loglikehood values of Five Test datasets, vs Training Model",
      xlab="Test cases Model", ylab="Normalized LogLikehood")
axis(1, at=1:6, labels=c("Test-1", "Test-2", "Test-3", "Test-4", "Test-5", "Training"))


# Evening
plot( factor(c(1,2,3,4,5,6)), xaxt = "n",
      c(test1_evening_result[[1]], test2_evening_result[[1]], test3_evening_result[[1]], test4_evening_result[[1]], test5_evening_result[[1]], test_evening_result[[1]]),
      main="LogLikehood of Test v/s Train (Evening)", sub="Comparing Normalized Loglikehood values of Five Test datasets, vs Training Model",
      xlab="Test cases Model", ylab="Normalized LogLikehood")
axis(1, at=1:6, labels=c("Test-1", "Test-2", "Test-3", "Test-4", "Test-5", "Training"))


