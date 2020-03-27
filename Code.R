trainData <- read.csv(file="TrainData.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
test1 <- read.csv(file="test1.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
test2 <- read.csv(file="test2.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
test3 <- read.csv(file="test3.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
test4 <- read.csv(file="test4.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
test5 <- read.csv(file="test5.txt", header=TRUE, sep=",", na.strings = c("", "NA"))

trainDF <- subset(trainData, select = c(Global_active_power, Global_intensity))
test1DF <- subset(test1, select = c(Global_active_power, Global_intensity))
test2DF <- subset(test2, select = c(Global_active_power, Global_intensity))
test3DF <- subset(test3, select = c(Global_active_power, Global_intensity))
test4DF <- subset(test4, select = c(Global_active_power, Global_intensity))
test5DF <- subset(test5, select = c(Global_active_power, Global_intensity))

trainDF <- na.omit(trainDF)
test1DF <- na.omit(test1DF)
test2DF <- na.omit(test2DF)
test3DF <- na.omit(test3DF)
test4DF <- na.omit(test4DF)
test5DF <- na.omit(test5DF)

trainMean <- apply(trainDF, 2, mean)
trainSD <- apply(trainDF, 2, sd)

test1Mean <- apply(test1DF, 2, mean)
test1SD <- apply(test1DF, 2, sd)

test2Mean <- apply(test2DF, 2, mean)
test2SD <- apply(test2DF, 2, sd)

test3Mean <- apply(test3DF, 2, mean)
test3SD <- apply(test3DF, 2, sd)

test4Mean <- apply(test4DF, 2, mean)
test4SD <- apply(test4DF, 2, sd)

test5Mean <- apply(test5DF, 2, mean)
test5SD <- apply(test5DF, 2, sd)

df <- data.frame(GAP_mean = c(trainMean[1], test1Mean[1], test2Mean[1], test3Mean[1], test4Mean[1], test5Mean[1]),
                    GAP_sd = c(trainSD[1], test1SD[1], test2SD[1], test3SD[1], test4SD[1], test5SD[1]),
                    GI_mean = c(trainMean[2], test1Mean[2], test2Mean[2], test3Mean[2], test4Mean[2], test5Mean[2]),
                    GI_sd = c(trainSD[2], test1SD[2], test2SD[2], test3SD[2], test4SD[2], test5SD[2]))
rownames(df)[1] = "Training"
rownames(df)[2] = "Test 1"
rownames(df)[3] = "Test 2"
rownames(df)[4] = "Test 3"
rownames(df)[5] = "Test 4"
rownames(df)[6] = "Test 5"

trainGAPMax <- max(trainDF$Global_active_power)
trainGIMax <- max(trainDF$Global_intensity)
trainGAPMin <- min(trainDF$Global_active_power)
trainGIMin <- min(trainDF$Global_intensity)

test1GAPMax <- max(test1DF$Global_active_power)
test1GIMax <- max(test1DF$Global_intensity)
test1GAPMin <- min(test1DF$Global_active_power)
test1GIMin <- min(test1DF$Global_intensity)

test2GAPMax <- max(test2DF$Global_active_power)
test2GIMax <- max(test2DF$Global_intensity)
test2GAPMin <- min(test2DF$Global_active_power)
test2GIMin <- min(test2DF$Global_intensity)

test3GAPMax <- max(test3DF$Global_active_power)
test3GIMax <- max(test3DF$Global_intensity)
test3GAPMin <- min(test3DF$Global_active_power)
test3GIMin <- min(test3DF$Global_intensity)

test4GAPMax <- max(test4DF$Global_active_power)
test4GIMax <- max(test4DF$Global_intensity)
test4GAPMin <- min(test4DF$Global_active_power)
test4GIMin <- min(test4DF$Global_intensity)

test5GAPMax <- max(test5DF$Global_active_power)
test5GIMax <- max(test5DF$Global_intensity)
test5GAPMin <- min(test5DF$Global_active_power)
test5GIMin <- min(test5DF$Global_intensity)

dfMinMax <- data.frame(
  GAP_Min = c(trainGAPMin, test1GAPMin, test2GAPMin, test3GAPMin, test4GAPMin, test5GAPMin),
  GAP_Max = c(trainGAPMax, test1GAPMax, test2GAPMax, test3GAPMax, test4GAPMax, test5GAPMax),
  GI_Min = c(trainGIMin, test1GIMin, test2GIMin, test3GIMin, test4GIMin, test5GIMin),
  GI_Max = c(trainGIMax, test1GIMax, test2GIMax, test3GIMax, test4GIMax, test5GIMax)
)

rownames(dfMinMax)[1] = "Training"
rownames(dfMinMax)[2] = "Test 1"
rownames(dfMinMax)[3] = "Test 2"
rownames(dfMinMax)[4] = "Test 3"
rownames(dfMinMax)[5] = "Test 4"
rownames(dfMinMax)[6] = "Test 5"
