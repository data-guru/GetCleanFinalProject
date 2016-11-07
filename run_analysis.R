packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)
# Define column widths for data set reads
widths <- c(17, (rep.int(16, 560)))
# Read test and train data sets and labels
testSubject <- fread("test/subject_test.txt")
trainSubject <- fread("train/subject_train.txt")
testSet <- read.fwf("test/X_test.txt", widths)
trainSet <- read.fwf("train/x_train.txt", widths)
testLabels <- read.csv("test/y_test.txt", header = FALSE)
trainLabels <- read.csv("train/y_train.txt", header = FALSE)
# Concatenate datatables
fullSubject <- rbind(testSubject, trainSubject)
setnames(fullSubject, "V1", "subject")
fullActivity <- rbind(testLabels, trainLabels)
setnames(fullActivity, "V1", "activityNum")
fullSet <- rbind(testSet, trainSet)
# Merge columns
fullSubject <- cbind(fullSubject, fullActivity)
fullSet <- cbind(fullSubject, fullSet)
# Set key
setkey(fullSet, subject, activityNum)
# Read features.txt
features <- fread("features.txt")
setnames(features, names(features), c("featureNum", "featureName"))
# Just keep the mean and std deviation vars
features <- features[grepl("mean\\(\\)|std\\(\\)", featureName)]
features$featureCode <- features[, paste0("V", featureNum)]
select <- c(key(fullSet), features$featureCode)
fullSet <- fullSet[, select, with=FALSE]
# Read from the activity_labels.txt file and use those
activityNames <- fread("activity_labels.txt")
setnames(activityNames, names(activityNames), c("activityNum", "activityName"))
# Merge labels and data set
fullSet <- merge(fullSet, activityNames, by="activityNum", all.x=TRUE)
# Add the new key
setkey(fullSet, subject, activityNum, activityName)
# melt and reshape to a tall and narrow format
fullSet <- data.table(melt(fullSet, key(fullSet), variable.name="featureCode"))
# Merge the activity names
fullSet <- merge(fullSet, features[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)
# create new variables for activty and feature
fullSet$activity <- factor(fullSet$activityName)
fullSet$feature <- factor(fullSet$featureName)
# Use a function grepper to separate features from featureName
grepper <- function (regex) {
    grepl(regex, fullSet$feature)
}
# Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepper("^t"), grepper("^f")), ncol=nrow(y))
fullSet$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grepper("Acc"), grepper("Gyro")), ncol=nrow(y))
fullSet$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepper("BodyAcc"), grepper("GravityAcc")), ncol=nrow(y))
fullSet$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepper("mean()"), grepper("std()")), ncol=nrow(y))
fullSet$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))
## Features with 1 category
fullSet$featJerk <- factor(grepper("Jerk"), labels=c(NA, "Jerk"))
fullSet$featMagnitude <- factor(grepper("Mag"), labels=c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepper("-X"), grepper("-Y"), grepper("-Z")), ncol=nrow(y))
fullSet$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))
# Make sure all possible combinations of feature are accounted for
r1 <- nrow(fullSet[, .N, by=c("feature")])
r2 <- nrow(fullSet[, .N, by=c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
# Build the tidy data set
setkey(fullSet, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- fullSet[, list(count = .N, average = mean(value)), by=key(fullSet)]
# Done!
write.table(dtTidy, file="ProjectSubmission.txt", row.names = FALSE)